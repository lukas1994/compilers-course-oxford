(* lab1/kgen.ml *)

open Tree
open Keiko
open Print

let optflag = ref false

(* |gen_expr| -- generate code for an expression *)
let rec gen_expr =
  function
      Variable x ->
        SEQ [LINE x.x_line; LDGW x.x_lab]
    | Number x ->
        CONST x
    | Monop (w, e1) ->
        SEQ [gen_expr e1; MONOP w]
    | Binop (w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP w]

(* |gen_cond| -- generate code for short-circuit condition *)
let rec gen_cond tlab flab e =
  (* Jump to |tlab| if |e| is true and |flab| if it is false *)
  match e with
      Number x ->
        if x <> 0 then JUMP tlab else JUMP flab
    | Binop ((Eq|Neq|Lt|Gt|Leq|Geq) as w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; JUMPC (w, tlab); JUMP flab]
    | Monop (Not, e1) ->
        gen_cond flab tlab e1
    | Binop (And, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond lab1 flab e1; LABEL lab1; gen_cond tlab flab e2]
    | Binop (Or, e1, e2) ->
        let lab1 = label () in
        SEQ [gen_cond tlab lab1 e1; LABEL lab1; gen_cond tlab flab e2]
    | _ ->
        SEQ [gen_expr e; CONST 0; JUMPC (Neq, tlab); JUMP flab]

(* |gen_stmt| -- generate code for a statement *)
let rec gen_stmt s exit_lab =
  match s with
      Skip -> NOP
    | Seq stmts -> SEQ (List.map (fun s -> gen_stmt s exit_lab) stmts)
    | Assign (v, e) ->
        SEQ [LINE v.x_line; gen_expr e; STGW v.x_lab]
    | Print e ->
        SEQ [gen_expr e; CONST 0; GLOBAL "Lib.Print"; PCALL 1]
    | Newline ->
        SEQ [CONST 0; GLOBAL "Lib.Newline"; PCALL 0]
    | IfStmt (test, thenpt, elsept) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [gen_cond lab1 lab2 test;
          LABEL lab1; gen_stmt thenpt exit_lab; JUMP lab3;
          LABEL lab2; gen_stmt elsept exit_lab; LABEL lab3]
    | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [JUMP lab2; LABEL lab1; gen_stmt body exit_lab;
          LABEL lab2; gen_cond lab1 lab3 test; LABEL lab3]
    | RepeatStmt (body, test) ->
        let lab1 = label () and lab2 = label () in
        SEQ [LABEL lab1; gen_stmt body exit_lab;
          gen_cond lab2 lab1 test; LABEL lab2]
    | LoopStmt body ->
        let lab = label () and exit_lab = label () in
        SEQ [LABEL lab; gen_stmt body exit_lab; JUMP lab; LABEL exit_lab]
    | Exit -> JUMP exit_lab
    | CaseStmt (switch, cases, default) ->
        let case_labs = List.map (fun _ -> label ()) cases and def_lab = label () and case_exit_lab = label () in
        let l = List.concat (List.map (fun ((nums, code), l) -> List.map (fun n -> (n, l)) nums) (List.combine cases case_labs)) in
        SEQ [gen_expr switch;
          CASEJUMP (List.length l); SEQ (List.map (fun (n, l) -> CASEARM (n, l)) l); JUMP def_lab;
          SEQ (List.map (fun ((_, code), l) ->
            SEQ [LABEL l; gen_stmt code exit_lab; JUMP case_exit_lab]
          ) (List.combine cases case_labs));
          LABEL def_lab; gen_stmt default exit_lab; LABEL case_exit_lab]

(* |translate| -- generate code for the whole program *)
let translate (Program ss) =
  let lab = label () in
  let code = gen_stmt ss lab in
  Keiko.output (if !optflag then Peepopt.optimise code else code)
