(* lab2/kgen.ml *)

open Dict
open Tree
open Keiko
open Print

let optflag = ref false

(* |line_number| -- find line number of variable reference *)
let rec line_number e =
  match e.e_guts with
      Variable x -> x.x_line
    | Sub (a, e) -> line_number a
    | _ -> 999

(* |gen_expr| -- generate code for an expression *)
let rec gen_expr e =
  match e.e_guts with
      Variable x ->
        let load_instr = if (get_def x).d_type == Boolean then LOADC else LOADW in
        SEQ [gen_addr e; load_instr]
    | Sub (a, _) ->
        let load_instr = if base_type a.e_type == Boolean then LOADC else LOADW in
        SEQ [gen_addr e; load_instr]
    | Number n ->
        CONST n
    | Monop (w, e1) ->
        SEQ [gen_expr e1; MONOP w]
    | Binop (w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP w]

(* |gen_addr| -- generate code to push address of a variable *)
and gen_addr v =
  match v.e_guts with
      Variable x ->
        let d = get_def x in
        SEQ [LINE x.x_line; GLOBAL d.d_lab]
    | Sub (a, e) ->
        (match a.e_type with
            Array (n, _) ->
              let ts = type_size (base_type a.e_type) in
              SEQ [gen_addr a; gen_expr e; CONST n; BOUND (line_number v); CONST ts; BINOP Times; BINOP PlusA]
          | _ -> failwith "gen_addr array")
    | _ ->
        failwith "gen_addr"

(* |gen_cond| -- generate code for short-circuit condition *)
let rec gen_cond tlab flab e =
  (* Jump to |tlab| if |e| is true and |flab| if it is false *)
  match e.e_guts with
      Number x ->
        if x <> 0 then JUMP tlab else JUMP flab
    | Binop ((Eq|Neq|Lt|Gt|Leq|Geq) as w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2;
          JUMPC (w, tlab); JUMP flab]
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
let rec gen_stmt =
  function
      Skip -> NOP
    | Seq stmts -> SEQ (List.map gen_stmt stmts)
    | Assign (v, e) ->
        let t = e.e_type in
        let t' = if is_array t then base_type t else t in
        let store_instr = if t' == Boolean then STOREW else STOREW in
        SEQ [LINE (line_number v); gen_expr e; gen_addr v; store_instr]
    | Print e ->
        SEQ [gen_expr e; CONST 0; GLOBAL "Lib.Print"; PCALL 1]
    | Newline ->
        SEQ [CONST 0; GLOBAL "Lib.Newline"; PCALL 0]
    | IfStmt (test, thenpt, elsept) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [gen_cond lab1 lab2 test;
          LABEL lab1; gen_stmt thenpt; JUMP lab3;
          LABEL lab2; gen_stmt elsept; LABEL lab3]
    | WhileStmt (test, body) ->
        let lab1 = label () and lab2 = label () and lab3 = label () in
        SEQ [JUMP lab2; LABEL lab1; gen_stmt body;
          LABEL lab2; gen_cond lab1 lab3 test; LABEL lab3]

let gen_decl (Decl (xs, t)) =
  List.iter (fun x ->
      let d = get_def x in
      let s = type_size d.d_type in
      printf "GLOVAR $ $\n" [fStr d.d_lab; fNum s]) xs

(* |translate| -- generate code for the whole program *)
let translate (Program (ds, ss)) =
  let code = gen_stmt ss in
  printf "PROC MAIN 0 0 0\n" [];
  Keiko.output (if !optflag then Peepopt.optimise code else code);
  printf "RETURN\n" [];
  printf "END\n\n" [];
  List.iter gen_decl ds
