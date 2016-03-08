(* lab3/kgen.ml *)

open Tree
open Dict
open Keiko
open Print

let optflag = ref false

let level = ref 0

let slink = 12

(* |get_slink_rec| -- helper for get_slink *)
let rec get_slink_rec lvl d =
  if lvl = d.d_level - 1 then SEQ []
  else SEQ [CONST slink; BINOP PlusA; LOADW; get_slink_rec (lvl - 1) d]

(* |get_slink| -- calculate static link *)
let get_slink d =
  SEQ [LOCAL 0; get_slink_rec (!level) d]

(* |gen_addr_rec| -- helper for gen_addr *)
let rec gen_addr_rec lvl d=
  if lvl  = d.d_level then
    CONST d.d_off
  else
    SEQ [CONST slink; BINOP PlusA; LOADW; gen_addr_rec (lvl - 1) d]

(* |gen_addr| -- generate code to push address of a variable *)
let gen_addr d =
  if d.d_level = 0 || d.d_off = 0 then
    GLOBAL d.d_lab
  else
    SEQ [LOCAL 0; gen_addr_rec (!level) d; BINOP PlusA]

(* |gen_expr| -- generate code for an expression *)
let rec gen_expr =
  function
      Variable x ->
        let d = get_def x in
        begin
          match d.d_kind with
              VarDef ->
                SEQ [LINE x.x_line; gen_addr d; LOADW]
            | ProcDef nargs ->
                failwith "no procedure values"
        end
    | Number x ->
        CONST x
    | Monop (w, e1) ->
        SEQ [gen_expr e1; MONOP w]
    | Binop (w, e1, e2) ->
        SEQ [gen_expr e1; gen_expr e2; BINOP w]
    | Call (p, args) ->
        SEQ [LINE p.x_line; SEQ (List.map gen_expr (List.rev args));
          get_slink (get_def p); (* static link *)
          gen_addr (get_def p);
          PCALLW (List.length args)
        ]

(* |gen_cond| -- generate code for short-circuit condition *)
let rec gen_cond tlab flab e =
  (* Jump to |tlab| if |e| is true and |flab| if it is false *)
  match e with
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
    | Seq ss ->
        SEQ (List.map gen_stmt ss)
    | Assign (v, e) ->
        let d = get_def v in
        begin
          match d.d_kind with
              VarDef ->
                SEQ [gen_expr e; gen_addr d; STOREW]
           | _ -> failwith "assign"
        end
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
    | Return e ->
        SEQ [gen_expr e; RETURNW]

(* |gen_proc| -- generate code for a procedure *)
let rec gen_proc (Proc (p, formals, Block (vars, procs, body))) =
  let d = get_def p in
  level := d.d_level;
  let code = gen_stmt body in
  printf "PROC $ $ 0 0\n" [fStr d.d_lab; fNum (4 * List.length vars)];
  Keiko.output (if !optflag then Peepopt.optimise code else code);
  printf "ERROR E_RETURN 0\n" [];
  printf "END\n\n" [];
  List.iter gen_proc procs

(* |translate| -- generate code for the whole program *)
let translate (Program (Block (vars, procs, body))) =
  level := 0;
  printf "PROC MAIN 0 0 0\n" [];
  Keiko.output (gen_stmt body);
  printf "RETURN\n" [];
  printf "END\n\n" [];
  List.iter gen_proc procs;
  List.iter (function x -> printf "GLOVAR _$ 4\n" [fStr x]) vars
