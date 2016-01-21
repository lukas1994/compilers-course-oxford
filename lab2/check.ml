(* lab2/check.ml *)

open Print 
open Keiko
open Tree 
open Dict 

(* |err_line| -- line number for error messages *)
let err_line = ref 1

(* |Semantic_error| -- exception raised if error detected *)
exception Semantic_error of string * Print.arg list * int

(* |sem_error| -- issue error message by raising exception *)
let sem_error fmt args = 
  raise (Semantic_error (fmt, args, !err_line))

(* |lookup_def| -- find definition of a name, give error is none *)
let lookup_def x env =
  err_line := x.x_line;
  try let d = lookup x.x_name env in x.x_def <- Some d; d.d_type with 
    Not_found -> sem_error "$ is not declared" [fStr x.x_name]

(* |add_def| -- add definition to env, give error if already declared *)
let add_def d env =
  try define d env with 
    Exit -> sem_error "$ is already declared" [fStr d.d_tag]

(* |type_error| -- report a type error.  The message could be better. *)
let type_error () = sem_error "type mismatch in expression" []

(* |check_monop| -- check a unary operator and return its type *)
let check_monop w t =
  match w with
      Uminus ->
        if t <> Integer then type_error ();
        Integer
    | Not ->
        if t <> Boolean then type_error ();
        Boolean
    | _ -> failwith "bad monop"

(* |check_binop| -- check a binary operator and return its type *)
let check_binop w ta tb =
  match w with
      Plus | Minus | Times | Div | Mod ->
        if ta <> Integer || tb <> Integer then type_error ();
        Integer
    | Eq | Lt | Gt | Leq | Geq | Neq ->
        if ta <> tb then type_error ();
        Boolean
    | And | Or ->
        if ta <> Boolean || tb <> Boolean then type_error ();
        Boolean
    | _ -> failwith "bad binop"

(* |check_expr| -- check and annotate an expression *)
let rec check_expr env e =
  let t = expr_type env e in
  (e.e_type <- t; t)

(* |expr_type| -- check an expression and return its type *)
and expr_type env e = 
  match e.e_guts with
      Variable x -> 
        lookup_def x env
    | Sub (e1, e2) ->
        failwith "subscripts not implemented"
    | Number n -> Integer
    | Monop (w, e1) -> 
        let t = check_expr env e1 in
        check_monop w t
    | Binop (w, e1, e2) -> 
        let ta = check_expr env e1
        and tb = check_expr env e2 in
        check_binop w ta tb

(* |check_stmt| -- check and annotate a statement *)
let rec check_stmt env =
  function
      Skip -> ()
    | Seq ss ->
        List.iter (check_stmt env) ss
    | Assign (lhs, rhs) ->
        let ta = check_expr env lhs
        and tb = check_expr env rhs in
        if ta <> tb then sem_error "type mismatch in assignment" []
    | Print e ->
        let t = check_expr env e in
        if t <> Integer then sem_error "print needs an integer" []
    | Newline ->
        ()
    | IfStmt (cond, thenpt, elsept) ->
        let t = check_expr env cond in
        if t <> Boolean then
          sem_error "boolean needed in if statement" [];
        check_stmt env thenpt; 
        check_stmt env elsept
    | WhileStmt (cond, body) ->
        let t = check_expr env cond in
        if t <> Boolean then
          sem_error "need boolean after while" [];
        check_stmt env body

(* |make_def| -- construct definition of variable *)
let make_def x t a = { d_tag = x; d_type = t; d_lab = a }

(* |check_decl| -- check declaration and return extended environment *)
let check_decl env0 (Decl (vs, t)) =
  let declare env x = 
    let lab = sprintf "_$" [fStr x.x_name] in
    let d = make_def x.x_name t lab in
    x.x_def <- Some d; add_def d env in
  List.fold_left declare env0 vs

(* |check_decls| -- check a sequence of declarations *)
let check_decls = List.fold_left check_decl

(* |annotate| -- check and annotate a program *)
let annotate (Program (ds, ss)) =
  let env = check_decls init_env ds in
  check_stmt env ss


