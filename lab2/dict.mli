(* lab2/dict.mli *)

type ident = string

type ptype =
    Integer
  | Boolean
  | Array of int * ptype
  | Void

(* |type_size| -- calculate the size of a type *)
val type_size : ptype -> int

(* |is_array| -- returns true iff the given type is an array *)
val is_array : ptype -> bool

(* |base_type| -- returns the base type of an array; throws an exception if the given type is not an array*)
val base_type : ptype -> ptype

(* |def| -- definitions in environment *)
type def =
  { d_tag: ident;               (* Name *)
    d_type: ptype;              (* Type *)
    d_lab: string }             (* Global label *)

type environment

(* |define| -- add a definition, raise Exit if already declared *)
val define : def -> environment -> environment

(* |lookup| -- search an environment or raise Not_found *)
val lookup : ident -> environment -> def

(* |init_env| -- initial empty environment *)
val init_env : environment
