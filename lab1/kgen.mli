(* lab1/kgen.mli *)

(* translate -- generate intermediate code *)
val translate : Tree.program -> unit

(* optflag -- flag to control optimisation *)
val optflag : bool ref
