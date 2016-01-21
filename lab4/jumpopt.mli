(* ppcpi/jumpopt.mli *)

(* The jump optimiser looks for multiple labels on the same tree,
   jumps that lead to jumps, and other simple patterns in control flow. *)

(* |optimise| -- clean up a forest *)
val optimise : Keiko.optree list -> Keiko.optree list
