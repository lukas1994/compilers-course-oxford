(* lab1/tree.mli *)

type ident = string

(* |name| -- type for applied occurrences, with annotations *)
type name =
  { x_name: ident;              (* Name of the reference *)
    x_lab: string;              (* Global label *)
    x_line: int }               (* Line number *)

val make_name : ident -> int -> name


(* Abstract syntax *)

type program = Program of stmt

and stmt =
    Skip
  | Seq of stmt list
  | Assign of name * expr
  | Print of expr
  | Newline
  | IfStmt of expr * stmt * stmt
  | WhileStmt of expr * stmt
  | RepeatStmt of stmt * expr
  | LoopStmt of stmt
  | Exit
  | CaseStmt of expr * (int list * stmt) list * stmt

and expr =
    Number of int
  | Variable of name
  | Monop of Keiko.op * expr
  | Binop of Keiko.op * expr * expr


(* seq -- neatly join a list of statements into a sequence *)
val seq : stmt list -> stmt

val print_tree : out_channel -> program -> unit
