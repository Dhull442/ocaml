open Types;;

(* the secd machine *)
val secdmc: answer list -> (bytes * answer) list -> opcode list -> dump list -> answer

(* the compiler *)
val compile: expr -> opcode list
