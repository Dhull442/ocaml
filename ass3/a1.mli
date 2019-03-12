open A0
(* code goes here *)
type answer = Num of bigint | Bool of bool | Tup of int * ( answer list );;
(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list);;

type exptree =
   N of int (* Integer constant *)
 | B of bool (* Boolean constant *)
 | Var of string (* variable *)
 | Conjunction of exptree * exptree (* binary operators on booleans /\ *)
 | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
 | Not of exptree
 | Equals of exptree * exptree      (* comparison operations on integers *)
 | GreaterTE of exptree * exptree   (* comparison operations on integers *)
 | LessTE of exptree * exptree      (* comparison operations on integers *)
 | GreaterT of exptree * exptree    (* comparison operations on integers *)
 | LessT of exptree * exptree       (* comparison operations on integers *)
 | InParen of exptree               (* expressions using parenthesis *)
 | IfThenElse of exptree * exptree * exptree (* a conditional expression *)
 | Tuple of int * ( exptree list )         (* creating n-tuples ( n >= 0 ) *)
 | Project of ( int*int ) * exptree          (* projecting the i-th component of an expression ( which evaluates to an n-tuple, and 1 <= i <= n ) *)
 | Plus of exptree * exptree        (* binary operators on integers *)
 | Minus of exptree * exptree       (* binary operators on integers *)
 | Mult of exptree * exptree        (* binary operators on integers *)
 | Div of exptree * exptree         (* binary operators on integers *)
 | Rem of exptree * exptree         (* binary operators on integers *)
 | Negative of exptree       (* unary operators on booleans *)
 | Abs of exptree;;        (* unary operators on integers *)

type opcode = VAR of string | NCONST of bigint | PLUS | MULT | MINUS | DIV | REM | ABS | UNARYMINUS
   | EQS | GTE | LTE | GT | LT | PAREN
   | BCONST of bool | CONJ | DISJ | NOT
   | IFTE | TUPLE of int | PROJ of int*int;;
(* the definitional interpreter *)
val eval : exptree -> (string -> value) -> value

(* the stack machine *)
val stackmc: (answer list) -> (string -> answer) -> (opcode list) -> answer
(* the compiler *)
val compile: exptree -> opcode list
