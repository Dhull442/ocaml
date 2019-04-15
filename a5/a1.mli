open A0
(* The possible types of expressions in the language of expressions *)
(* type exptype = Tint | Tunit | Tbool | Tfunc of (exptype * exptype) *)

(* abstract syntax *)
type expr =
  V of string
  | Lambda of (expr * expr)
  | App of (expr * expr)
  | Plus of (expr * expr)
  | Minus of (expr * expr)
  | Mult of (expr * expr)
  | Div of (expr * expr)
  | Rem of (expr * expr)
  | And of (expr * expr)
  | Or of (expr * expr)
  | InParen of expr
  | Bool of bool
  | Not of expr
  | Integer of int
  | Cmp of expr    (* CMP is |sjdnfls| *)
  | Equals of expr * expr
  | GreaterT of expr * expr
  | GreaterTE of expr * expr
  | LessT of expr * expr
  | LessTE of expr * expr
  | If_Then_Else of (expr * expr * expr);;
(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
  | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF


(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

(* the definitional interpreter *)
val eval : exptree -> (string -> value) -> value
(* the stack machine *)
val stackmc: (answer list) -> (string -> answer) -> (opcode list) -> answer
(* the compiler *)
val compile: exptree -> opcode list
