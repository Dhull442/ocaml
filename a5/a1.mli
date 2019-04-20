(* open A0 *)
(* The possible types of expressions in the language of expressions *)
(* type exptype = Tint | Tunit | Tbool | Tfunc of (exptype * exptype) *)

(* abstract syntax *)
type exptype = Tint | Tunit | Tbool | Tfunc of exptype* exptype;;
type expr =
  V of string
  | Integer of int
  | Bool of bool
  | Lambda of (expr * expr)
  | RecLambda of (expr * expr)
  | App of (expr * expr)
  | Plus of (expr * expr)
  | Minus of (expr * expr)
  | Mult of (expr * expr)
  | Div of (expr * expr)
  | Rem of (expr * expr)
  | And of (expr * expr)
  | Or of (expr * expr)
  | InParen of expr
  | Not of expr
  | Cmp of expr    (* CMP is |sjdnfls| *)
  | Equals of expr * expr
  | GreaterT of expr * expr
  | GreaterTE of expr * expr
  | LessT of expr * expr
  | LessTE of expr * expr
  | If_Then_Else of (expr * expr * expr)
  | Let of definition * expr
and definition =
    Simple of string * expr
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition;;

type closure = CL of expr * ((string * closure) list) | VCL of expr | DCL of definition * (string * closure) list;;

type krivinetoken = KADD of closure | KSUB of closure | KMULT of closure | KDO of closure | KDONE of closure | KDEF of (string * closure) | KIFTE of closure | KAND of closure | KOR of closure | KCMP;;

type opcode = VAR of string | NCONST of int | BCONST of bool | NOT | CMP
  | PLUS | MINUS | MULT | DIV | REM | AND | OR | EQS | GTE | LTE | GT | LT | DEF of string
  | LET of (opcode list)
  | PAREN | IFTE | CLOS of string*(opcode list) | RET | FCALL;;

type answer = N of int | B of bool | C of string*(opcode list)*((string * answer) list);;
type dump = D of (answer list)*((string * answer) list)*(opcode list);;

(* The type of value returned by the definitional interpreter. *)
(* type value = NumVal of int | BoolVal of bool | TupVal of int * (value list) *)

(* the definitional interpreter *)
val kmc : closure -> krivinetoken list -> krivinetoken list
val krivinemc : closure -> closure list -> closure list

val execute : expr -> (bytes * closure) list -> closure
(* the stack machine *)
val stackmc: answer list -> (bytes * answer) list -> opcode list -> dump list -> answer
(* the compiler *)
val compile: expr -> opcode list
