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
  | Cmp of expr
  | Equals of expr * expr
  | GreaterT of expr * expr
  | GreaterTE of expr * expr
  | LessT of expr * expr
  | LessTE of expr * expr
  | If_Then_Else of (expr * expr * expr)
  | Let of definition * expr
and definition =
    Simple of string * expr;;


type closure = CL of expr * ((string * closure) list) | VCL of expr | DCL of definition * (string * closure) list;;

type krivinetoken = KADD of closure | KSUB of closure | KMULT of closure | KDO of closure | KDONE of closure
                  | KDEF of (string * closure) | KIFTE of closure | KAND of closure | KOR of closure | KCMP;;

type opcode = VAR of string | NCONST of int | BCONST of bool | NOT | CMP
  | PLUS | MINUS | MULT | DIV | REM | AND | OR | EQS | GTE | LTE | GT | LT | DEF of string
  | LET of (opcode list)
  | PAREN | IFTE | CLOS of string*(opcode list) | RET | FCALL;;

type answer = N of int | B of bool | C of string*(opcode list)*((string * answer) list);;

type dump = D of (answer list)*((string * answer) list)*(opcode list);;


exception InvalidArgument;;
exception Error of string;;

let rec find x g = match g with
 gi :: gs -> (match gi with (a,b) -> (if (a = x) then b else find x gs))
 | [] -> raise (Error ("No definition found for "^x) );;

let rec augment g g_dash =
 let rec contains key l = match l with
   li :: ls -> (match li with (a,b) -> if ( a = key ) then true else contains key ls )
   | [] -> false
   in
   match g with
   li :: ls -> (match li with (key,value) -> if contains key g_dash then augment ls g_dash else augment ls (li :: g_dash))
   | [] -> g_dash
;;
