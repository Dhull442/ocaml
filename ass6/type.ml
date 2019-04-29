exception Error of string;;

type typ = Tint | Tunit ;;

type expr = N of int | VAR of string * typ | ASSIGN of string * expr | CALL of string * (expr list) | DEFINE of procedure | RET | DCL of expr list | V of string | Program of string | ViewStack
and procedure = P of string * (expr list);;
type register = string * typ * expr;;
type stackframe = F of string * (register list) * (register list) * (register list) * (stackframe list) * stackframe | NULL;;
(* stackframe = name * decl vbls in this frame * input vbls * vbls from prev * stackframe list callable from here * head frame  *)
(* let rec eval ls *)
