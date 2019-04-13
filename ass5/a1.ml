(* Dummy implementation of A1 *)
open A0
exception Not_implemented

(* The type of value returned by the definitional interpreter. *)
type value = NumVal of int | BoolVal of bool | TupVal of int * (value list)

(* The language should contain the following types of expressions:  integers and booleans *)
type answer = Num of bigint | Bool of bool | Tup of int * (answer list)

(* The possible types of expressions in the language of expressions *)
type exptype = Tint | Tunit | Tbool | Ttuple of (exptype list) | Tfunc of (exptype * exptype)

type closure = CL of exptree * ( (string * closure) list) | VCL of value ;;

(* abstract syntax *)
type  exptree =
  Var of string (* variables starting with a Capital letter, represented as alphanumeric strings with underscores (_) and apostrophes (') *)
  | N of int      (* Integer constant *)
  | B of bool     (* Boolean constant *)
  (* unary operators on integers *)
  | Abs of exptree                   (* abs *)
  | Negative of exptree              (* unary minus ~ *)
  (* unary operators on booleans *)
  | Not of exptree
  (* binary operators on integers *)
  | Add of exptree * exptree         (* Addition + *)
  | Sub of exptree * exptree         (* Subtraction - *)
  | Mult of exptree * exptree        (* Multiplication * *)
  | Div of exptree * exptree         (* div *)
  | Rem of exptree * exptree         (* mod *)
  (* binary operators on booleans *)
  | Conjunction of exptree * exptree (* conjunction /\ *)
  | Disjunction of exptree * exptree (* binary operators on booleans \/ *)
  (* comparison operations on integers *)
  | Equals of exptree * exptree      (* = *)
  | GreaterTE of exptree * exptree   (* >= *)
  | LessTE of exptree * exptree      (* <= *)
  | GreaterT of exptree * exptree    (* > *)
  | LessT of exptree * exptree       (* < *)
  (* expressions using parenthesis *)
  | InParen of exptree               (* ( ) *)
  (* a conditional expression *)
  | IfThenElse of exptree * exptree * exptree (* if then else fi  *)
  (* creating n-tuples (n >= 0) *)
  | Tuple of int * (exptree list)
  (* projecting the i-th component of an expression (which evaluates to an n-tuple, and 1 <= i <= n) *)
  | Project of (int*int) * exptree   (* Proj((i,n), e)  0 < i <= n *)
  | Let of definition * exptree
  | FunctionAbstraction of string * exptype * exptree
  | FunctionCall of exptree * exptree
(* definition *)
and definition =
    Simple of string * exptype * exptree
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition

(* opcodes of the stack machine (in the same sequence as above) *)
type opcode = VAR of string | NCONST of bigint | BCONST of bool | ABS | UNARYMINUS | NOT
  | PLUS | MINUS | MULT | DIV | REM | CONJ | DISJ | EQS | GTE | LTE | GT | LT
  | PAREN | IFTE | TUPLE of int | PROJ of int*int | LET | FABS | FCALL
  | SIMPLEDEF | SEQCOMPOSE | PARCOMPOSE | LOCALDEF



(* Excpetions *)
exception InvalidArgument;;
exception IndexOutOfBound;;
exception Error of string;;

let rec execute t rho clos =
match t with
N number -> VCL (NumVal ( number ))
| Var str -> VCL (rho str)
| B boolean -> VCL (BoolVal boolean)
| Conjunction ( a , b ) -> BoolVal ( ( bunwrap ( eval a rho )  ) || ( bunwrap ( eval b rho) ) )
| Disjunction ( a , b ) -> BoolVal ( ( bunwrap ( eval a rho)  ) && ( bunwrap ( eval b rho)  )  )
| Not t -> BoolVal ( not ( bunwrap ( eval t rho ) ) )
| Equals ( a , b ) ->  BoolVal ( ( nunwrap ( eval a rho)  ) == ( nunwrap ( eval b rho)  )  )
| GreaterTE ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) >= ( nunwrap ( eval b rho)  )  )
| LessTE ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) <= ( nunwrap ( eval b rho)  )  )
| GreaterT ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) > ( nunwrap ( eval b rho)  )  )
| LessT ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) < ( nunwrap ( eval b rho)  )  )
| InParen a -> eval a rho
| IfThenElse ( cond , caseT , caseF ) -> if ( bunwrap ( eval cond rho ) ) then ( eval caseT rho ) else ( eval caseF rho )
| Tuple ( intg , explist ) -> if (List.length explist != intg) then raise InvalidArgument else TupVal ( intg , map simpleeval explist)
| Project (  ( i , n ) , tree ) -> ( match ( eval tree rho ) with TupVal ( size , alist ) -> ( if (  ( i <= n ) && ( n <= size )  ) then get i alist else get 1 alist ) | _ -> raise (Error "The eval was not a tuple :(") )
| Add ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) + ( nunwrap ( eval b rho)  )  )
| Sub ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) - ( nunwrap ( eval b rho)  )  )
| Mult ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) * ( nunwrap ( eval b rho)  )  )
| Div ( a , b ) -> NumVal ( divide ( nunwrap ( eval a rho) ) ( nunwrap ( eval b rho) ) )
| Rem ( a , b ) -> NumVal ( modulo ( nunwrap ( eval a rho) ) ( nunwrap ( eval b rho) ) )
| Negative a -> NumVal ( -1 * ( nunwrap ( eval a rho)  )  )
| Abs a -> NumVal ( absolute ( nunwrap ( eval a rho)  )  )
| Let ( d , t ) ->
| FunctionAbstraction ( s , typ , tree )
| FunctionCall ( f , arg ) ->

(* Eval function using simple recursive calls *)
let rec eval t rho =
 (* Provides +ve modulo  *)
 let rec modulo a1 b1 =
   if a1 < 0 then modulo ( a1 + b1 ) b1
   else a1 mod b1
 in
 (* Provides accurate quotient with +ve modulo *)
 let rec divide a1 b1 =
   if a1 < 0 then ( divide ( a1 + b1 ) b1 ) - 1
   else a1 / b1
 in
 let absolute a =
 if( a < 0 ) then -a else a
 in
 (* Unwrap bigint *)
 let nunwrap a = match a with
   NumVal a1 -> a1
   | _ -> raise (InvalidArgument)
 in
 (* unwrap booleans *)
 let bunwrap b = match b with
   BoolVal b1 -> b1
   | _ -> raise InvalidArgument
 in
 let simpleeval a = eval a rho
 in
 (* mapping function *)
 let rec map func a = match a with
   [] -> []
 |  li :: ls -> ( func li ) :: ( map func ls )
 in
 (* get ith element *)
 let rec get i alist = match alist with
  li::ls -> ( if i > 1 then get ( i-1 ) ls else li )
 | _ -> raise IndexOutOfBound
 in
 match t with
     N number -> NumVal ( number )
   | Var str -> (rho str)
   | B boolean -> BoolVal boolean
   | Conjunction ( a , b ) -> BoolVal ( ( bunwrap ( eval a rho )  ) || ( bunwrap ( eval b rho) ) )
   | Disjunction ( a , b ) -> BoolVal ( ( bunwrap ( eval a rho)  ) && ( bunwrap ( eval b rho)  )  )
   | Not t -> BoolVal ( not ( bunwrap ( eval t rho ) ) )
   | Equals ( a , b ) ->  BoolVal ( ( nunwrap ( eval a rho)  ) == ( nunwrap ( eval b rho)  )  )
   | GreaterTE ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) >= ( nunwrap ( eval b rho)  )  )
   | LessTE ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) <= ( nunwrap ( eval b rho)  )  )
   | GreaterT ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) > ( nunwrap ( eval b rho)  )  )
   | LessT ( a , b ) -> BoolVal ( ( nunwrap ( eval a rho)  ) < ( nunwrap ( eval b rho)  )  )
   | InParen a -> eval a rho
   | IfThenElse ( cond , caseT , caseF ) -> if ( bunwrap ( eval cond rho ) ) then ( eval caseT rho ) else ( eval caseF rho )
   | Tuple ( intg , explist ) -> if (List.length explist != intg) then raise InvalidArgument else TupVal ( intg , map simpleeval explist)
   | Project (  ( i , n ) , tree ) -> ( match ( eval tree rho ) with TupVal ( size , alist ) -> ( if (  ( i <= n ) && ( n <= size )  ) then get i alist else get 1 alist ) | _ -> raise (Error "The eval was not a tuple :(") )
   | Add ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) + ( nunwrap ( eval b rho)  )  )
   | Sub ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) - ( nunwrap ( eval b rho)  )  )
   | Mult ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) * ( nunwrap ( eval b rho)  )  )
   | Div ( a , b ) -> NumVal ( divide ( nunwrap ( eval a rho) ) ( nunwrap ( eval b rho) ) )
   | Rem ( a , b ) -> NumVal ( modulo ( nunwrap ( eval a rho) ) ( nunwrap ( eval b rho) ) )
   | Negative a -> NumVal ( -1 * ( nunwrap ( eval a rho)  )  )
   | Abs a -> NumVal ( absolute ( nunwrap ( eval a rho)  )  )
   | Let ( d , t ) ->
   | FunctionAbstraction ( s , typ , tree )
   | FunctionCall ( f , arg ) ->
(* val eval : exptree -> value *)

let stackmc stk binding pgm = raise Not_implemented
let compile ex = raise Not_implemented
