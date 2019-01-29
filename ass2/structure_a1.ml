#load "structure_a0.cmo"
open Structure_a0.A0 (* Should be in same folder*)
open Signature_a1 (* Should be in same folder*)
module A1 : CalculatorLanguage = struct

(* code goes here *)
type exptree =
    N of int
  | Plus of exptree * exptree
  | Minus of exptree * exptree
  | Mult of exptree * exptree
  | Div of exptree * exptree
  | Rem of exptree * exptree
  | Nega of exptree (* Neg is for sign in BigInt. Nega is negative of expression  *)
  | Abs of exptree;;

type opcode =
    CONST of bigint
  | PLUS
  | TIMES
  | MINUS
  | DIV
  | REM
  | ABS
  | UNARYMINUS;;

(* Excpetions *)
exception InvalidArgument;;

(* Eval function using simple recursive calls *)
let rec eval t =
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
  match t with
      N a -> a
    | Plus ( a , b ) -> ( eval a ) + ( eval b )
    | Minus ( a , b ) -> ( eval a ) - ( eval b )
    | Mult ( a , b ) -> ( eval a ) * ( eval b )
    | Div ( a , b ) -> let k = eval b in if k = 0 then raise InvalidArgument else divide ( eval a ) k
    | Rem ( a , b ) -> let k = eval b in if k = 0 then raise InvalidArgument else modulo ( eval a ) k
    | Nega a -> ( -1 ) * ( eval a )
    | Abs a -> let k = eval a in if k < 0 then ( -1 ) * k else k ;;
(* val eval : exptree -> int *)

(* Stackmc function with tailrecursion *)
let stackmc bigintlist opcodelist =
  let rec stackmachine bl ol = match ol with
    [] -> bl
  | ( CONST a ) :: ols-> stackmachine ( a :: bl ) ols
  | PLUS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmachine ( ( add el1 el2 ) :: bls ) ols | _ -> raise InvalidArgument )
  | MINUS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmachine ( ( sub el1 el2 ) :: bls ) ols | _ -> raise InvalidArgument )
  | TIMES :: ols -> ( match bl with el2 :: ( el1 :: bls) -> stackmachine ( ( mult el1 el2 ) :: bls ) ols | _ -> raise InvalidArgument )
  | DIV :: ols -> ( match bl with el2 :: ( el1 :: bls) -> stackmachine ( ( div el1 el2 ) :: bls ) ols | _ -> raise InvalidArgument )
  | REM :: ols -> ( match bl with el2 :: ( el1 :: bls) -> stackmachine ( ( rem el1 el2 ) :: bls ) ols | _ -> raise InvalidArgument )
  | ABS :: ols -> ( match bl with el1 :: bls -> stackmachine ( ( abs el1 ) :: bls ) ols | _ -> raise InvalidArgument )
  | UNARYMINUS :: ols -> ( match bl with el1 :: bls-> stackmachine ( ( minus el1 ) :: bls ) ols | _ -> raise InvalidArgument )
  in
  List.hd ( stackmachine bigintlist opcodelist );;
(* val stackmc: (bigint list) -> (opcode list) -> bigint *)

(* Tail rec compilation *)
let compile tree =
  let rec tailreccompile t ol = match t with
    N a -> ( CONST ( mk_big a ) ) :: ol
  | Plus ( a , b ) -> ( tailreccompile a [] ) @ ( tailreccompile b ( PLUS :: ol ) )
  | Minus ( a , b ) -> ( tailreccompile a [] ) @ ( tailreccompile b ( MINUS :: ol ) )
  | Mult ( a , b ) -> ( tailreccompile a [] ) @ ( tailreccompile b ( TIMES :: ol ) )
  | Div ( a , b ) -> ( tailreccompile a [] ) @ ( tailreccompile b ( DIV :: ol ) )
  | Rem ( a , b ) -> ( tailreccompile a [] ) @ ( tailreccompile b ( REM :: ol ) )
  | Nega a -> tailreccompile a ( ABS :: ol )
  | Abs a -> tailreccompile a ( UNARYMINUS :: ol )
  in
  tailreccompile tree [];;
(* val compile: exptree -> opcode list *)

end
