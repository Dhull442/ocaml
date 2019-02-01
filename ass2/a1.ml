open A0
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
    | Abs a -> let k = eval a in if k <0 then (-k) else k ;;
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
let rec compile t = match t with
    N a -> [CONST ( mk_big a )]
  | Plus ( a , b ) -> ( compile a ) @ ( compile b ) @ [ PLUS ]
  | Minus ( a , b ) -> ( compile a ) @ ( compile b ) @ [ MINUS ]
  | Mult ( a , b ) -> ( compile a ) @ ( compile b ) @ [ TIMES ]
  | Div ( a , b ) -> ( compile a ) @ ( compile b ) @ [ DIV ]
  | Rem ( a , b ) -> ( compile a ) @ ( compile b ) @ [ REM ]
  | Nega a -> ( compile a ) @ [ UNARYMINUS ]
  | Abs a -> ( compile a ) @ [ ABS ];;
(* val compile: exptree -> opcode list *)
