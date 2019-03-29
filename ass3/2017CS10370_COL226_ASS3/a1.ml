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

 (* Excpetions *)
exception InvalidArgument;;
exception IndexOutOfBound;;
exception Error of string;;
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
    | IfThenElse ( cond , caseT , caseF ) -> if ( bunwrap ( eval cond rho)  ) then ( eval caseT rho) else ( eval caseF rho)
    | Tuple ( intg , explist ) -> if (List.length explist != intg) then raise InvalidArgument else TupVal ( intg , map simpleeval explist)
    | Project (  ( i , n ) , tree ) -> ( match ( eval tree rho ) with TupVal ( size , alist ) -> ( if (  ( i <= n ) && ( n <= size )  ) then get i alist else get 1 alist ) | _ -> raise (Error "The eval was not a tuple :(") )
    | Plus ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) + ( nunwrap ( eval b rho)  )  )
    | Minus ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) - ( nunwrap ( eval b rho)  )  )
    | Mult ( a , b ) -> NumVal ( ( nunwrap ( eval a rho)  ) * ( nunwrap ( eval b rho)  )  )
    | Div ( a , b ) -> NumVal ( divide ( nunwrap ( eval a rho) ) ( nunwrap ( eval b rho) ) )
    | Rem ( a , b ) -> NumVal ( modulo ( nunwrap ( eval a rho) ) ( nunwrap ( eval b rho) ) )
    | Negative a -> NumVal ( -1 * ( nunwrap ( eval a rho)  )  )
    | Abs a -> NumVal ( absolute ( nunwrap ( eval a rho)  )  ) ;;
 (* val eval : exptree -> value *)

 (* Stackmc function with tailrecursion *)
let rec stackmc bl binding ol =
 (* Unwrap bigint *)
let nunwrap a = match a with
  Num a1 -> a1
  | _ -> raise InvalidArgument
in
 (* unwrap booleans *)
let bunwrap b = match b with
  Bool b1 -> b1
  | _ -> raise InvalidArgument
in
 (* Extract top n elements from stack *)
let rec extractlist n ansstack stack = if ( n <= 0 ) then ( ansstack , stack ) else
match stack with
si :: ss -> extractlist ( n-1 )  ( si :: ansstack ) ss
| [] -> raise InvalidArgument
in
 (* get ith element *)
let rec get i alist = match alist with
 li::ls -> ( if i > 1 then get ( i-1 ) ls else li )
| _ -> raise IndexOutOfBound
in
 (* get ith element of a pair *)
let getT i tuple = match tuple with
 (a,b) -> (if i = 1 then a else b)
in
match ol with
  [] -> List.hd bl
| VAR str :: ols -> stackmc ( ( binding str ) :: bl ) binding ols
| NCONST a :: ols-> stackmc (  ( Num a ) :: bl ) binding ols
| PLUS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( add ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| MINUS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( sub ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| MULT :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( mult ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| DIV :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( div ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| REM :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( rem ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| ABS :: ols -> ( match bl with el1 :: bls -> stackmc (  ( Num ( abs ( nunwrap el1 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| UNARYMINUS :: ols -> ( match bl with el1 :: bls-> stackmc (  ( Num ( minus ( nunwrap el1 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| EQS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( eq ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| GTE :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( geq ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| LTE :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( leq ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| GT :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( gt ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| LT :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( lt ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| PAREN :: ols -> stackmc bl binding ols
| BCONST b :: ols -> stackmc (  ( Bool b ) :: bl ) binding ols
| CONJ :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool (  ( bunwrap el1 ) || ( bunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| DISJ :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool (  ( bunwrap el1 ) && ( bunwrap el2 )  )  ) :: bls ) binding ols | _ -> raise InvalidArgument )
| NOT :: ols -> ( match bl with el :: bls -> stackmc (  ( Bool ( not ( bunwrap el )  )  ) ::bls ) binding ols | _ -> raise InvalidArgument )
| IFTE :: ols -> ( match bl with caseF :: ( caseT :: ( cond :: bls )  ) -> ( if ( bunwrap cond ) then stackmc ( caseT :: bls ) binding ols else stackmc ( caseF :: bls ) binding ols ) | _ -> raise InvalidArgument )
| TUPLE a :: ols -> ( let temp = (extractlist a [] bl) in stackmc (  ( Tup ( a , ( getT 1 temp )  )  ) :: ( getT 2 temp )  ) binding ols )
| PROJ (which,tot) :: ols -> ( match bl with Tup ( a , alist ) ::bls -> stackmc (  ( get which alist ) ::bls ) binding ols | _ -> raise InvalidArgument );;
(* | _ -> raise InvalidArgument *)
 (* val stackmc: ( bigint list ) -> ( opcode list ) -> bigint *)

 (* Tail rec compilation *)
let rec compile t =
  let rec listcompile a = match a with
    li :: ls -> ( compile li ) @ ( listcompile ls )
    | [] -> []
  in
  match t with
    N a -> [ NCONST ( mk_big a ) ]
  | B b ->  [ BCONST b ]
  | Var str -> [ VAR str ]
  | Conjunction ( a , b ) -> ( compile a ) @ ( compile b ) @ [ CONJ ]
  | Disjunction ( a , b ) -> ( compile a ) @ ( compile b ) @ [ DISJ ]
  | Not a -> ( compile a ) @ [ NOT ]
  | Equals ( a , b ) -> ( compile a ) @ ( compile b ) @ [ EQS ]
  | GreaterTE ( a , b ) -> ( compile a ) @ ( compile b ) @ [ GTE ]
  | LessTE ( a , b ) -> ( compile a ) @ ( compile b ) @ [ LTE ]
  | GreaterT ( a , b ) -> ( compile a ) @ ( compile b ) @ [ GT ]
  | LessT ( a , b ) -> ( compile a ) @ ( compile b ) @ [ LT ]
  | InParen ( a ) -> [ PAREN ] @ ( compile a )
  | IfThenElse ( cond , caseT , caseF ) -> ( compile cond ) @ ( compile caseT ) @ ( compile caseF ) @ [ IFTE ]
  | Tuple ( a , explist ) -> ( listcompile explist ) @ [TUPLE a]
  | Project (  ( i , n ) , tree ) -> ( compile tree ) @ [ PROJ (i,n) ]
  | Plus ( a , b ) -> ( compile a ) @ ( compile b ) @ [ PLUS ]
  | Minus ( a , b ) -> ( compile a ) @ ( compile b ) @ [ MINUS ]
  | Mult ( a , b ) -> ( compile a ) @ ( compile b ) @ [ MULT ]
  | Div ( a , b ) -> ( compile a ) @ ( compile b ) @ [ DIV ]
  | Rem ( a , b ) -> ( compile a ) @ ( compile b ) @ [ REM ]
  | Negative a -> ( compile a ) @ [ UNARYMINUS ]
  | Abs a -> ( compile a ) @ [ ABS ];;
 (* val compile: exptree -> opcode list *)
