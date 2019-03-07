open A0
 (* code goes here *)
type answer = Num of bigint | Bool of bool | Tup of int * ( answer list );;

type exptree = Done
  | N of int (* Integer constant *)
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

type opcode = NCONST of bigint | PLUS | TIMES | MINUS | DIV | REM | ABS | UNARYMINUS
    | EQS | GTE | LTE | GT | LT | PAREN
    | BCONST of bool | CONJ | DISJ | NOT
    | IFTE | TUPLE of int | PROJ of int;;

 (* Excpetions *)
exception InvalidArgument;;
exception IndexOutOfBound;;
 (* Eval function using simple recursive calls *)
let rec eval t =
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
      Done -> raise InvalidArgument
    | N number -> Num ( mk_big number )
    | B boolean -> Bool boolean
    | Var str -> raise InvalidArgument
    | Conjunction ( a , b ) -> Bool (  ( bunwrap ( eval a )  ) || ( bunwrap ( eval b )  )  )
    | Disjunction ( a , b ) -> Bool (  ( bunwrap ( eval a )  ) && ( bunwrap ( eval b )  )  )
    | Not t -> Bool ( not ( bunwrap ( eval t )  )  )
    | Equals ( a , b ) ->  Bool ( eq ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | GreaterTE ( a , b ) -> Bool ( geq ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | LessTE ( a , b ) -> Bool ( leq ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | GreaterT ( a , b ) -> Bool ( gt ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | LessT ( a , b ) -> Bool ( lt ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | InParen a -> eval a
    | IfThenElse ( cond , caseT , caseF ) -> if ( bunwrap ( eval cond )  ) then ( eval caseT ) else ( eval caseF )
    | Tuple ( intg , explist ) -> Tup ( intg , map eval explist )
    | Project (  ( i , n ) , tree ) -> ( match ( eval tree ) with Tup ( size , alist ) -> ( if (  ( i <= n ) && ( n <= size )  ) then get i alist else raise InvalidArgument ) | _ -> raise InvalidArgument )
    | Plus ( a , b ) -> Num ( add ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | Minus ( a , b ) -> Num ( sub ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | Mult ( a , b ) -> Num ( mult ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | Div ( a , b ) -> Num ( div ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | Rem ( a , b ) -> Num ( rem ( nunwrap ( eval a )  )  ( nunwrap ( eval b )  )  )
    | Negative a -> Num ( minus ( nunwrap ( eval a )  )  )
    | Abs a -> Num ( abs ( nunwrap ( eval a )  )  ) ;;
 (* val eval : exptree -> answer *)

 (* Stackmc function with tailrecursion *)
let rec stackmc bl ol =
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
| NCONST a :: ols-> stackmc (  ( Num a ) :: bl ) ols
| PLUS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( add ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| MINUS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( sub ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| TIMES :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( mult ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| DIV :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( div ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| REM :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Num ( rem ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| ABS :: ols -> ( match bl with el1 :: bls -> stackmc (  ( Num ( abs ( nunwrap el1 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| UNARYMINUS :: ols -> ( match bl with el1 :: bls-> stackmc (  ( Num ( minus ( nunwrap el1 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| EQS :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( eq ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| GTE :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( geq ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| LTE :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( leq ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| GT :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( gt ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| LT :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool ( lt ( nunwrap el1 )  ( nunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| PAREN :: ols -> stackmc bl ols
| BCONST b :: ols -> stackmc (  ( Bool b ) :: bl ) ols
| CONJ :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool (  ( bunwrap el1 ) || ( bunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| DISJ :: ols -> ( match bl with el2 :: ( el1 :: bls ) -> stackmc (  ( Bool (  ( bunwrap el1 ) && ( bunwrap el2 )  )  ) :: bls ) ols | _ -> raise InvalidArgument )
| NOT :: ols -> ( match bl with el :: bls -> stackmc (  ( Bool ( not ( bunwrap el )  )  ) ::bls ) ols| _ -> raise InvalidArgument )
| IFTE :: ols -> ( match bl with caseF :: ( caseT :: ( cond :: bls )  ) -> ( if ( bunwrap cond ) then stackmc ( caseT :: bls ) ols else stackmc ( caseF :: bls ) ols ) | _ -> raise InvalidArgument )
| TUPLE a :: ols -> ( let temp = (extractlist a [] bl) in stackmc (  ( Tup ( a , ( getT 1 temp )  )  ) :: ( getT 2 temp )  ) ols )
| PROJ which :: ols -> ( match bl with Tup ( a , alist ) ::bls -> stackmc (  ( get which alist ) ::bls ) ols | _ -> raise InvalidArgument );;
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
  (* | Var str -> [] *)
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
  | Project (  ( i , n ) , tree ) -> ( compile tree ) @ [ PROJ i ]
  | Plus ( a , b ) -> ( compile a ) @ ( compile b ) @ [ PLUS ]
  | Minus ( a , b ) -> ( compile a ) @ ( compile b ) @ [ MINUS ]
  | Mult ( a , b ) -> ( compile a ) @ ( compile b ) @ [ TIMES ]
  | Div ( a , b ) -> ( compile a ) @ ( compile b ) @ [ DIV ]
  | Rem ( a , b ) -> ( compile a ) @ ( compile b ) @ [ REM ]
  | Negative a -> ( compile a ) @ [ UNARYMINUS ]
  | Abs a -> ( compile a ) @ [ ABS ]
  | _ -> raise InvalidArgument ;;
 (* val compile: exptree -> opcode list *)
