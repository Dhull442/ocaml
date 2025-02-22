(* class definition *)
type sign = Neg | NonNeg;;
type bigint = sign * int list;;

(* Assumption: *)
(* 1 ) 0 will be indicated as ( _ , [] )  *)
(* 2 ) Inverted List -> most to least significant digit from start to end whereas normal list is the one having the order least -> most significant digit *)

(* exceptions *)
exception InvalidInt;;
exception IndexOutOfBound;;


(* Conversion functions from OCaml int to bigint. *)
let mk_big a =
  let rec unwrap x l =
    if x = 0 then l
    else ( unwrap ( x/10 ) ( ( x mod 10 )::l ) )
  in
  if a < 0 then ( Neg , unwrap ( - a ) [] )
  else ( NonNeg , unwrap a [] );;


(* Functions to present the result in the form of a string. *)
let print_num a =
  let rec tostring l s = match l with
    [] -> s
    | xi :: xs -> ( string_of_int xi ) ^ ( tostring xs s )
  in
  match a with
    (_,[]) -> "0"
    | ( Neg , l ) -> "-" ^ tostring l ""
    | ( NonNeg , l ) -> tostring l "";;

(* Helper function for comparing unsigned lists *)
let listcomp  a b =
  if List.length a > List.length b then 1
  else if List.length a < List.length b then -1
       else let rec eqlengthlistcomp a1 b1 = match a1 with
              a1i::a1s -> ( match b1 with
                b1i::b1s ->
                  if b1i = a1i then eqlengthlistcomp a1s b1s
                  else if b1i > a1i then -1
                       else 1
                | [] -> 1 )
              | [] -> ( match b1 with
                  [] -> 0
                  | _ -> -1 )
            in
            eqlengthlistcomp a b ;;

(* Equal *)
let eq a b = match a with
  ( _ , [] ) -> ( match b with
    ( _ , [] ) -> true
    | ( _ , _ ) -> false )
  | ( NonNeg , al ) -> ( match b with
    ( _ , [] ) -> false
    | ( Neg , _ ) -> false
    | ( NonNeg , bl ) ->
      if listcomp al bl = 0 then true
      else false )
  | ( Neg , al ) -> ( match b with
    ( _ , [] ) -> false
    | ( NonNeg , _ ) -> false
    | ( Neg , bl ) ->
      if listcomp al bl = 0 then true
      else false );;

(* Greater_than. *)
let gt a b = match a with
  ( _ , [] ) -> ( match b with
    | ( Neg , _ ) -> true
    | ( _ , _ ) -> false )
  | ( NonNeg , al ) -> ( match b with
    ( _  ,  [] ) -> true
    | ( Neg , _ ) -> true
    | ( NonNeg , bl ) ->
      if listcomp al bl > 0 then true
      else false )
  | ( Neg , al ) -> ( match b with
    ( _ , [] ) -> false
    | ( NonNeg , _ ) -> false
    | ( Neg , bl ) ->
      if listcomp al bl < 0 then true
      else false );;

(* Less_than. *)
let lt a b = match a with
  ( _ , [] ) -> ( match b with
    ( _ , [] ) -> false
    | ( NonNeg , _ ) -> true
    | ( _ , _ ) -> false )
  | ( NonNeg , al ) -> ( match b with
    ( _ , [] ) -> false
    | ( Neg , _ ) -> false
    | ( NonNeg , bl ) ->
      if listcomp al bl < 0 then true
      else false )
  | ( Neg , al ) -> ( match b with
    ( _ , [] ) -> true
    | ( NonNeg , _ ) -> true
    | ( Neg , bl ) ->
      if listcomp al bl > 0 then true
      else false );;

(* Great_or_equal. *)
let geq a b = not ( lt a b );;

(* Less_or_equal.  *)
let leq a b = not ( gt a b );;


(* trim_initial_zeros to elimintate waste 0's *)
let rec trim_initial_zeros c = match c with
  [] -> []
  | ci :: cs ->
    if ci = 0 then trim_initial_zeros cs
    else c;;

(* Inverted List Addition *)
(* takes two inverted lists and gives back the ans as a normal list *)
let list_add a b =
  (* Provides +ve modulo  *)
  let rec modulo a1 b1 =
    if a1 < 0 then modulo ( a1+b1 ) b1
    else a1 mod b1
  in
  (* Provides accurate quotient with +ve modulo *)
  let rec divide a1 b1 =
    if a1 < 0 then divide ( a1 + b1 ) b1 - 1
    else a1 / b1
  in
  let rec add_carry a1 b1 carry = match a1 with
    [] -> ( match b1 with
      [] ->
        if carry = 0 then []
        else [carry]
      | b1i :: b1s -> ( modulo ( b1i + carry ) 10 ) :: add_carry [] b1s ( divide ( b1i + carry ) 10 ) )
    | a1i :: a1s -> ( match b1 with
      [] -> add_carry [] a1 carry
      | b1i :: b1s -> ( modulo ( a1i + b1i + carry ) 10 ) :: add_carry a1s b1s ( divide ( a1i + b1i + carry ) 10 ) )
  in
  trim_initial_zeros ( List.rev ( add_carry a b 0 ) );;

(* Unsigned Subtract List *)
(* Subtracts second list from first *)
(* Take two inverted list and returns ans as a normal list *)
(* ans = !a - !b  *)
let list_subtract a b =
  (* Converts first element to it's negative form *)
  let negative_at_front l = match l with
    [] -> []
    | x :: xs -> ( -x ) :: xs
  in
  (* This function converts each element into negative of that *)
  let rec negation a = match a with
    [] -> []
    | li :: ls -> ( -li )::( negation ls )
  in
  if listcomp ( List.rev a ) ( List.rev b ) < 0 then negative_at_front ( trim_initial_zeros ( list_add b ( negation a ) ) )
  else if listcomp ( List.rev a ) ( List.rev b ) = 0 then []
       else trim_initial_zeros ( list_add a ( negation b ) );;

(* Multiply lists *)
(* check for zero lists in mult function itself *)
(* ans = a*b *)
let list_mult a b =
  let multiply x1 x2 = x1*x2
  in
  (* Mapping function *)
  let rec map fn a l = match l with
    [] -> []
    | x::xs -> ( fn a x ) :: ( map fn a xs )
  in
  (* Bit by bit Multiplication function *)
  let rec bitbybit l1 l2 = match l2 with
    [] -> []
    | l2i :: l2s -> List.rev (list_add ( bitbybit ( 0 :: l1 ) l2s ) ( map multiply l2i l1 ))
  in
  trim_initial_zeros ( List.rev ( bitbybit (List.rev a) ( List.rev b ) ) );;

(* Function for obtaining nth element of a tuple *)
let get n t = match n with
  1 -> ( match t with ( a , _ ) -> a )
  | 2 -> ( match t with ( _ , a ) -> a )
  | _ -> raise IndexOutOfBound;;

(* Unsigned List div *)
(* Take normal list as input and return ans as tuple of inverted lists of quotient and Remainder *)
(* !ans = a / b  *)
let list_div a b = match b with
[] -> raise InvalidInt
| _ ->
(* Division by subtracting multiple times *)
let rec basic_list_div ta tb =
  if ( listcomp ( List.rev ta ) ( List.rev tb ) < 0 ) then ( [] , ta )
  else match basic_list_div ( List.rev ( list_subtract ta tb ) ) tb with
    ( qs , modulus ) -> ( List.rev (list_add [1] qs ) , modulus )
in
let rec bitbybit ta tb tc =
  if listcomp (List.rev (get 2 tc)) (List.rev tb) < 0 then (match ta with [] -> ( List.rev (trim_initial_zeros (List.rev (get 1 tc))), get 2 tc) | ai :: als -> bitbybit als tb (( 0 :: ( get 1 tc ) ), ai :: ( get 2 tc )) )
  else  let k = basic_list_div (get 2 tc) tb in
   bitbybit ta tb ( List.rev ( list_add (get 1 k) ( get 1 tc ) ) , get 2 k )
  in
  bitbybit a (List.rev b) ([],[]);;


(* Unsigned List div for different signs *)
(* Take normal list as input and return ans as tuple of quotient and Remainder *)
(* Takes care of keeping quotient and remainder positive *)
(* ans = a / b *)
let neg_list_div a b =
let ans = list_div a b in
match ans with
(qs,[]) -> (List.rev qs, [])
| (qs,rm) -> (list_add [1] qs, list_subtract (List.rev b) rm);;

(* Convert subtracted list to bigint *)
(* Taking care of -ve sign at the initial of list *)
let convertListtoBigINT a = match a with
  [] -> ( NonNeg , [] )
  | x::xs ->
    if x < 0 then ( Neg , ( -x ) :: xs )
    else ( NonNeg , a );;

(* Addition *)
let add a b = match a with
  ( _ , [] ) -> b
  | ( NonNeg , al ) -> ( match b with
    ( _ , [] )-> a
    | ( NonNeg , bl ) -> ( NonNeg , list_add ( List.rev al ) ( List.rev bl ) )
    | ( Neg , bl ) -> convertListtoBigINT ( list_subtract ( List.rev al ) ( List.rev bl ) ) )
  | ( Neg , al ) -> ( match b with
    ( _ , [] )-> a
    | ( Neg , bl ) -> ( Neg , list_add ( List.rev al ) ( List.rev bl ) )
    | ( NonNeg , bl ) -> convertListtoBigINT ( list_subtract ( List.rev bl ) ( List.rev al ) ) ) ;;

(* Subtraction *)
let sub a b = match a with
  ( _ , [] ) ->( match b with
    ( NonNeg , bs ) -> ( Neg , bs )
    | ( Neg , bs ) -> ( NonNeg , bs ) )
  |  ( NonNeg , al ) -> ( match b with
    ( _ , [] )-> a
    | ( Neg , bl ) -> ( NonNeg , list_add ( List.rev al ) ( List.rev bl ) )
    | ( NonNeg , bl ) ->  convertListtoBigINT ( list_subtract ( List.rev al ) ( List.rev bl ) ) )
  | ( Neg , al ) -> ( match b with
    ( _ , [] )-> a
    | ( NonNeg , bl ) -> ( Neg , list_add ( List.rev al ) ( List.rev bl ) )
    | ( Neg , bl ) ->  convertListtoBigINT ( list_subtract ( List.rev bl ) ( List.rev al ) ) ) ;;

(* Multiplication *)
let mult a b = match a with
  ( _ , [] ) -> ( NonNeg , [] )
  | ( Neg , al ) ->( match b with
    ( _ , [] ) -> ( NonNeg , [] )
    | ( Neg , bl ) -> ( NonNeg , list_mult al bl )
    | ( NonNeg , bl ) -> ( Neg , list_mult al bl ) )
  | ( NonNeg , al ) ->( match b with
    ( _ , [] ) -> ( NonNeg , [] )
    | ( Neg , bl ) -> ( Neg , list_mult al bl )
    | ( NonNeg , bl ) -> ( NonNeg , list_mult al bl ) ) ;;

(* Quotient *)
let div a b = match a with
  ( _ , [] ) -> ( match b with
    ( _ , [] ) -> raise InvalidInt
    | ( _ , _ ) -> a )
  | ( NonNeg , al ) -> ( match b with
    ( _ , [] ) -> raise InvalidInt
    | ( NonNeg , bl ) -> ( NonNeg , List.rev ( get 1 ( list_div al bl ) ) )
    | ( Neg , bl ) -> ( Neg , List.rev ( get 1 ( list_div al bl ) ) ) )
  | ( Neg , al ) -> ( match b with
    ( _ , [] ) -> raise InvalidInt
    | ( NonNeg , bl ) ->  ( Neg , get 1 ( neg_list_div al bl ) )
    | ( Neg , bl ) -> ( NonNeg , get 1 ( neg_list_div al bl ) ) );;

(* Remainder *)
let rem a b =  match a with
  ( _ , [] ) -> ( match b with
    ( _ , [] ) -> raise InvalidInt
    | ( _ , _ ) -> a )
  | ( NonNeg , al ) -> ( match b with
    ( _ , [] ) -> raise InvalidInt
    | ( NonNeg , bl ) -> ( NonNeg , List.rev ( get 2 ( list_div al bl ) ) )
    | ( Neg , bl ) -> ( NonNeg , List.rev ( get 2 ( list_div al bl ) ) ) )
  | ( Neg , al ) -> ( match b with
    ( _ , [] ) -> raise InvalidInt
    | ( NonNeg , bl ) ->  ( NonNeg , get 2 ( neg_list_div al bl ) )
    | ( Neg , bl ) -> ( NonNeg , get 2 ( neg_list_div al bl ) ) );;

(* Absolute value *)
let abs a = match a with
  ( Neg , l ) -> ( NonNeg , l )
  | ( _ , _ ) -> a;;

(* Unary negation *)
let minus a = match a with
  ( _ , [] ) -> a
  | ( Neg , l ) -> ( NonNeg , l )
  | ( NonNeg , l ) -> ( Neg , l );;
