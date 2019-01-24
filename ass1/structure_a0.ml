open Signature_a0
  module A0 : BigInt = struct
(* class definition *)
type sign = Neg | NonNeg;;
type bigint = sign * int list;;

(* Assumption: *)
(* 1) 0 will be indicated as (_,[])  *)

(* exceptions *)
exception InvalidInt;;

(* Conversion functions from OCaml int to bigint. *)
let mk_big a = let rec unwrap x l = if x = 0 then l else (unwrap (x/10) ((x mod 10)::l ))
in 
if a < 0 then (Neg, unwrap (-a) []) 
else (NonNeg, unwrap a []);;


(* Functions to present the result in the form of a string. *)
let print_num a =  let rec tostring l s = match l with
[] -> s
| xi :: xs -> ( string_of_int xi ) ^ ( tostring xs s ) 
in 
match a with 
(Neg, l) -> "-" ^ tostring l "" | (NonNeg, l) -> tostring l "";;

(* Comparison Operation: *)
(* Helper function for comparing unsigned lists *)
let listcomp  a b = if List.length a > List.length b then 1 else 
if List.length a < List.length b then -1 else
let rec eqlengthlistcomp a1 b1 =
match a1 with 
a1i::a1s -> (match b1 with b1i::b1s -> (if b1i = a1i then eqlengthlistcomp a1s b1s else if b1i > a1i then -1 else 1) | [] -> 1 )
| [] -> (match b1 with [] -> 0 | _ -> -1)
in 
eqlengthlistcomp a b ;; 

(* Equal *)
let eq a b =match a with 
(_,[]) -> (match b with (_, []) -> true | (_ , _) -> false)
| (NonNeg, al) -> (match b with (_,[]) -> false | (Neg, _) -> false | (NonNeg, bl) -> if listcomp al bl = 0 then true else false)
| (Neg, al) -> (match b with (_,[]) -> false | (NonNeg, _) -> false | (Neg, bl) -> if listcomp al bl = 0 then true else false);;
(* Greater_than. *)
let gt a b =match a with 
(_,[]) -> (match b with (_ ,[] ) -> false | (Neg , _) -> true | (_,_) -> false )
| (NonNeg, al) -> (match b with (_,[]) -> true | (Neg, _) -> true | (NonNeg, bl) -> if listcomp al bl > 0 then true else false)
| (Neg, al) -> (match b with (_,[]) -> false | (NonNeg, _) -> false | (Neg, bl) -> if listcomp al bl < 0 then true else false);;
(* val gt:  bigint -> bigint -> bool *)
(* Less_than. *)
let lt a b =match a with 
(_,[]) -> (match b with (_ ,[] ) -> false | (NonNeg , _) -> true | (_,_) -> false )
| (NonNeg, al) -> (match b with (_,[]) -> false | (Neg, _) -> false | (NonNeg, bl) -> if listcomp al bl < 0 then true else false)
| (Neg, al) -> (match b with (_,[]) -> true | (NonNeg, _) -> true | (Neg, bl) -> if listcomp al bl > 0 then true else false);;
(* val lt:  bigint -> bigint -> bool *)
(* Great_or_equal. *)
let geq a b = not (lt a b);;
(* val geq:  bigint -> bigint -> bool *)
(* Less_or_equal.  *)
let leq a b = not (gt a b);;
(* val leq:  bigint -> bigint -> bool *)


(* Checklist to elimintate waste 0's *)
let rec checklist c = match c with
[] -> []
| ci :: cs -> if ( ci = 0 ) then checklist cs else c;;

(* Inverted List Addition *)
(* takes two inverted lists and gives back summation in normal sequence *)
(* ans = !a + !b  *)
let list_add a b =
  let rec modulo a1 b1 = if a1 < 0 then modulo (a1+b1) b1
  else a1 mod b1
  in
  let rec divide a1 b1 = if a1 < 0 then divide (a1 + b1) b1 - 1
  else a1 / b1
  in
  match a with
 _ -> let rec add_carry a1 b1 carry = match a1 with
          [] -> (match b1 with
                 [] ->( if carry = 0 then [] else [carry] )
               | b1i :: b1s ->( modulo (b1i + carry) 10 )  :: add_carry [] b1s ( divide ( b1i + carry ) 10) )
        | a1i :: a1s -> (match b1 with
                 [] -> add_carry [] a1 carry
               | b1i :: b1s -> ( modulo ( a1i + b1i + carry ) 10 ) :: add_carry a1s b1s ( divide ( a1i + b1i + carry ) 10 ))
       in checklist( List.rev (add_carry a b 0)) ;;



(* Negation of the List *)


(* Unsigned Subtract List *)
(* ans = !a - !b  *)
let list_subtract a b =
let nega l = match l with [] -> [] | x :: xs -> (-x) :: xs 
in 
let rec negation a = match a with 
[] -> []
| li :: ls -> (-li)::(negation ls) 
in
if listcomp (List.rev a) (List.rev b) < 0 then nega (checklist (list_add b (negation a)))
else if  listcomp (List.rev a) (List.rev b) = 0 then [] 
else checklist (list_add a (negation b));;



(* Multiply lists *)
(* check for zero lists in mult function itself *)
(* ans = a*b *)
let list_mult a b = let multiply x1 x2 = x1*x2
in 
let rec map fn a l = match l with 
[] -> []
| x::xs -> (fn a x) :: (map fn a xs)
in
let rec bitbybit l1 l2 = match l2 with
[] -> []
| l2i :: l2s -> list_add (List.rev (bitbybit (l1 @ [0]) l2s)) (List.rev (map multiply l2i l1))
in checklist (bitbybit a (List.rev b));;

(* Unsigned List div *)
(* ans = !a / !b  *)
let rec list_div a b =if ( listcomp (List.rev a) (List.rev b) < 0 ) then [] else list_add [1] (List.rev (list_div (List.rev (list_subtract a b)) b));;
let rec neg_list_div a b =if ( listcomp (List.rev a) (List.rev b) <= 0 ) then [] else list_add [1] (List.rev (neg_list_div (List.rev (list_subtract a b)) b));;


(* Convert subtracted list to bigint *)
let cvt_listsub_to_sub a = match a with 
[] -> (NonNeg, [])
| x::xs -> if x < 0 then (Neg, (-x) :: xs ) else (NonNeg, a);;

(* Addition *)
let add a b = match a with 
(_, [] ) -> b
| (NonNeg, al) -> ( match b with (_,[])-> a | (NonNeg, bl) -> (NonNeg, list_add (List.rev al) (List.rev bl)) | (Neg, bl) -> cvt_listsub_to_sub (list_subtract (List.rev al) (List.rev bl)) )
| (Neg, al) -> (match b with (_,[])-> a | (Neg, bl) -> (Neg, list_add (List.rev al) (List.rev bl)) | (NonNeg, bl) -> cvt_listsub_to_sub (list_subtract (List.rev bl) (List.rev al)) ) ;;

(* Subtraction *)
let sub a b = match a with 
(_,[]) ->(  match b with (NonNeg, bs) -> (Neg, bs) | (Neg,bs) -> (NonNeg,bs) )
|  (NonNeg, al) -> (match b with (_,[])-> a | (Neg, bl) -> (NonNeg, list_add (List.rev al) (List.rev bl)) | (NonNeg, bl) ->  cvt_listsub_to_sub (list_subtract (List.rev al) (List.rev bl)))
|  (Neg, al) -> (match b with (_,[])-> a | (NonNeg, bl) -> (Neg, list_add (List.rev al) (List.rev bl)) | (Neg, bl) ->  cvt_listsub_to_sub (list_subtract (List.rev bl) (List.rev al)) ) ;;

(* Multiplication *)
let mult a b = match a with 
(_,[]) -> (NonNeg, [])
| (Neg, al) ->( match b with (_,[]) -> (NonNeg, []) | (Neg, bl) -> (NonNeg, list_mult al bl ) | (NonNeg, bl) -> (Neg, list_mult al bl) )
| (NonNeg, al) ->( match b with (_,[]) -> (NonNeg, []) | (Neg, bl) -> (Neg, list_mult al bl ) | (NonNeg, bl) -> (NonNeg, list_mult al bl) ) ;;
(* val mult: bigint -> bigint -> bigint *)

(* Quotient *)
let div a b = match a with
(_,[]) -> a
| (NonNeg, al) -> (match b with (_,[]) -> raise InvalidInt  | (NonNeg, bl) -> (NonNeg, list_div (List.rev al) (List.rev bl)) | (Neg, bl) -> (Neg,  list_div (List.rev al) (List.rev bl) ) )
| (Neg, al) -> (match b with (_,[]) -> raise InvalidInt | (NonNeg, bl) ->  (Neg, list_add [1] (List.rev (neg_list_div (List.rev al) (List.rev bl)) )) | (Neg, bl) -> (NonNeg, list_add [1] (List.rev (neg_list_div (List.rev al) (List.rev bl)) )) );;
(* val div: bigint -> bigint -> bigint *)
(* Remainder *)
let rem a b = match b with
(_,[]) -> raise InvalidInt
| (_,_) -> sub a (mult (div a b) b);;
(* val rem: bigint -> bigint -> bigint *)

(* Absolute value *)
let abs a = match a with
(Neg, l) -> (NonNeg, l)
| (_,_) -> a;;
(* val abs: bigint -> bigint *)

(* Unary negation *)
let minus a = match a with 
(_,[]) -> a 
| (Neg, l) -> (NonNeg, l)
| (NonNeg, l) -> (Neg, l);;
(* val minus: bigint -> bigint *)

end