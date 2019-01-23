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

(* Complementary functions *)
let rec modulo a b = if a < 0 then modulo (a+b) b
else a mod b;;
let rec divide a b = if a < 0 then divide (a + b) b - 1
else a / b ;;

(* Inverted List Addition *)
(* takes two inverted lists and gives back summation in normal sequence *)
let list_add a b = match a with
[] -> ( match b with [] -> [0] | _ -> b )
| _ -> let rec add_carry a1 b1 carry = match a1 with
          [] -> (match b1 with
                 [] ->( if carry = 0 then [] else [carry] )
               | b1i :: b1s ->( modulo (b1i + carry) 10 )  :: add_carry [] b1s ( divide ( b1i + carry ) 10) )
        | a1i :: a1s -> (match b1 with
                 [] -> add_carry [] a1 carry
               | b1i :: b1s -> ( modulo ( a1i + b1i + carry ) 10 ) :: add_carry a1s b1s ( divide ( a1i + b1i + carry ) 10 ))
       in List.rev (add_carry a b 0) ;;

(* Negation of the List *)
let rec negation a = match a with 
[] -> []
| li :: ls -> (-li)::(negation ls);; 

(* Subtract List *)
let list_subtract a b =
  let nega l = match l with [] -> [] | x :: xs -> (-x) :: xs 
  in 
if listcomp (List.rev a) (List.rev b) < 0 then nega (list_add b (negation a))
else if  listcomp (List.rev a) (List.rev b) = 0 then [] 
else list_add a (negation b);;

(* Convert subtracted list to bigint *)
let cvt_listsub_to_sub a = match a with 
[] -> (NonNeg, [])
| x::xs -> if x < 0 then (Neg, (-x) :: xs ) else (NonNeg, a);;

(* Addition *)
let add a b = match a with 
(_, [] ) -> b
| (NonNeg, al) -> ( match b with (_,[])-> raise InvalidInt | (NonNeg, bl) -> (NonNeg, list_add (List.rev al) (List.rev bl)) | (Neg, bl) -> cvt_listsub_to_sub (list_subtract (List.rev al) (List.rev bl)) )
| (Neg, al) -> (match b with (_,[])-> raise InvalidInt | (Neg, bl) -> (Neg, list_add (List.rev al) (List.rev bl)) | (NonNeg, bl) -> cvt_listsub_to_sub (list_subtract (List.rev bl) (List.rev al)) ) ;;

(* Subtraction *)
let sub a b = match a with 
(_,[]) ->(  match b with (NonNeg, bs) -> (Neg, bs) | (Neg,bs) -> (NonNeg,bs) )
|  (NonNeg, al) -> (match b with (_,[])-> raise InvalidInt | (Neg, bl) -> (NonNeg, list_add (List.rev al) (List.rev bl)) | (NonNeg, bl) ->  cvt_listsub_to_sub (list_subtract (List.rev al) (List.rev bl)))
|  (Neg, al) -> (match b with (_,[])-> raise InvalidInt | (NonNeg, bl) -> (Neg, list_add (List.rev al) (List.rev bl)) | (Neg, bl) ->  cvt_listsub_to_sub (list_subtract (List.rev bl) (List.rev al)) ) ;;

(* Absolute value *)
let abs a = match a with
(Neg, l) -> (NonNeg, l)
| (_,_) -> a;;
(* val abs: bigint -> bigint *)

end