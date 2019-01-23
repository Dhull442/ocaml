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
let print_num a = match a with 


 (* Comparison Operation: *)

(* Inverted List Addition *)
let rec modulo a b = if a < 0 then modulo (a+b) b
else a mod b;;
let rec divide a b = if a < 0 then divide (a + b) b - 1
else a / b ;;
let list_add a b = match a with
[] -> ( match b with [] -> [0] | _ -> b )
| _ -> let rec add_carry a1 b1 carry = match a1 with
          [] -> (match b1 with
                 [] ->( if carry = 0 then [] else [carry] )
               | b1i :: b1s ->( modulo (b1i + carry) 10 )  :: add_carry [] b1s ( divide ( b1i + carry ) 10) )
        | a1i :: a1s -> (match b1 with
                 [] -> add_carry [] a1 carry
               | b1i :: b1s -> ( modulo ( a1i + b1i + carry ) 10 ) :: add_carry a1s b1s ( divide ( a1i + b1i + carry ) 10 ))
       in add_carry a b 0 ;;

(* Negation of the List *)
let rec negation a = match a with 
[] -> []
| li :: ls -> (-li)::(negation ls);; 

(* Subtract List *)
let list_subtract a b =
if List.length a < List.length b then list_subtract b a 
else if List.length a = List.length b then 
        if List.hd (List.rev a) > List
  match a with 
[] -> b 
| _ -> list_add a (negation b);;

(* Addition *)
let add a b = match a with 
(_, [] ) -> raise InvalidInt
| (NonNeg, al) -> ( match b with (_,[])-> raise InvalidInt | (NonNeg, bl) -> (NonNeg, List.rev (list_add (List.rev al) (List.rev bl))) | (Neg, bl) -> raise InvalidInt )
| (Neg, al) -> (match b with (_,[])-> raise InvalidInt | (Neg, bl) -> (Neg, List.rev (list_add (List.rev al) (List.rev bl))) | (NonNeg, bl) -> raise InvalidInt ) ;;

(* Subtraction *)
let sub a b = match a with 
(_,[]) -> raise InvalidInt
|  (NonNeg, []) -> (match b with (_,[])-> raise InvalidInt | (Neg, bl) -> (NonNeg, List.rev (list_add (List.rev al) (List.rev bl))) | (NonNeg, bl) -> raise InvalidInt)
|  (Neg, al) -> (match b with (_,[])-> raise InvalidInt | (NonNeg, bl) -> (Neg, List.rev (list_add (List.rev al) (List.rev bl))) | (Neg, bl) -> raise InvalidInt ) ;;