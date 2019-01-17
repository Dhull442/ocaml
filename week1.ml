(* Tutorial: Week 1 *)
(*  Run the following code on the interactive toplevel (Run ocaml on the terminal)
    Refer: https://ocaml.org/learn/tutorials/basics.html
*)

(* Hello world! *)
"hello world!";;

(* Let bindings. *)
let x = 5 in 2*x;;
let x = 3;;
3*x;;

(* Function definition. Notice the type inference. *)
let double x = 2 * x;;

(* Recursive functions are defined using `let rec`.
   With let x = e1 in e2, the binding is only present in
   e2's environment, while with let rec x = e1 in e2 the
   binding is present in both e1 and e2's environments. *)
let rec fib x = match x with
    0 -> 0
|   1 -> 1
|   x -> fib(x-1) + fib(x-2);;

(* Order of patterns is important! General rule of thumb is: use more
   specific patterns first. *)
let rec fib' x = match x with
    x -> fib'(x-1) + fib'(x-2)
|   1 -> 1
|   0 -> 0;;

(* Exercise: Factorial *)
let rec fac x = match x with
    0 -> 1
|   x -> x * fac(x-1);;

(* Function call *)
fib 6;;
fib 7;;
fac 6;;
fib' 6;;

(* List introduction *)
(* Empty list *)
[];;
(* Int list *)
[1;2];;
(* List of tuples: notice , instead of ; *)
[(1,true)];;
(* Invalid list : why? *)
[1;true];;
(* Cons *)
1::[];;

(* Map *)
let rec map fn l = match l with
    [] -> []
    | x::xs -> (fn x)::(map fn xs)
;;
let inc x = x+1;;
let double x = x*2 ;;
map inc [1;2;3];;
map double [1;2;3];;

(* Fold *)
let rec foldr fn res l = match l with
    [] -> res
    | x::xs -> fn x (foldr fn res xs)
;;

let rec foldl fn res l = match l with
    [] -> res
    | x::xs -> foldl fn (fn x res) xs
;;
let add x y = x+y;;
foldr add 0 [1;2;3];;
let square x = x*x ;;
foldr add 0 (map square [1;2;3]);;

(* Filter *)
let rec filter pat l = match l with
    [] -> []
    | x::xs -> if (pat x) then x::(filter pat xs) else (filter pat xs)
;;
let isNeg x = (x < 0);;
filter isNeg [5;-5;0];;
(* H.W.: List of lists *)

(* Binary tree search *)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree;;
exception NotFound;;
exception Negative of int;;

(* Take first min(n, length(l)) elements of l *)
let rec take n l = if n < 0 then raise (Negative n)
                    else match l with
                        [ ] -> [ ]
                    |   x::xs -> if n = 0 then [ ]
                                else x::(take (n-1) xs);;

(* Drop first min(n, length(l)) elements of l *)
let rec drop n l = if n < 0 then raise (Negative n)
                    else match l with
                        [ ] -> [ ]
                    |   x::xs -> if n = 0 then l
                        else (drop (n-1) xs);;

(* Make a bst from given l *)
let rec makebst l = match l with
                    [ ] -> Leaf
                |   [x] -> Node(x,Leaf, Leaf)
                |   _ -> let
                            halfn = (List.length l)/2
                        in
                            let l1 = take halfn l
                                and l2 = drop halfn l
                            in Node(List.hd l2, makebst l1, makebst (List.tl l2));;

(* Insert x into a bst t *)
let rec insert x t = match t with
            Leaf -> Node(x,Leaf,Leaf)
        |   Node(y,t1,t2) -> if x <= y then Node(y, insert x t1, t2)
                            else Node(y, t1, insert x t2);;

(* Max elem in a bst *)
let rec bstmax t = match t with
            Leaf -> raise NotFound
        |   Node(x,t1,Leaf) -> x
        |   Node(x,t1,t2) -> bstmax t2;;

(* Min elem in a bst *)
let rec bstmin t = match t with
            Leaf -> raise NotFound
        |   Node(x,Leaf,t2) -> x
        |   Node(x,t1,t2) -> bstmin t1;;

(* Check if a bst *)
let rec isbst t = match t with
            Leaf -> true
        |   Node(x,t1,t2) -> (isbst t1) && (isbst t2) &&
                                match (t1,t2) with
                                    (Leaf,Leaf) -> true
                                |   (Leaf,_) -> x <= (bstmin t2)
                                |   (_,Leaf) -> (bstmax t1) <= x
                                |   (_,_) -> ((bstmax t1) <= x) && (x <= (bstmin t2));;

(* Binary search in a bst *)
let rec find x t = match t with
            Leaf -> raise NotFound
        |   Node(y,t1,t2) -> if x = y then x
                            else if x < y then find x t1
                            else find x t2;;


let t1 = makebst [1; 2; 5; 7; 8; 9; 11; 21; 25; 32; 36; 40; 44; 49; 52; 57; 61; 66];;
isbst t1;;
find 44 t1;;
(* find 45 t1;; *)
(* Should throw an exception *)
let t2 = insert 45 t1;;
isbst t2;;
find 45 t2;;

let t1 = makebst [];;
let t2 = insert 3 t1;;


(* Consider the following function that adds two integers *)
let plusA (a,b) = a + b;;
(* val plusA : int * int -> int = <fun> *)
(* The type denotes that plusA is a function that:
    * Takes a single argument - a tuple of two integers as an input
    * Emits an integer as its output
* *)
(* Now consider the following implementation *)
let plusB a b = a + b;;
(* val plus : int -> int -> int = <fun> *)
(* plusB takes in more than a single argument.
 * What is plusB 10.
 * Is it illegal?
 * *)

(* Type for:
 * plusB      : int -> ( int -> int )
 * plusB 1    : int -> int
 * plusB 1 2  : int
 * *)

(* We now consider the following functions *)
let curry f a b = f(a,b);;
(* val curry : ('a * 'b -> 'c) -> 'a -> 'b -> 'c = <fun> *)

let uncurry g(a,b) = g a b;;
(* val uncurry : ('a -> 'b -> 'c) -> 'a * 'b -> 'c = <fun> *)

let plusC = curry plusA;;
(* val plusC : int -> int -> int = <fun> *)
(* Compare type of plusC to the type of plusB
 * curry takes a function in ('a*'b -> 'c') space
 * to a function in ('a -> 'b -> 'c') space.
 * *)

let plusD = uncurry plusB;;
(* val plusD : int * int -> int = <fun> *)
(* Compare type of plusD to the type of plusA
 * uncurry takes a function in ('a ->'b -> 'c) space
 * to a function in ('a*'b -> 'c') space.
 * *)

(* Higher order function *)

let double (x : int) : int = 2 * x;;

let quad (x : int) : int = double (double x);;

let twice ((f : int -> int), (x : int)) : int = f (f x);;

(* Test *)
let fnew (x : int) : int =
  let square (y : int) : int = y * y in
    twice (square, x)
    ;;

let compose f g = fun x -> f (g x);;
(* val compose : ('a -> 'b) -> ('c -> 'a) -> 'c -> 'b = <fun> *)

exception Negative;;
exception Undefined;;
let rec power x n =
   if n < 0 then raise Negative
   else if n=0 then
      if x=0 then raise Undefined
      else 1
   else
      if (x=0) then 0 else
      let r=power x n/2
      in
      if (n mod 2) = 0 then r*r
      else x*r*r;;
(* val power : int -> int -> int = <fun> *)

(* Polynomials in single variable using Ocaml
 * We can represent a polynomial in a single variable as a list
 * of the coefficients of exponents in decreasing order.
 * Eg. x^2 - 5*x + 4 can be represented as [1; -5; 4]
 * or 4*x^2 - 1 can be represented as [4; 0; -1]
 *
 * Now consider the derivative of a polynomial.
 * The list representing the derivative will be
 * the product of each coefficient with its exponent shifted right.
 * Eg. [1; -5; 4] upon derivation becomes [1*2 ; -5*1] or [2, -5].
 * (Derivative of constant 4 is 0 and gets truncated on shifting right)
 * *)
let eval plist x = List.fold_left (fun acc coef -> acc*x + coef) 0 plist;;
let rec derivative_helper plist_rev pow = match plist_rev with [] -> []
    | li::ls -> (pow*li)::(derivative_helper ls (pow+1))
;;
let derivative plist = List.rev (derivative_helper (List.tl (List.rev plist)) 1);;
(*Now evaluating the derivative of a polynomial is trivially the composition of eval and derivative *)
let eval_derivative plist x = eval (derivative plist) x;;
