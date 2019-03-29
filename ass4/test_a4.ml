#directory "_build";; (* Consider this folder when looking for files *)
#load "a0.cmo";;
#load "a1.cmo";;
#load "a2.cmo";;
#load "a3.cmo";;
#load "a4.cmo";;
open A0;;
open A1;;
open A2;;
open A3;;
open A4;;

exception Not_implemented
(* Helper function to print *)
let rec print_tree tr = match tr with
  N a -> "INT " ^ (string_of_int a)
  | _ -> raise Not_implemented
;;
let rec print_answer tr = match tr with
  Num a -> print_num a
  | Bool a -> string_of_bool a
  | _ -> raise Not_implemented
;;
let rec print_value tr = match tr with
  NumVal a -> string_of_int a
  | BoolVal a -> string_of_bool a
  | _ -> raise Not_implemented
;;

(* Input is given as value and output is an answer *)
let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);;

(* Input is given as string and output is an answer *)
let binding rho s = toAnswer (rho s);;

let parser s rho =
  let result = A3.main A2.read (Lexing.from_string s) in
    (* Do not implement eval, compile, stackmc *)
    (* (result, (A1.eval result rho), (A1.stackmc [] (binding rho) (A1.compile result))) *)
    result
;;

(* Input is given as string and output is a value *)
let rho s = match s with
  "X" -> NumVal 5
  |  "Y" -> BoolVal true
  |  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
  | _ -> raise Not_implemented
;;

let e = (parser "\\X.Y" rho);;
let t = Tfunc (Tint, Tbool);;

(* Type assumptions as a list of tuples of the form (variable name, type) *)
let g = [("X", Tint), ("Y", Tbool), ("Z", Ttuple [Tint ; Tbool ; Tint]), ("W", Tfunc (Tint, Tbool))];;
let d = (parser "def U = X ; def V = Y" rho);;
let g_dash = [("U", Tint), ("V", Tbool)];;

assert(hastype g e t);;
assert(yields g d g_dash);;
