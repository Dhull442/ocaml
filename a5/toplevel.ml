#directory "_build";; (* Consider this folder when looking for files *)
#load "a1.cmo";;
#load "a3.cmo";;
#load "parser.cmo";;
open A1;;
open Parser;;
open A3;;

exception Not_implemented
(* Helper function to print *)
(* let rec print_tree tr = match tr with
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
let rec print_def df = match df with
  Simple(l,r) -> "def " ^ l ^ " = " ^ (print_tree r)
  | _ -> raise Not_implemented
;;


Input is given as value and output is an answer *)
(* let rec toAnswer v = match v with
  NumVal a     -> Num (mk_big a)
| BoolVal b    -> Bool b
| TupVal (n,xs) -> Tup (n, List.map toAnswer xs);; *)

(* Input is given as string and output is an answer *)
(* let binding rho s = toAnswer (rho s);; *)
let p0="\\X:Tint.(3*X+5)(3)";;
let p1="if 10>20 then \\X:Tbool.(if X then 1 else 0 fi) (T) else 100 fi";;
(* Both use the same lexer in A1 but different parser in A3 *)
let exp_parser s = A3.exp_parser Parser.read (Lexing.from_string s) ;;
(* let def_parser s rho = A3.def_parser A2.read (Lexing.from_string s) ;; *)
let krivine s = A1.execute (exp_parser s) [];;
let compile s = A1.compile (exp_parser s);;
let secd s = A1.stackmc [] [] (A1.compile (exp_parser s)) [];;
let eval s = (krivine s,secd s,compile s,exp_parser s);;
(* Input is given as string and output is a value *)
(* let rho s = match s with
  "X" -> NumVal 5
  |  "Y" -> BoolVal true
  |  "Z" -> TupVal (3, [NumVal 5; BoolVal true; NumVal 1])
  | _ -> raise Not_implemented
;; *)

(* Sample parsing *)
(* print_endline ( print_tree (exp_parser "5" rho));;
print_endline ( print_def (def_parser "def A=5" rho));; *)

(* Sample test case *)
(* let e = (exp_parser "\\X.Y" rho);;
let t = Tfunc (Tint, Tbool);; *)

(* Type assumptions as a list of tuples of the form (variable name, type) *)
(* let g = [("X", Tint); ("Y", Tbool);("Z", Ttuple [Tint ; Tbool ; Tint]); ("W", Tfunc (Tint, Tbool))];; *)
(* let d = (def_parser "def U = X ; def V = Y" rho);; *)
(* let g_dash = [("U", Tint); ("V", Tbool)];; *)

(* assert(hastype g e t);;
assert(yields g d g_dash);; *)
