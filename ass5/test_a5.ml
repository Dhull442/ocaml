#directory "_build";; (* Consider this folder when looking for files *)
#load "types.cmo";;
#load "krivinemachine.cmo";;
#load "secdmachine.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";;
open Types;;
open Krivinemachine;;
open Secdmachine;;
open Lexer;;
open Parser;;

exception Not_implemented;;

let p1 =  App (Lambda (V "x", Mult (Integer 3, V "x")), Integer 4);;
	(*12*)
let p2 = If_Then_Else
   (Cmp (Integer 7),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 31),
    Integer 0);;
   (*34*)
let p3 = If_Then_Else
    (Cmp (Integer 0),
    App (Lambda (V "x", Plus (Integer 3, V "x")), Integer 4),
        Integer 110);;
    (*110*)

let p4 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), Bool true)), Bool false);;
(*false*)

let p5 = App(Lambda(V "x", App(Lambda(V "y", Or(V "x", V "y")), Bool true)), Bool false);;
(*true*)

let p6 = App(Lambda(V "x", Mult(V "x", App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer 3))), Integer 2);;
(*14*)

let p7 = If_Then_Else(
  Cmp(App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), Integer (-5))),
  Integer (-29),
  App(Lambda(V "x", Plus(V "x",App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 7))), Integer 5));;
(*13*)

let p8 = App(Lambda(V "x", App(Lambda(V "y", Plus(V "x", V "y")), Integer 4)), App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 3));;
(*10*)

let p9 = App(Lambda(V "x", App(Lambda(V "y", Mult(V "x", V "y")), V "x")), Integer 4);;
(*16*)

let p10 = App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Mult(V "x", Integer 2)), App(Lambda(V "x", Plus(V "x", Integer 4)), Integer 3)))), Integer 20);;
(*34*)

let p11 = App(Lambda(V "x", App(Lambda(V "y", And(V "x", V "y")), V "x")), Bool true);;
(*true*)

let p12 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", Integer 2)), Integer 4)), App(Lambda(V "x", App(Lambda(V "y", Or(V "x", V "y")), V "x")), Bool false), Bool true);;
(*false*)

let p13 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool true);;
(*true*)

let p14 = App(Lambda(V "x", And(V "x", App(Lambda(V "x", And(V "x", Bool true)), App(Lambda(V "x", And(V "x", Bool true)), Bool true)))), Bool false);;
(*false*)

let p15 = If_Then_Else(Cmp(App(Lambda(V "x", Mult(V "x", App(Lambda(V "y", V "y"), V "x"))), Integer 1)), App(Lambda(V "x", Plus(V "x", App(Lambda(V "x", Plus(V "x", Integer 1)), Integer 3))), Integer 5), Integer (-1));;
(*9*)
let g0="\\X.(3*X+5)(3)";;
let g1="if 10>20 then \\X.(if X then 1 else 0 fi) (T) else 100 fi";;
let g2 ="let def Func1 = \\X.(if ^X^ then 1 else 1 + (X-1) fi) in Func1 (4) end";;
let g3 ="let def Fib = rec \\X.(if ^X^ then Fib (X-1) + Fib (X-2) else 0 fi) in Fib 4 end";;
let g4 ="let def Power = rec \\X.(if proj (0) X = 0 then proj (2) X else let def Y = (proj (1) X)*(proj (2) X) in Power ( (proj (0) X) -1 , proj (1) X, Y ) end fi) in Power (4, 7, 1) end";;
let g5 ="let def GCD = rec \\X.(if proj (1) X = 0 then proj (0) X else let def R = (proj (0) X) mod (proj (1) X) in GCD (proj (1) X, R) end fi) in GCD (10,11) end"
let g6 = "let def Fac = rec \\X.(if X = 0 then 1 else X*Fac(X-1) fi) in Fac 10 end ";;
let g7 ="
let def Power =
 rec \\X.(
    let def A = proj (0) X
    in
    let
     def B = proj (1) X
    in
      if B = 0 then 1
      else
        if (B mod 2) = 0 then
          let def C = Power (A, B div 2) in C*C end
        else
          let def C = Power (A, B div 2) in C*C*A end
        fi
      fi
      end
    end
  )
in
  Power (3, 7)
end
";;
(* Some example exptrees *)
let e0 = Let ( Simple ("fun1",Lambda( V "x", Plus ( V "x", Integer 30 ) ) ), App ( V "fun1" , Integer 5 ) ) ;;
let rece = Let ( Simple ("fun1", RecLambda(V "x", If_Then_Else (Cmp (V "x"),App ( V "fun1", Minus ( V "x", Integer 1 )),Integer 0))), App ( V "fun1", Integer 4 ) );;
let fac = Let ( Simple ("fun1", RecLambda(V "x", If_Then_Else (Cmp (V "x"),Mult ( V "x", App ( V "fun1", Minus ( V "x", Integer 1 )) ),Integer 1))), App ( V "fun1", Integer 4 ) );;


(*Your code will go here*)
(*For thise who have implemented lexer parser, modify the testcases in your grammar and you will have to get those tet_cases at the time of the demo*)


(* functions *)
(* expression parser *)
let exp_parser s = Parser.exp_parser Lexer.read (Lexing.from_string s) ;;
(* definition parser *)
let def_parser s = Parser.def_parser Lexer.read (Lexing.from_string s) ;;
(* krivine executioner *)
let krivine s g = Krivinemachine.kmc (CL (exp_parser s,g)) [];;
(* compiler from expr to opcode *)
let compile s = Secdmachine.compile (exp_parser s);;
(* secd executioner *)
let secd s g = Secdmachine.secdmc [] g (compile s) [];;
(* everything *)
let eval s g a = (krivine s g,secd s a,compile s,exp_parser s);;

let eval_secd inp = Secdmachine.secdmc [] [] (Secdmachine.compile inp) [];;

let eval_krivine inp = match (Krivinemachine.kmc (CL (inp,[])) []) with KDONE (VCL x) :: ls -> x | _ -> raise (Not_implemented);;

(*Your code ends*)

let check_secd n inp out =
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (inp = out)
    then print_string("Passed ")
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let check_krivine n inp out =
  print_string("T" ^ string_of_int(n) ^ " : ");
  try if (inp = out)
    then print_string("Passed ")
    else print_string("Failed ");
  with e -> print_endline("Failed : Wrong exception raised : " ^ (Printexc.to_string e))
;;

let print_heading a = print_endline("\n" ^ a ^ " :");;

(*SECD*)
print_heading "SECD test cases\n";;

check_secd 1 (eval_secd p1) (N 12);;
check_secd 2 (eval_secd p2) (N 34);;
check_secd 3 (eval_secd p3) (N 110);;
check_secd 4 (eval_secd p4) (B false);;
check_secd 5 (eval_secd p5) (B true);;
check_secd 6 (eval_secd p6) (N 14);;
check_secd 7 (eval_secd p7) (N 13);;
check_secd 8 (eval_secd p8) (N 10);;
check_secd 9 (eval_secd p9) (N 16);;
check_secd 10 (eval_secd p10) (N 34);;
check_secd 11 (eval_secd p11) (B true);;
check_secd 12 (eval_secd p12) (B false);;
check_secd 13 (eval_secd p13) (B true);;
check_secd 14 (eval_secd p14) (B false);;
check_secd 15 (eval_secd p15) (N 9);;

print_heading "Krivine test cases";;

check_krivine 1 (eval_krivine p1) (Integer 12);;
check_krivine 2 (eval_krivine p2) (Integer 34);;
check_krivine 3 (eval_krivine p3) (Integer 110);;
check_krivine 4 (eval_krivine p4) (Bool false);;
check_krivine 5 (eval_krivine p5) (Bool true);;
check_krivine 6 (eval_krivine p6) (Integer 14);;
check_krivine 7 (eval_krivine p7) (Integer 13);;
check_krivine 8 (eval_krivine p8) (Integer 10);;
check_krivine 9 (eval_krivine p9) (Integer 16);;
check_krivine 10 (eval_krivine p10) (Integer 34);;
check_krivine 11 (eval_krivine p11) (Bool true);;
check_krivine 12 (eval_krivine p12) (Bool false);;
check_krivine 13 (eval_krivine p13) (Bool true);;
check_krivine 14 (eval_krivine p14) (Bool false);;
check_krivine 15 (eval_krivine p15) (Integer 9);;
(*Krivine*)
