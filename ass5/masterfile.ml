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

exception Not_implemented

(* Some example expressions *)
let p0="\\X.(3*X+5)(3)";;
let p1="if 10>20 then \\X.(if X then 1 else 0 fi) (T) else 100 fi";;
let p2 ="let def Func1 = \\X.(if ^X^ then 1 else 1 + (X-1) fi) in Func1 (4) end";;
let p3 ="let def Fib = rec \\X.(if ^X^ then Fib (X-1) + Fib (X-2) else 0 fi) in Fib 4 end";;
let p4 ="let def Power = rec \\X.(if proj (0) X = 0 then proj (2) X else let def Y = (proj (1) X)*(proj (2) X) in Power ( (proj (0) X) -1 , proj (1) X, Y ) end fi) in Power (4, 7, 1) end";;
let p5 ="let def GCD = rec \\X.(if proj (1) X = 0 then proj (0) X else let def R = (proj (0) X) mod (proj (1) X) in GCD (proj (1) X, R) end fi) in GCD (10,11) end"
let p6 = "let def Fac = rec \\X.(if X = 0 then 1 else X*Fac(X-1) fi) in Fac 10 end ";;
let p7 ="
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


(* Types of programs executed *)
(* Fibonacci series *)
(* Power function *)
(* Factorial function *)
(* Tail recursive factorial *)
