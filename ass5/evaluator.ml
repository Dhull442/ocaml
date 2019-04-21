(* #directory "_build";; (* Consider this folder when looking for files *)
#load "types.cmo";;
#load "krivinemachine.cmo";;
#load "secdmachine.cmo";;
#load "parser.cmo";;
#load "lexer.cmo";; *)
open Types;;
open Krivinemachine;;
open Secdmachine;;
open Lexer;;
open Parser;;

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let _ =
    Printf.printf ">> ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = Parser.exp_parser Lexer.read lexbuf in
            Printf.printf "Answer: "; Krivinemachine.kmc (CL (result, [])) []; Printf.printf "\n==> "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with Lexer.Eof ->
            exit 0
