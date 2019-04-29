(* #load "" *)
open Evaluator
open Parser
open Lexer

(* This is the top level file for the expression evaluator. It is basically an infinite loop that consumes legal expression inputs
 * and prints their corresponding parse tree and evaluated output *)
let frame=NULL;
let cstack = [];
let program = (frame,cstack);
let _ =
    Printf.printf "~$ ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = Parser.main Lexer.token lexbuf in
            let program = Evaluator.eval result program;
             Printf.printf "\n~$ "; flush stdout
            (* flush ensures that the evaluated information gets printed to stdout *)
            done
        with Lexer.Eof ->
            exit 0
