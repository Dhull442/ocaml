open Evaluator
open Parser
open Lexer
let frame=NULL;
let cstack = [];
let program = (frame,cstack);
let _ =
    Printf.printf "~$ ";flush stdout;
    try
        let lexbuf = Lexing.from_channel stdin in
            while true do
            let result = Parser.main Lexer.token lexbuf in
            let program = Evaluator.eval result program in Printf.printf "~$ "; flush stdout
            done
      with Lexer.Eof ->
            exit 0
