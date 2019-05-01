open Main
open Parser
open Lexer
let ans = ref Main.ans;;
let _ =
Printf.printf ":~> ";flush stdout;
    let lexbuf = Lexing.from_channel stdin in
        while true do
        try
        let result = Parser.comd Lexer.read lexbuf in
        ans := Main.eval result !ans ;
        Printf.printf "\n:~> "; flush stdout
        (* flush ensures that the evaluated information gets printed to stdout *)
        with Main.Error s ->
          print_bytes ("EXCEPTION: "^s^" \n:~> ") ; flush stdout
        done
