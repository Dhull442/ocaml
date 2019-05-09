open Main
open Parser
open Lexer
let ans = ref Main.ans;;
let _ =
print_endline "_______________________________________________________________________";
print_endline "| SYNTAX | call P (a,b) | pointers | view | a := 10 | a := b | return | ";
print_endline "|________|______________|__________|______|_________|________|________|";
Printf.printf ":~> ";flush stdout;
    let lexbuf = Lexing.from_channel stdin in
        while true do
        try
        let result = Parser.comd Lexer.read lexbuf in
        ans := Main.eval result !ans ;
        print_endline "_______________________________________________________________________";
        print_endline "| SYNTAX | call P (a,b) | pointers | view | a := 10 | a := b | return | ";
        print_endline "|________|______________|__________|______|_________|________|________|";
        Printf.printf ":~> ";flush stdout;
        (* flush ensures that the evaluated information gets printed to stdout *)
        with Main.Error s ->
          print_bytes ("EXCEPTION: "^s^" \n:~> ") ; flush stdout
        done
