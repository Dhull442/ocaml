{
  open Parser
  exception InvalidToken of char;;
}

(*
  Below is a dummy implementation. Please note the following
  - Tokens are defined in A3.mly
  - Return type is token and not token list
  - End of buffer is indicated by EOF token below
  - There is no trailer. The scanner function is written in the wrapper file (test_a4.ml)
*)

(* REGEX PATTERNS *)
let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let integer = ('0'|['1'-'9']digit*)
let alphanum = ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
let everything = ['A'-'Z' 'a'-'z' '0'-'9' '_' ' ' '\n' '\t' '&' ]*

(* RULE *)
rule read = parse
 whitespace             { read lexbuf }
| '/''*' everything '*''/'      { read lexbuf }
| integer as n          { INT (int_of_string n) }
| '\n'                  { EOL }
| '('                   { LP }
| ')'                   { RP }
| "var"                 { PVAR }
| "call"                { PCALL }
| "="                   { EQ }
| "program"             { PROGRAM }
| "ps"                  { PS }
| "procedure"           { PPROCEDURE }
| ';'                   { SEMICOLON }
| ':'                   { COLON }
| "Tint"                { TINT }
| "Tunit"               { TUNIT }
| alphanum as s         { ID s }
| eof                   { EOF }
| _ as invalid          { raise (InvalidToken invalid) }
