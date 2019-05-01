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
let alphanum = ['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']+

(* RULE *)
rule read = parse
 whitespace             { read lexbuf }
| integer as n          { INT (int_of_string n) }
| '('                   { LP }
| ')'                   { RP }
| '='                   { EQ }
| ','                   { COMMA }
| "return"              { PRET }
| "view"                { PVIEW }
| "call"                { PCALL }
| "pointers"            { VIEWP }
| ':'                   { COLON }

| '\n'                  { EOL }
| alphanum as s         { ID s }
| _ as invalid          { raise (InvalidToken invalid) }
