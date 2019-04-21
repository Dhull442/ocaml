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
let whitespace = [' ' '\n' '\t']+
let digit = ['0'-'9']
let integer = ('0'|['1'-'9']digit*)
let alphanum = ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*
let everything = ['A'-'Z' 'a'-'z' '0'-'9' '_' ' ' '\n' '\t' '&' ]*

(* RULE *)
rule read = parse
 whitespace             { read lexbuf }
| '/''*' everything '*''/'      { read lexbuf }
| integer as n          { INT (int_of_string n) }
| 'T'                   { BOOL (true) }
| 'F'                   { BOOL (false) }
| '+'                   { PPLUS }
| '-'                   { PMINUS }
| '*'                   { TIMES }
| '^'                   { PCMP }
| "div"                 { PDIV }
| "mod"                 { PREM }
| '('                   { LP }
| ')'                   { RP }
| "not"                 { PNOT }
| '/''\\'               { DISJ }
| '\\''/'               { CONJ }
| '='                   { EQ }
| '>'                   { GTA }
| '<'                   { LTA }
| "if"                  { IF }
| "then"                { THEN }
| "else"                { ELSE }
| "fi"                  { FI }
| "def"                 { PDEF }
| "let"                 { PLET }
| "in"                  { IN }
| "end"                 { END }
| "rec"                 { PREC }
| '.'                   { DOT }
| '\\'                  { BACKSLASH }
| ';'                   { SEMICOLON }
| ':'                   { COLON }
| '|'                   { PIPE }
| "Tint"                { TINT }
| "Tunit"               { TUNIT }
| "Tbool"               { TBOOL }
| alphanum as s         { ID s }
| eof                   { EOF }
| _ as invalid          { raise (InvalidToken invalid) }
