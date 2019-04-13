{
  open A3
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

(* RULE *)
rule read = parse
 whitespace             { read lexbuf }
| integer as n          { INT (int_of_string n) }
| 'T'                   { BOOL (true) }
| 'F'                   { BOOL (false) }
| "abs"                 { ABS }
| '~'                   { TILDA }
| '+'                   { PLUS }
| '-'                   { MINUS }
| '*'                   { TIMES }
(* | '^'                   { EXP } *)
| "div"                 { DIV }
| "mod"                 { REM }
| '('                   { LP }
| ')'                   { RP }
| "not"                 { NOT }
| '/''\\'               { DISJ }
| '\\''/'               { CONJ }
(* | '>''='                { GEQ } *)
(* | '<''='                { LEQ } *)
| '='                   { EQ }
| '>'                   { GT }
| '<'                   { LT }
| "if"                  { IF }
| "then"                { THEN }
| "else"                { ELSE }
| "fi"                  { FI }
| "def"                 { DEF }
| "let"                 { LET }
| "in"                  { IN }
| "end"                 { END }
| '.'                   { DOT }
| '\\'                  { BACKSLASH }
| ';'                   { SEMICOLON }
| ':'                   { COLON }
| '|''|'                { PARALLEL }
| "local"               { LOCAL }
| ','                   { COMMA }
| "proj"                { PROJ }
| "Tint"                { TINT }
| "Tunit"               { TUNIT }
| "Tbool"               { TBOOL }
| alphanum as s         { ID s }
| eof                   { EOF }
| _ as invalid          { raise (InvalidToken invalid) }
