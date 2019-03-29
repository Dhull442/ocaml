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
| 'a''b''s'             { ABS }
| '~'                   { TILDA }
| '+'                   { PLUS }
| '-'                   { MINUS }
| '*'                   { TIMES }
(* | '^'                   { EXP } *)
| 'd''i''v'             { DIV }
| 'm''o''d'             { REM }
| '('                   { LP }
| ')'                   { RP }
| 'n''o''t'             { NOT }
| '/''\\'               { DISJ }
| '\\''/'               { CONJ }
(* | '>''='                { GEQ } *)
(* | '<''='                { LEQ } *)
| '='                   { EQ }
| '>'                   { GT }
| '<'                   { LT }
| 'i''f'                { IF }
| 't''h''e''n'          { THEN }
| 'e''l''s''e'          { ELSE }
| 'f''i'                { FI }
| 'd''e''f'             { DEF }
| 'l''e''t'             { LET }
| 'i''n'                { IN }
| 'e''n''d'             { END }
| '.'                   { DOT }
| '\\'                  { BACKSLASH }
| ';'                   { SEMICOLON }
| '|''|'                { PARALLEL }
| 'l''o''c''a''l'       { LOCAL }
| ','                   { COMMA }
| 'p''r''o''j'          { PROJ }
| alphanum as s         { ID s }
| eof                   { EOF }
| _ as invalid          { raise (InvalidToken invalid) }
