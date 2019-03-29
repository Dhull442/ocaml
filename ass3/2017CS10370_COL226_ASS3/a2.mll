 (* TOKEN definition in a3.mly *)
 {
   open A3
   exception Not_implemented;;
   exception InvalidToken of char;;
 }

 (*
   Below is a dummy implementation. Please note the following
   - Tokens are defined in A3.mly
   - Return type is token and not token list
   - End of buffer is indicated by EOF token below
   - There is no trailer. The scanner function is written in the wrapper file
 *)
(* REGEX PATTERNS *)

let whitespace = [' ' '\n' '\t']+
let digit = ['0'-'9']
let integer = ('0'|['1'-'9']digit*)
let alphanum = ['A'-'Z']['A'-'Z' 'a'-'z' '0'-'9' '_' '\'']*

(* RULE *)
rule read = parse
  whitespace            { read lexbuf }
| integer as n          { INT (int_of_string n) }
| 'T'                   { BOOL (true) }
| 'F'                   { BOOL (false) }
| 'a''b''s'             { ABS }
| '~'                   { NEG }
| '+'                   { PLUS }
| '-'                   { MINUS }
| '*'                   { MUL }
| '^'                   { EXP }
| 'd''i''v'             { DIV }
| 'm''o''d'             { MOD }
| '('                   { LP }
| ')'                   { RP }
| 'n''o''t'             { NOT }
| '/''\\'               { AND }
| '\\''/'               { OR }
(* | '>''='                { GEQ } *)
(* | '<''='                { LEQ } *)
| '='                   { EQ }
| '>'                   { GTA }
| '<'                   { LTA }
| 'i''f'                { IF }
| 't''h''e''n'          { THEN }
| 'e''l''s''e'          { ELSE }
| 'f''i'                { FI }
| 'd''e''f'             { DEF }
| ';'                   { DELIMITER }
| ','                   { COMMA }
| 'p''r''o''j'          { PROJ }
| alphanum as s         { ID s }
| eof                   { EOF }
| _ as invalid          { raise (InvalidToken invalid) }
