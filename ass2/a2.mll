 (* TOKEN definition *)
{
  type token =
     INT of int          (* integer constant, positive or negative w/o leading zeros *)
  |  TRUE                (* boolean constant "T" *)
  |  FALSE               (* boolean constant "F" *)
  |  ABS                 (* unary operator, "abs" *)
  |  PLUS                (* arithmetic plus, "+" *)
  |  MINUS               (* arithmetic minus, "-" *)
  |  MUL                 (* arithmetic multiply, "*" *)
  |  DIV                 (* integer div, "div" *)
  |  MOD                 (* remainder, "mod" *)
  |  EXP                 (* exponentiation, "^" *)
  |  LP                  (* left paren, "(" *)
  |  RP                  (* right paren, ")" *)
  |  NOT                 (* boolean NOT, "not" *)
  |  AND                 (* boolean AND, "/\ " *)
  |  OR                  (* boolean OR, "\/" *)
  |  EQ                  (* equal to, "=" *)
  |  GTA                 (* greater than, ">" *)
  |  LTA                 (* less than, "<" *)
  |  GEQ                 (* greater than/equal to, ">=" *)
  |  LEQ                 (* less than/equal to, "<=" *)
  |  IF                  (* keyword "if" *)
  |  THEN                (* keyword "then" *)
  |  ELSE                (* keyword "else" *)
  |  ID of string        (* variable identifier, alphanumeric string with first char lowercase *)
  |  DEF                 (* definition construct, "def" *)
  |  DELIMITER         (* delimiter, ";" *)
  |  ENDINPUT;;         (* command for end of input signature *)
  exception InvalidToken of char;;
}

(* REGEX PATTERNS *)

let whitespace = [' ' '\n' '\t']+
let digit = ['0'-'9']
let integer = ['-']?('0'|['1'-'9']digit*)
let alphanum = ['a'-'z']['A'-'Z' 'a'-'z' '0'-'9']*

(* RULE *)
rule read = parse
  whitespace {read lexbuf}
| integer as n  { INT (int_of_string n) }
| 'T'   { TRUE }
| 'F'   { FALSE }
| 'a''b''s' {ABS}
| '+'  { PLUS }
| '-'  { MINUS }
| '*'  {MUL}
| 'd''i''v'  {DIV}
| 'm''o''d'  {MOD}
| '^'  {EXP}
| '('     { LP }
| ')'    {  RP }
| 'n''o''t'  {NOT}
| '/''\\'     { AND }
| '\\''/'    {  OR }
| '>''='  {GEQ}
| '<''='  {LEQ}
| '='  {EQ}
| '>'  {GTA}
| '<'  {LTA}
| 'i''f'  {IF}
| 't''h''e''n'  {THEN}
| 'e''l''s''e'  {ELSE}
| 'd''e''f'  {DEF}
| alphanum as s { ID s }
| ';' {DELIMITER}
| eof {ENDINPUT}
| _ as invalid {raise (InvalidToken invalid)}


(* FUNCTIONS *)
{
  (* Function to read recursively *)
  let rec readwhole lexbuf readlist = match read lexbuf with
    ENDINPUT -> readlist
  | readval  -> readwhole lexbuf (readlist @ [readval] );;

  let scanner s = readwhole (Lexing.from_string s) [];;
}


(* EXAMPLES *)
(* scanner "";; - : token list = [] *)
(* scanner "123 -123";; - : token list = [INT 123; INT (-123)] *)
(* scanner "123 - 123";; - : token list = [INT 123; MINUS; INT 123] *)
(* scanner "T /\\ F";; - : token list = [TRUE; AND; FALSE] *)
(* scanner "vAr001=120";; - : token list = [ID "sEAd"; EQ; INT 120] *)
(* scanner "if (cat = 10) then T else (24*7 + 23)";; - : token list = [IF; LP; ID "cat"; EQ; INT 10; RP; THEN; TRUE; ELSE; LP; INT 24; MUL; INT 7; PLUS; INT 23; RP] *)
(* scanner ">==<";; - : token list = [GEQ; EQ; LTA] *)
(* scanner "modnot";; - : token list = [ID "modnot"] *)
(* scanner "mod not";; - : token list = [MOD; NOT] *)
(* canner "10 + -20 - 10*230div-2";; - : token list = [INT 10; PLUS; INT (-20); MINUS; INT 10; MUL; INT 230; DIV; INT (-2)] *)
(* scanner "def >= 10";; - : token list = [DEF; GEQ; INT 10] *)
(* scanner " notT ";; - : token list = [ID "notT"] *)
(* scanner "+192 --1";; - : token list = [PLUS; INT 192; MINUS; INT (-1)] *)
(* scanner "000";; - : token list = [INT 0; INT 0; INT 0] *)
(* scanner "i(f>=<";; - : token list = [ID "i"; LP; ID "f"; GEQ; LTA] *)
(* scanner "if>=<";; - : token list = [IF; GEQ; LTA] *)
(* scanner "10div2";; - : token list = [INT 10; ID "div2"] *)
(* scanner "div;";; - : token list = [DIV; DELIMITER] *)
(* scanner ";()mod;
;;;;+10;";; - : token list = [DELIMITER; LP; RP; MOD; DELIMITER; DELIMITER; DELIMITER; DELIMITER; DELIMITER; PLUS; INT 10; DELIMITER] *)
