type token =
  | ABS
  | NEG
  | PLUS
  | MINUS
  | MUL
  | DIV
  | MOD
  | EXP
  | LP
  | RP
  | NOT
  | AND
  | OR
  | EQ
  | GTA
  | LTA
  | IF
  | THEN
  | ELSE
  | FI
  | DEF
  | DELIMITER
  | EOF
  | COMMA
  | PROJ
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)

val main :
  (Lexing.lexbuf  -> token) -> Lexing.lexbuf -> A1.exptree
