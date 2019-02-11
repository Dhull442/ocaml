(* Generate a lexer from this file by executing the following command:

    $ ocamllex week2.mll

   Above command generates a file named "week2A.ml". Use this file from an
   ocaml top-level as below:

   #use "week2.ml";;
   lexme "hello";;
   lexme "42";;
   lexme "-42";;
   lexpwd "hello$";;
   lexpwd "hello";;
*)

(* This section within the braces is called the header. You can include
  arbitrary OCaml code here and use it in later sections. *)
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
  |  DELIMITER;;         (* delimiter, ";" *)
  exception InvalidToken of char;;
}

(* This section is called the identifiers section. Here, you can define
  variables pointing to regex patterns. *)
let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let digits = digit+
(* let integer = '-'? digits *)
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let spl_char = ['*' '.' '$']
let letter_or_spl_char = letter|spl_char
let varregexp = ['a'-'z']['a'-'z' '0'-'9']*
let integer = ['+' '-']?('0'|['1'-'9']digit*)
let abs = 'a''b''s'
(* Password that requires a combination of letters and special characters. *)

(* (Exercise) Write regex for:
Integer constants, which have an optional sign, followed by at least one digit, without useless leading zeroes.
Unary arithmetic operations: abs,
Binary operations: + (addition), - (subtraction), * (multiplication), div, mod, ^ (exponentiation)
Parentheses: (, )
Boolean constants: T and F
Unary boolean operation: not
Binary boolean operations:  /\ (and), \/ (or)
Comparison operators: = (equal) , > (greater than), < (less than) , >= (greater or equal), <= (less or equal)
A conditional operator consisting of three tokens: if then else
Identifiers, which are alphanumeric strings beginning with lower-case letter.
A definition construct: def
A delimiter to terminate the expression: ;
*)

(* This section is called the rules section. Think of a rule name as a
  function that, in this case, returns a value of type 'token'. Notice that
  everything inside the curly braces can be any arbitrary OCaml expression. *)
rule read = parse
 integer as n  {[ INT (int_of_string n) ]}
| 'T'   {[ TRUE ]}
| 'F'   {[ FALSE ]}
| whitespace? abs whitespace? {[ABS]}
| whitespace '+' whitespace {[ PLUS ]}
| whitespace '-' whitespace {[ MINUS ]}
| whitespace '*' whitespace {[MUL]}
| whitespace 'd''i''v' whitespace {[DIV]}
| whitespace 'm''o''d' whitespace {[MOD]}
| whitespace '^' whitespace {[EXP]}
| '('     {[ LP ]}
| ')'    { [ RP ]}
| whitespace 'n''o''t' whitespace {[NOT]}
| whitespace '/''\\' whitespace    {[ AND ]}
| whitespace '\\''/' whitespace   { [ OR ]}
| whitespace '=' whitespace {[EQ]}
| whitespace '>' whitespace {[GTA]}
| whitespace '<' whitespace {[LTA]}
| whitespace '>''=' whitespace {[GEQ]}
| whitespace '<''=' whitespace {[LEQ]}
| whitespace 'i''f' whitespace {[IF]}
| whitespace 't''h''e''n' whitespace {[THEN]}
| whitespace 'e''l''s''e' whitespace {[ELSE]}
| varregexp as s   {[ ID s ]}
| whitespace? 'd''e''f' whitespace {[DEF]}
| ';' {[DELIMITER]}
| _ as invalid {raise (InvalidToken invalid)}

(* and readpasswd = parse
  password as p {Password p}

and rec readbrac = parse
  brac as x   {Password x} *)

(* This last section within the curly braces is called the trailer. Some useful
functions are defined here to be usable by clients. Notice how the rules are
invoked as function calls with the input string as an argument. *)
{
  let lexme s = read (Lexing.from_string s)
}
