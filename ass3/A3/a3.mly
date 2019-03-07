%{
    open A1
%}

/* Tokens are defined below.  */
%token TRUE FALSE ABS PLUS MINUS MUL DIV MOD NEG EXP LP RP NOT AND OR EQ GTA LTA GEQ LEQ IF THEN ELSE DEF DELIMITER EOF COMMA PROJ
%token <int> INT
%token <string> ID
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/

main:
  main SUB add_expression  { Minus($1,$3) }
  | add_expression         { $1 }
  | EOF { Done }
add_expression:
  add_expression ADD mult_expression { Plus($1,$3) }
  | mult_expression        { $1 }
andexpr:
  notexpr AND andexpr { Disjunction($1,$3) }
  |
const:
  LP subexpr RP { InParen($2) }
  | INT   { N($1) }
  | TRUE  { B(true) }
  | FALSE { B(false) }
  | ID    { Var($1) }
;
