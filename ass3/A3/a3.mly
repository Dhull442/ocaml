%{
    open A1
%}

/* Tokens are defined below.  */
%token TRUE FALSE ABS PLUS MINUS MUL DIV MOD EXP LP RP NOT AND OR EQ GTA LTA GEQ LEQ IF THEN ELSE DEF DELIMITER EOF COMMA PROJ
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
  INT   { N($1) }
  | EOF { Done }
;
