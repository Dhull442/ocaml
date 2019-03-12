%{
    open A1
%}

/* Tokens are defined below.  */
%token ABS NEG PLUS MINUS MUL DIV MOD EXP LP RP NOT AND OR EQ GTA LTA IF THEN ELSE FI DEF DELIMITER EOF COMMA PROJ
%token <int> INT
%token <bool> BOOL
%token <string> ID
%start main
%type <A1.exptree> main /* Return type */
%%
/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/
main:
    main OR and_expression                { Conjunction($1,$3) }
    | and_expression                        { $1 }
;
ifte:
    IF main THEN main ELSE main FI          { IfThenElse($2,$4,$6) }
;
and_expression:
    and_expression AND arith            { Disjunction($1,$3) }
    | arith                             { $1 }
;
arith:
    arith MINUS add_expression              { Minus($1,$3) }
    | add_expression                        { $1 }
;
add_expression:
    add_expression PLUS mult_expression     { Plus($1,$3) } /* Created a tree with PLUS at root and two subtrees corresponding to left: add_expression and right: mult_expression */
    | mult_expression                       { $1 }
;
mult_expression:
    mult_expression MUL div_expression      { Mult($1,$3) }
    | div_expression                        { $1 }
;
div_expression:
    div_expression DIV rem_expression       { Div($1,$3) }
    | rem_expression                        { $1 }
;
rem_expression:
    rem_expression MOD constantN            { Rem($1,$3) }
    | constantN                            { $1 }
;
constantN:

     BOOL                                  { B($1) }
    | ID                                    { Var($1) }
    | INT                                  { N($1) }
    | ABS constantN                           { Abs($2) }
    | NEG constantN                         { Negative($2) }
    | LP main RP                            { InParen($2) }
    | PROJ LP int COMMA int RP tuple         { Project(($3,$5),$7) }
    | NOT constantN                           { Not($2) }
    | sub EQ sub                          { Equals($1,$3) }
    | sub GTA sub                         { GreaterT($1,$3) }
    | sub LTA sub                         { LessT($1,$3) }
    | ifte                                     { $1 }
;
sub:
  INT                                  { N($1) }
  | LP main RP                            { InParen($2) }
int:
    INT                                   { $1 }
;
tuple:
    LP int COMMA tuplelist RP                 { Tuple($2,$4) }
    | ID {Var($1)}
tuplelist:
      tuplelist COMMA main                      { $1 @ [$3] }
    | main                                { [$1] }
;
