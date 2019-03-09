%{
    open A1
%}

/* Tokens are defined below.  */
%token TRUE FALSE ABS NEG PLUS MINUS MUL DIV MOD EXP LP RP NOT AND OR EQ GTA LTA GEQ LEQ IF THEN ELSE FI DEF DELIMITER EOF COMMA PROJ
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
    bool                                    { $1 }
    | arith                                 { $1 }
    | EOF                                   { Done }
;
bool:
    IF bool THEN main ELSE main FI          { IfThenElse($2,$4,$6) }
    | bool OR and_expression                { Conjunction($1,$3) }
    | and_expression                        { $1 }
;
and_expression:
    and_expression AND constantB            { Disjunction($1,$3) }
    | constantB                             { $1 }
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
    |  constantN                            { $1 }
;
constantN:
    ABS constantN                           { Abs($2) }
    | NEG constantN                         { Negative($2) }
    | LP main RP                            { InParen($2) }
    | ID                                    { Var($1) }      /* To be interpreted as a variable name with string as tokenised */
    | INT                                   { N($1) }      /* To be interpreted as an integer with its value as tokenised   */
    | LP int COMMA tuple RP                 { Tuple($2,$4) }
    | PROJ LP int COMMA int RP main         { Project(($3,$5),$7) }
;
int:
    | INT                                   { $1 }
tuple:
    tuple COMMA main                      { $1 @[$3] }
    | main                                { [$1] }

constantB:
    NOT constantB                           { Not($2) }
    | LP main RP                            { InParen($2) }
    | TRUE                                  { B(true) }
    | FALSE                                 { B(false) }
    | main EQ  main                         { Equals($1,$3) }
    | main GEQ main                         { GreaterTE($1,$3) }
    | main GTA main                         { GreaterT($1,$3) }
    | main LTA main                         { LessT($1,$3) }
    | main LEQ main                         { LessTE($1,$3) }
;
