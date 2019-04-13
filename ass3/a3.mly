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
    main AND or_expression                { Disjunction($1,$3) }
    | or_expression                        { $1 }
;
or_expression:
    or_expression OR not            { Conjunction($1,$3) }
    | not                             { $1 }
;
not:
    NOT comp                       { Not($2) }
    | comp                                 { $1 }
comp:
 comp EQ arith                          { Equals($1,$3) }
| comp GTA arith                         { GreaterT($1,$3) }
| comp LTA arith                         { LessT($1,$3) }
| arith                                     { $1 }
arith:
    arith MINUS mult_expression              { Sub($1,$3) }
    | arith PLUS mult_expression            { Add($1,$3) }
    | mult_expression                        { $1 }
;
mult_expression:
    mult_expression MUL abs      { Mult($1,$3) }
    | mult_expression DIV abs    { Div($1,$3) }
    | mult_expression MOD abs          { Rem($1,$3) }
    | abs                        { $1 }
;
abs:
    ABS neg     { Abs($2) }
    | neg {$1}
neg:
    NEG ifte          { Negative($2) }
    | ifte   {$1}
ifte:
      IF main THEN main ELSE main FI          { IfThenElse($2,$4,$6) }
    | proj      {$1}
proj:
  PROJ LP int COMMA int RP tup        { Project(($3,$5),$7) }
  | tup   {$1}
tup:
  LP tuplelist RP               { Tuple(List.length $2 , $2) }
|  paren    {$1}
paren:
  LP main RP                            { InParen($2) }
  | constant          { $1 }
constant:
     BOOL                                  { B($1) }
    | ID                                    { Var($1) }
    | INT                                  { N($1) }
;
int:
    INT                                   { $1 }
;
tuplelist:
      tuplelist COMMA main                      { $1 @ [$3] }
    | main COMMA main                             { [$1;$3] }
;
