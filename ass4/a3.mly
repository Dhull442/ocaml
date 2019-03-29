%{
    open A1
%}

/*
- Tokens (token name and rules) are modified wrt to A2. Please make necessary changes in A3
- LP and RP are left and right parenthesis
- Write grammar rules to recognize
  - >= <= from GT EQ LT tokens
  - if then else fi
*/
/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token ABS TILDA NOT PLUS MINUS TIMES DIV REM CONJ DISJ EQ GT LT LP RP IF THEN ELSE FI COMMA PROJ
LET IN END BACKSLASH DOT DEF SEMICOLON PARALLEL LOCAL EOF
%start main mdef
%type <A1.definition> mdef /* Returns definitions */
%type <A1.exptree> main /* Returns expression */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/
main:
    | main DISJ or_expression                { Disjunction($1,$3) }
    | or_expression                        { $1 }
;
or_expression:
    or_expression CONJ not            { Conjunction($1,$3) }
    | not                             { $1 }
;
not:
    NOT comp                       { Not($2) }
    | comp                                 { $1 }
comp:
 comp EQ arith                          { Equals($1,$3) }
| comp GT arith                         { GreaterT($1,$3) }
| comp LT arith                         { LessT($1,$3) }
| arith                                     { $1 }
arith:
    arith MINUS mult_expression              { Sub($1,$3) }
    | arith PLUS mult_expression            { Add($1,$3) }
    | mult_expression                        { $1 }
;
mult_expression:
    mult_expression TIMES abs      { Mult($1,$3) }
    | mult_expression DIV abs    { Div($1,$3) }
    | mult_expression REM abs          { Rem($1,$3) }
    | abs                        { $1 }
;
abs:
    ABS neg                           { Abs($2) }
    | neg                             { $1 }
neg:
    TILDA ifte                        { Negative($2) }
    | ifte                            { $1 }
ifte:
      IF main THEN main ELSE main FI  { IfThenElse($2,$4,$6) }
    | proj                            { $1 }
proj:
  PROJ LP int COMMA int RP tup       { Project(($3,$5),$7) }
  | tup                               { $1 }
tup:
  LP tuplelist RP                     { Tuple(List.length $2 , $2) }
|  func                              { $1 }
;
func:
  BACKSLASH ID DOT paren               { FunctionAbstraction ($2,$4) }
  | paren LP main RP                    { FunctionCall ($1,$3) }
  | paren                         { $1 }
;
paren:
  LP main RP                          { InParen($2) }
  | constant                                 { $1 }
  | LET mdef IN main END              { Let ($2,$4) }
;
constant:
     BOOL                             { B($1) }
    | ID                              { Var($1) }
    | INT                             { N($1) }
;
int:
    INT                                   { $1 }
;
tuplelist:
      tuplelist COMMA main                        { $1 @ [$3] }
    | main COMMA main                             { [$1;$3] }
;


mdef:
   LOCAL mdef IN mdef END             { Local ($2,$4) }
  | mdef SEMICOLON def                { (Sequence [$1;$3]) }
  | mdef PARALLEL def                { (Parallel [$1;$3]) }
  | def                               { $1 }
;
def:
  DEF ID EQ main                      { Simple($2,$4) }
;
