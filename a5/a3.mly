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
%token PNOT PPLUS PMINUS TIMES PDIV PREM CONJ DISJ GTA LTA EQ LP RP IF THEN ELSE FI COLON BACKSLASH DOT PIPE EOF TINT TUNIT TBOOL PLET IN END SEMICOLON PDEF PCMP
%start exp_parser
%type <A1.definition> def_parser /* Returns definitions */
%type <A1.expr> exp_parser /* Returns expression */
%type <A1.exptype> typefunc /* Returns type */
%%
/* The grammars written below are dummy. Please rewrite it as per the specifications. */

/*
DESIGN a grammar for a simple expression language, taking care to enforce precedence rules (e.g., BODMAS)
The language should contain the following types of expressions:  integers and booleans.
*/
exp_parser:
    | exp_parser DISJ or_expression                { And($1,$3) }
    | or_expression                        { $1 }
;
or_expression:
    or_expression CONJ not            { Or($1,$3) }
    | not                             { $1 }
;
not:
    PNOT comp                       { Not($2) }
    | comp                                 { $1 }
comp:
 comp EQ arith                          { Equals($1,$3) }
| comp GTA arith                         { GreaterT($1,$3) }
| comp LTA arith                         { LessT($1,$3) }
| comp GTA EQ arith                      { GreaterTE($1,$4) }
| comp LTA EQ arith                      { LessTE($1,$4) }
| PCMP arith PCMP                         { Cmp($2) }
| arith                                     { $1 }
arith:
    arith PMINUS mult_expression              { Minus($1,$3) }
    | arith PPLUS mult_expression            { Plus($1,$3) }
    | mult_expression                        { $1 }
;
mult_expression:
    mult_expression TIMES ifte     { Mult($1,$3) }
    | mult_expression PDIV ifte    { Div($1,$3) }
    | mult_expression PREM ifte          { Rem($1,$3) }
    | ifte                        { $1 }
;
/* abs:
    ABS neg                           { Abs($2) }
    | neg                             { $1 }
neg:
    TILDA ifte                        { Negative($2) }
    | ifte                            { $1 } */
ifte:
      IF exp_parser THEN exp_parser ELSE exp_parser FI  { If_Then_Else($2,$4,$6) }
    | func                            { $1 }
/* proj:
  PROJ LP int COMMA int RP func       { Project(($3,$5),$7) }
  | func                               { $1 } */
func:
   funabs LP exp_parser RP                    { App($1,$3) }
  | funabs                         { $1 }
;
funabs:
  BACKSLASH constant COLON typefunc DOT paren               { Lambda($2,$6) }
  | paren                                               { $1 }
paren:
  LP exp_parser RP                          { InParen($2) }
  /* | LP tuplelist RP                     { Tuple(List.length $2 , $2) } */
  | constant                                 { $1 }
  | PLET def_parser IN exp_parser END              { Let ($2,$4) }
;
constant:
     BOOL                             { Bool($1) }
    | ID                              { V($1) }
    | INT                             { Integer($1) }
;
/* int:
    INT                                   { $1 }
; */
/* tuplelist:
      tuplelist COMMA exp_parser                        { $1 @ [$3] }
    | exp_parser COMMA exp_parser                             { [$1;$3] } */
/* ; */

typelist:
    typelist TIMES typefunc               { $1 @ [$3] }
    | typefunc                            { [$1] }
;
typefunc:
    typefunc PMINUS GTA simpletype      { Tfunc ($1,$4) }
    | simpletype                         { $1 }
;
simpletype:
    TINT                                { Tint }
    | TBOOL                             { Tbool }
    | TUNIT                             { Tunit }
    /* | LP typelist RP            { Ttuple($2) } */
;
def_parser:
   /* LOCAL def_parser IN def_parser END             { Local ($2,$4) } */
  def_parser SEMICOLON def                { (Sequence [$1;$3]) }
  | def_parser PIPE def                { (Parallel [$1;$3]) }
  | def                               { $1 }
;
def:
  PDEF ID EQ exp_parser                      { Simple($2,$4) }
;
