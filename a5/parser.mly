%{
    open Types
%}

/* Tokens are defined below.  */
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token PNOT PPLUS PMINUS TIMES PDIV PREM CONJ DISJ GTA LTA EQ LP RP IF THEN ELSE FI COLON BACKSLASH DOT PIPE EOF TINT TUNIT TBOOL PLET IN END SEMICOLON PDEF PCMP PREC
%start exp_parser def_parser
%type <Types.definition> def_parser /* Returns definitions */
%type <Types.expr> exp_parser /* Returns expression */
%%

exp_parser:
      exp_parser DISJ or_expression                 { And($1,$3) }
    | or_expression                                 { $1 }
;

or_expression:
      or_expression CONJ not                        { Or($1,$3) }
    | not                                           { $1 }
;

not:
      PNOT comp                                     { Not($2) }
    | comp                                          { $1 }

comp:
      comp EQ arith                                 { Equals($1,$3) }
    | comp GTA arith                                { GreaterT($1,$3) }
    | comp LTA arith                                { LessT($1,$3) }
    | comp GTA EQ arith                             { GreaterTE($1,$4) }
    | comp LTA EQ arith                             { LessTE($1,$4) }
    | PCMP arith PCMP                               { Cmp($2) }
    | arith                                         { $1 }

arith:
    arith PMINUS mult_expression                    { Minus($1,$3) }
    | arith PPLUS mult_expression                   { Plus($1,$3) }
    | mult_expression                               { $1 }
;

mult_expression:
      mult_expression TIMES ifte                    { Mult($1,$3) }
    | mult_expression PDIV ifte                     { Div($1,$3) }
    | mult_expression PREM ifte                     { Rem($1,$3) }
    | ifte                                          { $1 }
;

ifte:
      IF exp_parser THEN exp_parser ELSE exp_parser FI  { If_Then_Else($2,$4,$6) }
    | func                                          { $1 }
;

func:
      funabs paren                                  { App($1,$2) }
    | funabs                                        { $1 }
;

funabs:
      BACKSLASH constant DOT LP exp_parser RP       { Lambda($2,$5) }
    | BACKSLASH constant DOT letp                   { Lambda($2,$4) }
    | PREC BACKSLASH constant DOT LP exp_parser RP  { RecLambda($3,$6) }
    | PREC BACKSLASH constant DOT letp              { RecLambda($3,$5) }
    | paren                                         { $1 }
;

paren:
      LP exp_parser RP                              { InParen($2) }
    | letp                                          { $1 }
;

letp:
      PLET def_parser IN exp_parser END             { Let ($2,$4) }
    | constant                                      { $1 }
;

constant:
      BOOL                                          { Bool($1) }
    | ID                                            { V($1) }
    | INT                                           { Integer($1) }
;

def_parser:
      def                                           { $1 }
;
def:
      PDEF ID EQ exp_parser                         { Simple($2,$4) }
;
