%{
    open Evaluator
%}

/* Tokens are defined below.  */
%token <int> INT
%token <string> ID
%token EQ LP RP COLON EOF TINT TUNIT SEMICOLON PVAR PCALL PPROCEDURE EOL PS
%start main
%type <Evaluator.expr> main /* Returns expression */
%%

main:
    assign EOL                       { $1 }
    | SEMICOLON SEMICOLON EOL            { RET }
    | PS                 EOL             { ViewStack }
;
assign:
    ID COLON EQ constant                         { ASSIGN ($1,$4) }
    | proc                                    { $1 }
;

proc:
      PCALL ID LP exprlist RP                                  { CALL ($2,$4) }
    | PPROCEDURE ID LP vbleslist RP COLON                { DEFINE (P($2,$4)) }
    | dcl                               { $1 }
;

vbleslist:
    vbleslist SEMICOLON vble              { $1 @ [$3] }
    | vble                                { [$1] }
;

vble:
    ID COLON type             { VAR ($1,$3) }
;

type:
    TINT                      { Tint }
    | TUNIT                   { Tunit }
;

dcl:
    PVAR vbleslist SEMICOLON         { DCL ($2) }
    | constant                      { $1 }
;
exprlist:
  exprlist SEMICOLON constant           { $1 @ [@3] }
| constant                              { [$1] }
constant:
   INT                                  { N($1) }
   | ID                                 { V($1) }
;
