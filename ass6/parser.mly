%{
    open Main
%}

/* Tokens are defined below.  */
%token <int> INT
%token <string> ID
%token EQ COLON EOL PCALL COMMA PRET PVIEW LP RP VIEWP
%start comd
%type <Main.commands> comd /* Returns definitions */
%type <Main.expr> value
%%

comd:
  main EOL                      { $1 }
;
main:
  ID COLON EQ value             { ASSIGN ($1,$4) }
  | PCALL ID LP valuelist RP      { CALL ($2,$4) }
  | PRET                        { RETURN }
  | PVIEW                       { VIEW }
  | VIEWP                       { VIEWR }
;
valuelist:
  valuelist COMMA value          {  $1@[$3] }
  | value                       { [$1] }
;
value:
  INT                         { N ($1) }
  | ID                        { V ($1) }
;
