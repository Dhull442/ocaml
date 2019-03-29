type token =
  | ABS
  | NEG
  | PLUS
  | MINUS
  | MUL
  | DIV
  | MOD
  | EXP
  | LP
  | RP
  | NOT
  | AND
  | OR
  | EQ
  | GTA
  | LTA
  | IF
  | THEN
  | ELSE
  | FI
  | DEF
  | DELIMITER
  | EOF
  | COMMA
  | PROJ
  | INT of (int)
  | BOOL of (bool)
  | ID of (string)

open Parsing;;
let _ = parse_error;;
# 2 "a3.mly"
    open A1
# 36 "a3.ml"
let yytransl_const = [|
  257 (* ABS *);
  258 (* NEG *);
  259 (* PLUS *);
  260 (* MINUS *);
  261 (* MUL *);
  262 (* DIV *);
  263 (* MOD *);
  264 (* EXP *);
  265 (* LP *);
  266 (* RP *);
  267 (* NOT *);
  268 (* AND *);
  269 (* OR *);
  270 (* EQ *);
  271 (* GTA *);
  272 (* LTA *);
  273 (* IF *);
  274 (* THEN *);
  275 (* ELSE *);
  276 (* FI *);
  277 (* DEF *);
  278 (* DELIMITER *);
    0 (* EOF *);
  279 (* COMMA *);
  280 (* PROJ *);
    0|]

let yytransl_block = [|
  281 (* INT *);
  282 (* BOOL *);
  283 (* ID *);
    0|]

let yylhs = "\255\255\
\001\000\001\000\002\000\002\000\003\000\003\000\004\000\004\000\
\004\000\004\000\005\000\005\000\005\000\006\000\006\000\006\000\
\006\000\007\000\007\000\008\000\008\000\009\000\009\000\010\000\
\010\000\012\000\012\000\014\000\014\000\015\000\015\000\015\000\
\011\000\013\000\013\000\000\000"

let yylen = "\002\000\
\003\000\001\000\003\000\001\000\002\000\001\000\003\000\003\000\
\003\000\001\000\003\000\003\000\001\000\003\000\003\000\003\000\
\001\000\002\000\001\000\002\000\001\000\007\000\001\000\007\000\
\001\000\003\000\001\000\003\000\001\000\001\000\001\000\001\000\
\001\000\003\000\003\000\002\000"

let yydefred = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\032\000\030\000\031\000\000\000\000\000\004\000\006\000\000\000\
\000\000\017\000\019\000\021\000\023\000\025\000\027\000\029\000\
\018\000\020\000\000\000\000\000\005\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\028\000\000\000\026\000\000\000\000\000\033\000\000\000\
\000\000\003\000\000\000\000\000\000\000\000\000\000\000\014\000\
\015\000\016\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\022\000\000\000"

let yydgoto = "\002\000\
\012\000\013\000\014\000\015\000\016\000\017\000\018\000\019\000\
\020\000\021\000\048\000\022\000\028\000\023\000\024\000"

let yysindex = "\009\000\
\002\255\000\000\040\255\027\255\002\255\006\255\002\255\003\255\
\000\000\000\000\000\000\005\255\012\255\000\000\000\000\031\255\
\067\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\059\255\251\254\000\000\038\255\023\255\002\255\
\002\255\006\255\006\255\006\255\006\255\006\255\006\255\006\255\
\006\255\000\000\002\255\000\000\002\255\002\255\000\000\016\255\
\012\255\000\000\067\255\067\255\017\255\017\255\017\255\000\000\
\000\000\000\000\005\255\005\255\253\254\023\255\002\255\053\255\
\250\254\002\255\000\000\005\255"

let yyrindex = "\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\070\000\001\000\000\000\000\000\173\000\
\118\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\022\000\000\000\136\000\154\000\062\000\081\000\100\000\000\000\
\000\000\000\000\014\255\045\255\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\043\000"

let yygindex = "\000\000\
\251\255\046\000\047\000\073\000\039\000\025\000\044\000\083\000\
\084\000\000\000\027\000\000\000\000\000\000\000\000\000"

let yytablesize = 452
let yytable = "\027\000\
\002\000\030\000\003\000\004\000\044\000\032\000\003\000\004\000\
\032\000\001\000\005\000\031\000\006\000\067\000\005\000\063\000\
\032\000\045\000\007\000\034\000\035\000\001\000\007\000\035\000\
\033\000\008\000\009\000\010\000\011\000\008\000\009\000\010\000\
\011\000\034\000\035\000\005\000\035\000\059\000\062\000\060\000\
\061\000\004\000\024\000\007\000\036\000\037\000\038\000\047\000\
\005\000\032\000\008\000\009\000\010\000\011\000\034\000\046\000\
\007\000\065\000\051\000\052\000\068\000\007\000\066\000\008\000\
\009\000\010\000\011\000\034\000\042\000\036\000\032\000\039\000\
\040\000\041\000\053\000\054\000\055\000\049\000\029\000\050\000\
\008\000\043\000\056\000\057\000\058\000\025\000\000\000\026\000\
\064\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\009\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\013\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\012\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\011\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\010\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
\000\000\000\000\000\000\002\000\002\000\002\000\002\000\002\000\
\000\000\000\000\002\000\000\000\002\000\000\000\002\000\002\000\
\002\000\000\000\002\000\002\000\002\000\000\000\000\000\002\000\
\001\000\001\000\001\000\001\000\001\000\000\000\000\000\001\000\
\000\000\001\000\000\000\001\000\001\000\001\000\000\000\001\000\
\001\000\001\000\000\000\000\000\001\000\024\000\024\000\024\000\
\024\000\024\000\000\000\000\000\024\000\000\000\000\000\024\000\
\024\000\024\000\024\000\000\000\024\000\024\000\024\000\000\000\
\000\000\024\000\007\000\007\000\007\000\000\000\000\000\007\000\
\000\000\007\000\007\000\007\000\007\000\007\000\000\000\007\000\
\007\000\007\000\000\000\000\000\007\000\008\000\008\000\008\000\
\000\000\000\000\008\000\000\000\008\000\008\000\008\000\008\000\
\008\000\000\000\008\000\008\000\008\000\000\000\000\000\008\000\
\009\000\009\000\009\000\000\000\000\000\009\000\000\000\009\000\
\009\000\009\000\009\000\009\000\000\000\009\000\009\000\009\000\
\013\000\013\000\009\000\000\000\000\000\000\000\000\000\013\000\
\000\000\013\000\013\000\013\000\013\000\013\000\000\000\013\000\
\013\000\013\000\012\000\012\000\013\000\000\000\000\000\000\000\
\000\000\012\000\000\000\012\000\012\000\012\000\012\000\012\000\
\000\000\012\000\012\000\012\000\011\000\011\000\012\000\000\000\
\000\000\000\000\000\000\011\000\000\000\011\000\011\000\011\000\
\011\000\011\000\000\000\011\000\011\000\011\000\000\000\000\000\
\011\000\010\000\010\000\010\000\000\000\000\000\010\000\000\000\
\010\000\010\000\000\000\000\000\000\000\000\000\010\000\010\000\
\010\000\000\000\000\000\010\000"

let yycheck = "\005\000\
\000\000\007\000\001\001\002\001\010\001\012\001\001\001\002\001\
\012\001\001\000\009\001\009\001\011\001\020\001\009\001\019\001\
\012\001\023\001\017\001\003\001\004\001\000\000\017\001\010\001\
\013\001\024\001\025\001\026\001\027\001\024\001\025\001\026\001\
\027\001\003\001\004\001\009\001\023\001\043\000\023\001\045\000\
\046\000\002\001\000\000\017\001\014\001\015\001\016\001\025\001\
\009\001\012\001\024\001\025\001\026\001\027\001\010\001\018\001\
\017\001\063\000\034\000\035\000\066\000\000\000\010\001\024\001\
\025\001\026\001\027\001\023\001\010\001\000\000\012\001\005\001\
\006\001\007\001\036\000\037\000\038\000\032\000\006\000\033\000\
\000\000\023\001\039\000\040\000\041\000\003\000\255\255\004\000\
\062\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\000\000\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\000\000\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\000\000\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\000\000\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
\255\255\255\255\255\255\003\001\004\001\005\001\006\001\007\001\
\255\255\255\255\010\001\255\255\012\001\255\255\014\001\015\001\
\016\001\255\255\018\001\019\001\020\001\255\255\255\255\023\001\
\003\001\004\001\005\001\006\001\007\001\255\255\255\255\010\001\
\255\255\012\001\255\255\014\001\015\001\016\001\255\255\018\001\
\019\001\020\001\255\255\255\255\023\001\003\001\004\001\005\001\
\006\001\007\001\255\255\255\255\010\001\255\255\255\255\013\001\
\014\001\015\001\016\001\255\255\018\001\019\001\020\001\255\255\
\255\255\023\001\005\001\006\001\007\001\255\255\255\255\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\020\001\255\255\255\255\023\001\005\001\006\001\007\001\
\255\255\255\255\010\001\255\255\012\001\013\001\014\001\015\001\
\016\001\255\255\018\001\019\001\020\001\255\255\255\255\023\001\
\005\001\006\001\007\001\255\255\255\255\010\001\255\255\012\001\
\013\001\014\001\015\001\016\001\255\255\018\001\019\001\020\001\
\003\001\004\001\023\001\255\255\255\255\255\255\255\255\010\001\
\255\255\012\001\013\001\014\001\015\001\016\001\255\255\018\001\
\019\001\020\001\003\001\004\001\023\001\255\255\255\255\255\255\
\255\255\010\001\255\255\012\001\013\001\014\001\015\001\016\001\
\255\255\018\001\019\001\020\001\003\001\004\001\023\001\255\255\
\255\255\255\255\255\255\010\001\255\255\012\001\013\001\014\001\
\015\001\016\001\255\255\018\001\019\001\020\001\255\255\255\255\
\023\001\005\001\006\001\007\001\255\255\255\255\010\001\255\255\
\012\001\013\001\255\255\255\255\255\255\255\255\018\001\019\001\
\020\001\255\255\255\255\023\001"

let yynames_const = "\
  ABS\000\
  NEG\000\
  PLUS\000\
  MINUS\000\
  MUL\000\
  DIV\000\
  MOD\000\
  EXP\000\
  LP\000\
  RP\000\
  NOT\000\
  AND\000\
  OR\000\
  EQ\000\
  GTA\000\
  LTA\000\
  IF\000\
  THEN\000\
  ELSE\000\
  FI\000\
  DEF\000\
  DELIMITER\000\
  EOF\000\
  COMMA\000\
  PROJ\000\
  "

let yynames_block = "\
  INT\000\
  BOOL\000\
  ID\000\
  "

let yyact = [|
  (fun _ -> failwith "parser")
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : A1.exptree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 18 "a3.mly"
                                          ( Disjunction(_1,_3) )
# 287 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'or_expression) in
    Obj.repr(
# 19 "a3.mly"
                                           ( _1 )
# 294 "a3.ml"
               : A1.exptree))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'or_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'not) in
    Obj.repr(
# 22 "a3.mly"
                                    ( Conjunction(_1,_3) )
# 302 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'not) in
    Obj.repr(
# 23 "a3.mly"
                                      ( _1 )
# 309 "a3.ml"
               : 'or_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'comp) in
    Obj.repr(
# 26 "a3.mly"
                                   ( Not(_2) )
# 316 "a3.ml"
               : 'not))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'comp) in
    Obj.repr(
# 27 "a3.mly"
                                           ( _1 )
# 323 "a3.ml"
               : 'not))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith) in
    Obj.repr(
# 29 "a3.mly"
                                         ( Equals(_1,_3) )
# 331 "a3.ml"
               : 'comp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith) in
    Obj.repr(
# 30 "a3.mly"
                                          ( GreaterT(_1,_3) )
# 339 "a3.ml"
               : 'comp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'arith) in
    Obj.repr(
# 31 "a3.mly"
                                          ( LessT(_1,_3) )
# 347 "a3.ml"
               : 'comp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'arith) in
    Obj.repr(
# 32 "a3.mly"
                                            ( _1 )
# 354 "a3.ml"
               : 'comp))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 34 "a3.mly"
                                             ( Minus(_1,_3) )
# 362 "a3.ml"
               : 'arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'arith) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 35 "a3.mly"
                                            ( Plus(_1,_3) )
# 370 "a3.ml"
               : 'arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'mult_expression) in
    Obj.repr(
# 36 "a3.mly"
                                             ( _1 )
# 377 "a3.ml"
               : 'arith))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 39 "a3.mly"
                                 ( Mult(_1,_3) )
# 385 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 40 "a3.mly"
                                 ( Div(_1,_3) )
# 393 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'mult_expression) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 41 "a3.mly"
                                       ( Rem(_1,_3) )
# 401 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'abs) in
    Obj.repr(
# 42 "a3.mly"
                                 ( _1 )
# 408 "a3.ml"
               : 'mult_expression))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'neg) in
    Obj.repr(
# 45 "a3.mly"
                ( Abs(_2) )
# 415 "a3.ml"
               : 'abs))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'neg) in
    Obj.repr(
# 46 "a3.mly"
          (_1)
# 422 "a3.ml"
               : 'abs))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 0 : 'ifte) in
    Obj.repr(
# 48 "a3.mly"
                      ( Negative(_2) )
# 429 "a3.ml"
               : 'neg))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'ifte) in
    Obj.repr(
# 49 "a3.mly"
             (_1)
# 436 "a3.ml"
               : 'neg))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 5 : A1.exptree) in
    let _4 = (Parsing.peek_val __caml_parser_env 3 : A1.exptree) in
    let _6 = (Parsing.peek_val __caml_parser_env 1 : A1.exptree) in
    Obj.repr(
# 51 "a3.mly"
                                              ( IfThenElse(_2,_4,_6) )
# 445 "a3.ml"
               : 'ifte))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'proj) in
    Obj.repr(
# 52 "a3.mly"
                (_1)
# 452 "a3.ml"
               : 'ifte))
; (fun __caml_parser_env ->
    let _3 = (Parsing.peek_val __caml_parser_env 4 : 'int) in
    let _5 = (Parsing.peek_val __caml_parser_env 2 : 'int) in
    let _7 = (Parsing.peek_val __caml_parser_env 0 : A1.exptree) in
    Obj.repr(
# 54 "a3.mly"
                                       ( Project((_3,_5),_7) )
# 461 "a3.ml"
               : 'proj))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'tup) in
    Obj.repr(
# 55 "a3.mly"
          (_1)
# 468 "a3.ml"
               : 'proj))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : 'tuplelist) in
    Obj.repr(
# 57 "a3.mly"
                                ( Tuple(List.length _2 , _2) )
# 475 "a3.ml"
               : 'tup))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'paren) in
    Obj.repr(
# 58 "a3.mly"
            (_1)
# 482 "a3.ml"
               : 'tup))
; (fun __caml_parser_env ->
    let _2 = (Parsing.peek_val __caml_parser_env 1 : A1.exptree) in
    Obj.repr(
# 60 "a3.mly"
                                        ( InParen(_2) )
# 489 "a3.ml"
               : 'paren))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : 'constant) in
    Obj.repr(
# 61 "a3.mly"
                      ( _1 )
# 496 "a3.ml"
               : 'paren))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : bool) in
    Obj.repr(
# 63 "a3.mly"
                                           ( B(_1) )
# 503 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : string) in
    Obj.repr(
# 64 "a3.mly"
                                            ( Var(_1) )
# 510 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 65 "a3.mly"
                                           ( N(_1) )
# 517 "a3.ml"
               : 'constant))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 0 : int) in
    Obj.repr(
# 68 "a3.mly"
                                          ( _1 )
# 524 "a3.ml"
               : 'int))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : 'tuplelist) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : A1.exptree) in
    Obj.repr(
# 71 "a3.mly"
                                                ( _1 @ [_3] )
# 532 "a3.ml"
               : 'tuplelist))
; (fun __caml_parser_env ->
    let _1 = (Parsing.peek_val __caml_parser_env 2 : A1.exptree) in
    let _3 = (Parsing.peek_val __caml_parser_env 0 : A1.exptree) in
    Obj.repr(
# 72 "a3.mly"
                                                  ( [_1;_3] )
# 540 "a3.ml"
               : 'tuplelist))
(* Entry main *)
; (fun __caml_parser_env -> raise (Parsing.YYexit (Parsing.peek_val __caml_parser_env 0)))
|]
let yytables =
  { Parsing.actions=yyact;
    Parsing.transl_const=yytransl_const;
    Parsing.transl_block=yytransl_block;
    Parsing.lhs=yylhs;
    Parsing.len=yylen;
    Parsing.defred=yydefred;
    Parsing.dgoto=yydgoto;
    Parsing.sindex=yysindex;
    Parsing.rindex=yyrindex;
    Parsing.gindex=yygindex;
    Parsing.tablesize=yytablesize;
    Parsing.table=yytable;
    Parsing.check=yycheck;
    Parsing.error_function=parse_error;
    Parsing.names_const=yynames_const;
    Parsing.names_block=yynames_block }
let main (lexfun : Lexing.lexbuf -> token) (lexbuf : Lexing.lexbuf) =
   (Parsing.yyparse yytables 1 lexfun lexbuf : A1.exptree)
