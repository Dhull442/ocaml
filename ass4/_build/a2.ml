# 1 "a2.mll"
 
  open A3
  exception InvalidToken of char;;

# 7 "a2.ml"
let __ocaml_lex_tables = {
  Lexing.lex_base = 
   "\000\000\219\255\220\255\079\000\000\000\223\255\001\000\226\255\
    \228\255\002\000\001\000\007\000\000\000\009\000\237\255\238\255\
    \239\255\011\000\001\000\009\000\243\255\244\255\010\000\036\000\
    \247\255\248\255\249\255\250\255\000\000\163\000\247\000\066\001\
    \254\255\002\000\007\000\251\255\005\000\005\000\246\255\232\255\
    \038\000\245\255\023\000\242\255\241\255\240\255\230\255\236\255\
    \039\000\032\000\235\255\043\000\055\000\070\000\234\255\229\255\
    \233\255\000\000\056\000\231\255\076\000\067\000\224\255\225\255\
    \092\000\098\000\222\255";
  Lexing.lex_backtrk = 
   "\255\255\255\255\255\255\034\000\036\000\255\255\036\000\255\255\
    \255\255\036\000\036\000\036\000\036\000\036\000\255\255\255\255\
    \255\255\028\000\036\000\036\000\255\255\255\255\036\000\036\000\
    \255\255\255\255\255\255\255\255\036\000\003\000\002\000\001\000\
    \255\255\000\000\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_default = 
   "\001\000\000\000\000\000\255\255\255\255\000\000\255\255\000\000\
    \000\000\255\255\255\255\255\255\255\255\255\255\000\000\000\000\
    \000\000\255\255\255\255\255\255\000\000\000\000\255\255\255\255\
    \000\000\000\000\000\000\000\000\255\255\255\255\255\255\255\255\
    \000\000\255\255\255\255\000\000\255\255\255\255\000\000\000\000\
    \255\255\000\000\255\255\000\000\000\000\000\000\000\000\000\000\
    \255\255\255\255\000\000\255\255\255\255\255\255\000\000\000\000\
    \000\000\255\255\255\255\000\000\255\255\255\255\000\000\000\000\
    \255\255\255\255\000\000";
  Lexing.lex_trans = 
   "\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\033\000\033\000\033\000\033\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \033\000\000\000\033\000\000\000\000\000\000\000\000\000\000\000\
    \021\000\020\000\024\000\026\000\005\000\025\000\008\000\018\000\
    \032\000\031\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\045\000\007\000\014\000\016\000\015\000\000\000\
    \000\000\003\000\003\000\003\000\003\000\003\000\029\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\030\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\000\000\017\000\044\000\000\000\000\000\
    \000\000\028\000\034\000\060\000\023\000\011\000\010\000\058\000\
    \048\000\013\000\056\000\039\000\009\000\022\000\019\000\047\000\
    \004\000\057\000\064\000\052\000\012\000\051\000\003\000\046\000\
    \042\000\040\000\035\000\038\000\006\000\063\000\027\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\036\000\041\000\043\000\049\000\037\000\050\000\055\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\053\000\054\000\059\000\061\000\003\000\062\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\065\000\066\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\000\000\000\000\
    \002\000\000\000\003\000\000\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\000\000\000\000\000\000\000\000\003\000\000\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\000\000\000\000\000\000\000\000\
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
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000";
  Lexing.lex_check = 
   "\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\000\000\000\000\033\000\033\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \000\000\255\255\033\000\255\255\255\255\255\255\255\255\255\255\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\017\000\000\000\000\000\000\000\000\000\255\255\
    \255\255\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\000\
    \000\000\000\000\000\000\255\255\000\000\018\000\255\255\255\255\
    \255\255\000\000\028\000\057\000\000\000\000\000\000\000\009\000\
    \012\000\000\000\010\000\036\000\000\000\000\000\000\000\013\000\
    \000\000\009\000\004\000\011\000\000\000\011\000\003\000\013\000\
    \019\000\022\000\034\000\037\000\000\000\006\000\000\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\023\000\040\000\042\000\048\000\023\000\049\000\051\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\052\000\053\000\058\000\060\000\003\000\061\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\003\000\003\000\003\000\003\000\003\000\003\000\
    \003\000\003\000\029\000\064\000\065\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\255\255\255\255\
    \000\000\255\255\029\000\255\255\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\029\000\029\000\
    \029\000\029\000\029\000\029\000\029\000\029\000\030\000\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\255\255\255\255\255\255\255\255\030\000\255\255\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\030\000\030\000\030\000\030\000\030\000\030\000\
    \030\000\030\000\031\000\031\000\031\000\031\000\031\000\031\000\
    \031\000\031\000\031\000\031\000\255\255\255\255\255\255\255\255\
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
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\255\
    \255\255\255\255\255\255";
  Lexing.lex_base_code = 
   "";
  Lexing.lex_backtrk_code = 
   "";
  Lexing.lex_default_code = 
   "";
  Lexing.lex_trans_code = 
   "";
  Lexing.lex_check_code = 
   "";
  Lexing.lex_code = 
   "";
}

let rec read lexbuf =
    __ocaml_lex_read_rec lexbuf 0
and __ocaml_lex_read_rec lexbuf __ocaml_lex_state =
  match Lexing.engine __ocaml_lex_tables __ocaml_lex_state lexbuf with
      | 0 ->
# 23 "a2.mll"
                        ( read lexbuf )
# 208 "a2.ml"

  | 1 ->
let
# 24 "a2.mll"
             n
# 214 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 24 "a2.mll"
                        ( INT (int_of_string n) )
# 218 "a2.ml"

  | 2 ->
# 25 "a2.mll"
                        ( BOOL (true) )
# 223 "a2.ml"

  | 3 ->
# 26 "a2.mll"
                        ( BOOL (false) )
# 228 "a2.ml"

  | 4 ->
# 27 "a2.mll"
                        ( ABS )
# 233 "a2.ml"

  | 5 ->
# 28 "a2.mll"
                        ( TILDA )
# 238 "a2.ml"

  | 6 ->
# 29 "a2.mll"
                        ( PLUS )
# 243 "a2.ml"

  | 7 ->
# 30 "a2.mll"
                        ( MINUS )
# 248 "a2.ml"

  | 8 ->
# 31 "a2.mll"
                        ( TIMES )
# 253 "a2.ml"

  | 9 ->
# 33 "a2.mll"
                        ( DIV )
# 258 "a2.ml"

  | 10 ->
# 34 "a2.mll"
                        ( REM )
# 263 "a2.ml"

  | 11 ->
# 35 "a2.mll"
                        ( LP )
# 268 "a2.ml"

  | 12 ->
# 36 "a2.mll"
                        ( RP )
# 273 "a2.ml"

  | 13 ->
# 37 "a2.mll"
                        ( NOT )
# 278 "a2.ml"

  | 14 ->
# 38 "a2.mll"
                        ( DISJ )
# 283 "a2.ml"

  | 15 ->
# 39 "a2.mll"
                        ( CONJ )
# 288 "a2.ml"

  | 16 ->
# 42 "a2.mll"
                        ( EQ )
# 293 "a2.ml"

  | 17 ->
# 43 "a2.mll"
                        ( GT )
# 298 "a2.ml"

  | 18 ->
# 44 "a2.mll"
                        ( LT )
# 303 "a2.ml"

  | 19 ->
# 45 "a2.mll"
                        ( IF )
# 308 "a2.ml"

  | 20 ->
# 46 "a2.mll"
                        ( THEN )
# 313 "a2.ml"

  | 21 ->
# 47 "a2.mll"
                        ( ELSE )
# 318 "a2.ml"

  | 22 ->
# 48 "a2.mll"
                        ( FI )
# 323 "a2.ml"

  | 23 ->
# 49 "a2.mll"
                        ( DEF )
# 328 "a2.ml"

  | 24 ->
# 50 "a2.mll"
                        ( LET )
# 333 "a2.ml"

  | 25 ->
# 51 "a2.mll"
                        ( IN )
# 338 "a2.ml"

  | 26 ->
# 52 "a2.mll"
                        ( END )
# 343 "a2.ml"

  | 27 ->
# 53 "a2.mll"
                        ( DOT )
# 348 "a2.ml"

  | 28 ->
# 54 "a2.mll"
                        ( BACKSLASH )
# 353 "a2.ml"

  | 29 ->
# 55 "a2.mll"
                        ( SEMICOLON )
# 358 "a2.ml"

  | 30 ->
# 56 "a2.mll"
                        ( PARALLEL )
# 363 "a2.ml"

  | 31 ->
# 57 "a2.mll"
                        ( LOCAL )
# 368 "a2.ml"

  | 32 ->
# 58 "a2.mll"
                        ( COMMA )
# 373 "a2.ml"

  | 33 ->
# 59 "a2.mll"
                        ( PROJ )
# 378 "a2.ml"

  | 34 ->
let
# 60 "a2.mll"
              s
# 384 "a2.ml"
= Lexing.sub_lexeme lexbuf lexbuf.Lexing.lex_start_pos lexbuf.Lexing.lex_curr_pos in
# 60 "a2.mll"
                        ( ID s )
# 388 "a2.ml"

  | 35 ->
# 61 "a2.mll"
                        ( EOF )
# 393 "a2.ml"

  | 36 ->
let
# 62 "a2.mll"
       invalid
# 399 "a2.ml"
= Lexing.sub_lexeme_char lexbuf lexbuf.Lexing.lex_start_pos in
# 62 "a2.mll"
                        ( raise (InvalidToken invalid) )
# 403 "a2.ml"

  | __ocaml_lex_state -> lexbuf.Lexing.refill_buff lexbuf; 
      __ocaml_lex_read_rec lexbuf __ocaml_lex_state

;;

