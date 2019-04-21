open Types

let rec compile t =
  let rec compiledef d = match d with
  Simple (str,e) -> (compile e) @ [DEF str]
  in
  match t with
  V str -> [ VAR str ]
| Integer i -> [ NCONST i ]
| Bool b -> [ BCONST b ]
| Plus (a,b) -> (compile a) @ (compile b) @ [PLUS]
| Minus (a,b) -> (compile a) @ (compile b) @ [MINUS]
| Mult (a,b) -> (compile a) @ (compile b) @ [MULT]
| Div (a,b) -> (compile a) @ (compile b) @ [DIV]
| Rem (a,b) -> (compile a) @ (compile b) @ [REM]
| And (a,b) -> (compile a) @ (compile b) @ [AND]
| Or (a,b) -> (compile a) @ (compile b) @ [OR]
| Equals (a,b) -> (compile a) @ (compile b) @ [EQS]
| GreaterTE (a,b) -> (compile a) @ (compile b) @ [GTE]
| GreaterT (a,b) -> (compile a) @ (compile b) @ [GT]
| LessTE (a,b) -> (compile a) @ (compile b) @ [LTE]
| LessT (a,b) -> (compile a) @ (compile b) @ [LT]
| InParen (a) -> (compile a) @ [PAREN]
| Not a -> (compile a) @ [NOT]
| Cmp a -> (compile a) @ [CMP]
| If_Then_Else (a,b,c) -> (compile a) @ (compile b) @ (compile c) @ [IFTE]
| App (a,b) -> (compile a) @ (compile b) @ [FCALL]
| Lambda(a,b) -> (match a with V str -> [CLOS (str,(compile b) @ [RET])] | _ -> raise (Error "Compile Fail: lambda"))
| RecLambda(a,b) -> (match a with V str -> [CLOS (str,(compile b) @ [RET])] | _ -> raise (Error "Compile Fail: reclambda"))
| Let (d,e) -> [ LET ((compiledef d) @ (compile e) @ [RET]) ]
(* | _ -> raise (Error "Not Implemented") *)
;;

let rec secdmc s e c d =
  match c with
  [] -> List.hd s
| VAR str :: cs -> secdmc ((find str e)::s) e cs d
| NCONST i :: cs -> secdmc ((N i)::s) e cs d
| BCONST b :: cs -> secdmc ((B b)::s) e cs d
| NOT :: cs ->( match s with B b :: ss -> secdmc ((B (not b))::ss) e cs d | _ -> raise (Error "stackpop != bool"))
| CMP :: cs ->( match s with N i :: ss -> secdmc ((B (i > 0))::ss) e cs d | _ -> raise (Error "stackpop != int"))
| EQS :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((B (i = ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| GT :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((B (i > ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| LT :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((B (i < ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| GTE :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((B (not (i < ii)))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| LTE :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((B (not (i > ii)))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| PLUS :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((N (i + ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| MINUS :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((N (i - ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in sub") )
| MULT :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((N (i * ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in mult") )
| DIV :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((N (i / ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in div") )
| REM :: cs ->(match s with N ii :: (N i :: ss) -> secdmc ((N (i mod ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in rem") )
| AND :: cs -> (match s with B ii :: (B i :: ss) -> secdmc ((B (i && ii))::ss) e cs d | _ -> raise (Error "stackpop != 2bool in and") )
| OR :: cs -> (match s with B ii :: (B i :: ss) -> secdmc ((B (i || ii))::ss) e cs d | _ -> raise (Error "stackpop != 2bool in or") )
| PAREN :: cs -> secdmc s e cs d
| IFTE :: cs -> (match s with cF::(cT::(B i::ss)) -> if i then secdmc (cT::ss) e cs d else secdmc (cF::ss)  e cs d | _ -> raise (Error "IFTE condition not satisfied by stack"))
| LET (ls) :: cs -> secdmc [] e ls (D (s,e,cs) :: d)
| DEF str :: cs -> (match s with ans :: ss -> secdmc ss (augment e [(str,ans)]) cs d | _ -> raise (Error "Cant define") )
| CLOS (str,ls) :: cs -> secdmc ((C (str,ls,e))::s) e cs d
| FCALL :: cs -> (match s with ans :: C (str,ls,ed) :: ss -> secdmc [] (augment ed [(str,ans)]) ls (D (ss,e,cs) :: d) | _ -> raise (Error "FCALL can't be done due to prior errors") )
| RET :: cs -> (match s with ans :: sdash -> (match d with (D (sOLD,eOLD,cOLD) :: ds) -> secdmc (ans :: sOLD) eOLD cOLD ds | _ -> raise (Error "Dump malfunction")) | _ -> raise (Error "Stackpop = NULL"))
(* | _ -> raise (Error "Not Implemented") *)
;;
