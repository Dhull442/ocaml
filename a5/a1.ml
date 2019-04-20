exception InvalidArgument;;
exception Error of string;;
type exptype = Tint | Tunit | Tbool | Tfunc of exptype* exptype;;
type expr =
  V of string
  | Integer of int
  | Bool of bool
  | Lambda of (expr * expr)
  | RecLambda of (expr * expr)
  | App of (expr * expr)
  | Plus of (expr * expr)
  | Minus of (expr * expr)
  | Mult of (expr * expr)
  | Div of (expr * expr)
  | Rem of (expr * expr)
  | And of (expr * expr)
  | Or of (expr * expr)
  | InParen of expr
  | Not of expr
  | Cmp of expr    (* CMP is |sjdnfls| *)
  | Equals of expr * expr
  | GreaterT of expr * expr
  | GreaterTE of expr * expr
  | LessT of expr * expr
  | LessTE of expr * expr
  | If_Then_Else of (expr * expr * expr)
  | Let of definition * expr
and definition =
    Simple of string * expr
  | Sequence of (definition list)
  | Parallel of (definition list)
  | Local of definition * definition;;

type closure = CL of expr * ((string * closure) list) | VCL of expr | DCL of definition * (string * closure) list;;

type krivinetoken = KADD of closure | KSUB of closure | KMULT of closure | KDO of closure | KDONE of closure | KDEF of (string * closure) | KIFTE of closure | KAND of closure | KOR of closure | KCMP;;

let rec find x g = match g with
 gi :: gs -> (match gi with (a,b) -> (if (a = x) then b else find x gs) | _ -> raise InvalidArgument)
 | [] -> raise (Error ("G doesn't contain "^x) );;

 let rec augment g g_dash =
 let rec contains key l = match l with
   li :: ls -> (match li with (a,b) -> if ( a = key ) then true else contains key ls )
   | [] -> false
   in
   match g with
   li :: ls -> (match li with (key,value) -> if contains key g_dash then augment ls g_dash else augment ls (li :: g_dash))
   | [] -> g_dash
 ;;
let unwrap ad = match ad with
 VCL (Integer x) -> x | _ -> raise (Error "execution result not integer")
;;
let bunwrap ad = match ad with
  VCL (Bool x) -> x | _ -> raise (Error "execution result not bool");;
(* let printexp e = match
let printclosure cl = match cl with
  CL (exp,) *)
let printstack s = match s with
  si :: ss -> (match si with
      KADD cl -> print_bytes "KADD ";
    | KSUB cl -> print_bytes "KSUB ";
    | KMULT cl -> print_bytes "KMULT ";
    | KDO cl -> print_bytes "KDO ";
    | KDEF (str,cl) -> print_bytes "KDEF ";print_bytes str;
    | KDONE cl -> print_bytes "KDONE ";
    | KIFTE cl -> print_bytes "KIFTE ";
    | KAND cl -> print_bytes "KAND ";
    | KOR cl -> print_bytes "KOR ";
    | KCMP -> print_bytes "KCMP ";
      )
  | [] -> print_endline "EMPTY";
;;

let rec kmc c s =
if List.length s > 100 then
  raise (Error "Stack Overflow")
else(
match c with
  VCL ( Integer a ) -> (
      match s with
        KADD (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KADD c :: ss)
      | KADD (VCL (Integer first)) :: ss -> kmc (VCL (Integer (first + a))) ss
      | KMULT (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KMULT c :: ss)
      | KMULT (VCL (Integer first)) :: ss -> kmc (VCL (Integer (first * a))) ss
      | KSUB (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KSUB c :: ss)
      | KSUB (VCL (Integer first)) :: ss -> kmc (VCL (Integer (first - a))) ss
      | KCMP :: ss -> kmc (VCL (Bool (a > 0))) ss
      | _ -> KDONE (c) :: s
      )
| VCL ( Bool b ) -> (
      match s with
        KIFTE caseT :: KIFTE caseF :: ss -> if b then kmc caseT ss else kmc caseF ss
      | KAND (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KAND (c) :: ss)
      | KAND (VCL (Bool first)) :: ss -> kmc (VCL (Bool (first && b)))  ss
      | KOR (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KOR (c) :: ss)
      | KOR (VCL (Bool first)) :: ss -> kmc (VCL (Bool (first || b))) ss
      | _ -> KDONE (c) :: s
  )
| CL (And (a,b), g) -> (
      kmc (CL (a,g)) (KAND (CL (b,g)) :: s)
  )
| CL (Or (a,b), g) -> (
      kmc (CL (a,g)) (KOR (CL (b,g)) :: s)
  )
| CL (Cmp (a), g) -> (
      kmc (CL (a,g)) (KCMP :: s)
  )
| CL ( Plus (a,b) , g ) -> (
      kmc (CL (a,g)) (KADD (CL (b,g)) :: s)
  )
| CL ( Minus (a,b) , g ) -> (
        kmc (CL (a,g)) (KSUB (CL (b,g)) :: s)
    )
| CL ( Mult (a,b) , g ) -> (
          kmc (CL (a,g)) (KMULT (CL (b,g)) :: s)
      )
| CL ( Integer a , g ) -> (
      kmc (VCL (Integer a)) s
  )
| CL (Lambda (V str,body), g) -> (
      match s with
      last :: ss -> (match last with KDO clos -> kmc (CL (body, augment g [(str,clos)])) ss
    | _ -> raise (Error "Lamda expects DO in stack")) | _ -> raise (Error "Stack is empty, Can't apply Lambda")
  )
| CL (RecLambda (V str,body), g) -> (
      match s with
      last :: ss -> (match last with KDO clos -> kmc (CL (body, augment g [(str,clos)])) ss
    | _ -> raise (Error "Lamda expects DO in stack")) | _ -> raise (Error "Stack is empty, Can't apply Lambda")
  )
| CL (App (next,input), g) -> (
      kmc (CL (next,g)) (KDO (CL (input,g))::s)
  )
| CL (V str,g) -> (
    let k = find str g in
    match k with
     CL (exp,gnew) -> (match exp with RecLambda (a,b) -> kmc (CL (exp,augment gnew [(str,k)])) s | _ -> kmc (CL (exp,gnew)) s)
    | VCL (exp) -> KDONE k :: s
    | _ -> raise (Error "Not handled DCL")
  )
| CL (Let (def,expr),g) -> ( kmc (DCL (def,g)) (KDO (CL (expr,g)) :: s)
  )
| DCL (def,g) -> (
    match def with
      Simple (str,exp) -> (match s with KDO (CL (expr,gexp)) :: ss -> kmc (CL (expr,augment gexp [(str,CL (exp,g))])) ss | _ -> raise (Error "expected sth to do for the def"))
      | _ -> raise InvalidArgument
  )
| CL (If_Then_Else(case,condT,condF),g) -> (
    kmc (CL (case,g)) (KIFTE (CL (condT,g)) :: KIFTE (CL (condF,g)) :: s )
  )
| _ -> raise InvalidArgument)
;;

let rec krivinemc c s =
let getgdash def gd = match def with
 Simple (str,expr) -> [(str, CL (expr,gd))]
 (* SimpleRec (str,expr) -> let gd = augment gd [] *)
| _ -> raise (Error "definition not Implemented")
in
  match c with
  VCL a -> c :: s
| CL (Integer a,_) -> VCL (Integer a) :: s
| CL (Bool b,_) -> VCL (Bool b) :: s
| CL (V str, g) -> krivinemc (find str g) s
| CL (Lambda (V str,b),g) -> (match s with last :: ss -> krivinemc (CL (b, augment g [(str,last)])) ss | _ -> raise (Error "Stack has no value on top"))
| CL (App(abs,input),g) -> krivinemc (CL (abs,g)) (CL (input,g) :: s)
| CL (Plus(a,b),g) -> (let f1 = List.hd (krivinemc (CL (a,g)) []) in let f2 = List.hd (krivinemc (CL (b,g)) []) in (match (f1,f2) with (VCL (Integer op1), VCL (Integer op2)) -> (VCL (Integer (op1 + op2))) :: s | _ -> raise (Error "PLUS can\'t be done")))
| CL (Minus(a,b),g) -> (let f1 = List.hd (krivinemc (CL (a,g)) []) in let f2 = List.hd (krivinemc (CL (b,g)) []) in (match (f1,f2) with (VCL (Integer op1), VCL (Integer op2)) -> (VCL (Integer (op1 - op2))) :: s | _ -> raise (Error "Minus can\'t be done")))
| CL (Mult(a,b),g) -> (let f1 = List.hd (krivinemc (CL (a,g)) []) in let f2 = List.hd (krivinemc (CL (b,g)) []) in (match (f1,f2) with (VCL (Integer op1), VCL (Integer op2)) -> (VCL (Integer (op1 * op2))) :: s | _ -> raise (Error "Mult can\'t be done")))
| CL (Div(a,b),g) -> (let f1 = List.hd (krivinemc (CL (a,g)) []) in let f2 = List.hd (krivinemc (CL (b,g)) []) in (match (f1,f2) with (VCL (Integer op1), VCL (Integer op2)) -> (VCL (Integer (op1 / op2))) :: s | _ -> raise (Error "Div can\'t be done")))
| CL (Rem(a,b),g) -> (let f1 = List.hd (krivinemc (CL (a,g)) []) in let f2 = List.hd (krivinemc (CL (b,g)) []) in (match (f1,f2) with (VCL (Integer op1), VCL (Integer op2)) -> (VCL (Integer (op1 mod op2))) :: s | _ -> raise (Error "Rem can\'t be done")))
| CL (Cmp(a),g) -> let f1 = List.hd (krivinemc (CL (a,g)) []) in (match f1 with VCL (Integer a) -> VCL (Bool (a>0)) :: s | _ -> raise (Error "Invalid evaluation"))
| CL (And(a,b),g) -> let f1 = List.hd (krivinemc (CL (a,g)) []) in let f2 = List.hd (krivinemc (CL (b,g)) []) in (match (f1,f2) with (VCL (Bool op1), VCL (Bool op2)) -> (VCL (Bool (op1 && op2))) :: s | _ -> raise (Error "AND can\'t be done"))
| CL (Or(a,b),g) -> let f1 = List.hd (krivinemc (CL (a,g)) []) in let f2 = List.hd (krivinemc (CL (b,g)) []) in (match (f1,f2) with (VCL (Bool op1), VCL (Bool op2)) -> (VCL (Bool (op1 || op2))) :: s | _ -> raise (Error "OR can\'t be done"))
| CL (If_Then_Else(a,b,c),g) -> let cond = List.hd (krivinemc (CL (a,g)) []) in (match cond with VCL (Bool ans) -> if ans then (krivinemc (CL (b,g)) s) else (krivinemc (CL (c,g)) s) | _ -> raise (Error "IFTE wrong Implemented" ) )
| CL (InParen(a),g) -> krivinemc (CL (a,g)) s
| CL (Let(def,t),g) -> krivinemc (CL (t,augment g (getgdash def g))) s
| _ -> raise InvalidArgument;;

let rec execute t g =
let bigex ad = match ad with
  CL (td,gd) -> execute td gd
  | VCL td -> ad
  | _ -> raise (Error "Not Understood")
  in
let getgdash def gd = match def with
 Simple (str,expr) -> [(str, execute expr gd)]
| _ -> raise (Error "definition not Implemented")
in
  match t with
  V str -> bigex (find str g)
| Integer a -> VCL t
| Bool a -> VCL t
| Lambda (a,b) -> CL (t,g)
| App (a,b) -> (match a with (Lambda(V str, expression)) -> bigex (CL (expression, augment g [(str,CL(b,g))])) | _ -> raise (Error "Lamda in App is not good"))
| Plus (a,b) -> VCL (Integer ((unwrap (execute a g)) + (unwrap (execute b g))))
| Minus (a,b) -> VCL (Integer ((unwrap (execute a g)) - (unwrap (execute b g))))
| Mult (a,b) -> VCL (Integer ((unwrap (execute a g)) * (unwrap (execute b g))))
| Div (a,b) -> VCL (Integer ((unwrap (execute a g)) / (unwrap (execute b g))))
| Rem (a,b) -> VCL (Integer ((unwrap (execute a g)) mod (unwrap (execute b g))))
| Cmp (a) -> VCL (Bool (unwrap (execute a g) > 0))
| And (a,b) -> VCL (Bool ((bunwrap (execute a g)) && (bunwrap (execute b g))))
| Or (a,b) -> VCL (Bool ((bunwrap (execute a g)) || (bunwrap (execute b g))))
| Not (a) -> VCL (Bool (not (bunwrap (execute a g))))
| InParen (a) -> execute a g
| Equals (a,b) -> VCL (Bool ((unwrap (execute a g)) = (unwrap (execute b g))))
| GreaterT (a,b) -> VCL (Bool ((unwrap (execute a g)) > (unwrap (execute b g))))
| GreaterTE (a,b) -> VCL (Bool (not ((unwrap (execute a g)) < (unwrap (execute b g)))))
| LessT (a,b) -> VCL (Bool ((unwrap (execute a g)) < (unwrap (execute b g))))
| LessTE (a,b) -> VCL (Bool (not ((unwrap (execute a g)) = (unwrap (execute b g)))))
| If_Then_Else (a,b,c) -> (let k = bunwrap (execute a g) in if k then execute b g else execute c g)
| Let (d,e) -> execute e (augment g (getgdash d g))
| _ -> raise (Error "not Implemented");;

type opcode = VAR of string | NCONST of int | BCONST of bool | NOT | CMP
  | PLUS | MINUS | MULT | DIV | REM | AND | OR | EQS | GTE | LTE | GT | LT | DEF of string
  | LET of (opcode list)
  | PAREN | IFTE | CLOS of string*(opcode list) | RET | FCALL;;

type answer = N of int | B of bool | C of string*(opcode list)*((string * answer) list);;
type dump = D of (answer list)*((string * answer) list)*(opcode list);;
let rec compile t =
  let rec compiledef d = match d with
  Simple (str,e) -> (* match e with RecLambda(V inp, expr) -> () | _ -> *) (compile e) @ [DEF str]
  | _ -> raise (Error "Not Implemented")
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
| _ -> raise (Error "Not Implemented");;

let rec stackmc s e c d =
  match c with
  [] -> List.hd s
| VAR str :: cs -> stackmc ((find str e)::s) e cs d
| NCONST i :: cs -> stackmc ((N i)::s) e cs d
| BCONST b :: cs -> stackmc ((B b)::s) e cs d
| NOT :: cs ->( match s with B b :: ss -> stackmc ((B (not b))::ss) e cs d | _ -> raise (Error "stackpop != bool"))
| CMP :: cs ->( match s with N i :: ss -> stackmc ((B (i > 0))::ss) e cs d | _ -> raise (Error "stackpop != int"))
| EQS :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((B (i = ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| GT :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((B (i > ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| LT :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((B (i < ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| GTE :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((B (not (i < ii)))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| LTE :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((B (not (i > ii)))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| PLUS :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((N (i + ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in add") )
| MINUS :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((N (i - ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in sub") )
| MULT :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((N (i * ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in mult") )
| DIV :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((N (i / ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in div") )
| REM :: cs ->(match s with N ii :: (N i :: ss) -> stackmc ((N (i mod ii))::ss) e cs d | _ -> raise (Error "stackpop != 2int in rem") )
| AND :: cs -> (match s with B ii :: (B i :: ss) -> stackmc ((B (i && ii))::ss) e cs d | _ -> raise (Error "stackpop != 2bool in and") )
| OR :: cs -> (match s with B ii :: (B i :: ss) -> stackmc ((B (i || ii))::ss) e cs d | _ -> raise (Error "stackpop != 2bool in or") )
| PAREN :: cs -> stackmc s e cs d
| IFTE :: cs -> (match s with cF::(cT::(B i::ss)) -> if i then stackmc (cT::ss) e cs d else stackmc (cF::ss)  e cs d | _ -> raise (Error "IFTE condition not satisfied by stack"))
| LET (ls) :: cs -> stackmc [] e ls (D (s,e,cs) :: d)
| DEF str :: cs -> (match s with ans :: ss -> stackmc ss (augment e [(str,ans)]) cs d | _ -> raise (Error "Cant define") )
| CLOS (str,ls) :: cs -> stackmc ((C (str,ls,e))::s) e cs d
| FCALL :: cs -> (match s with ans :: C (str,ls,ed) :: ss -> stackmc [] (augment ed [(str,ans)]) ls (D (ss,e,cs) :: d) | _ -> raise (Error "FCALL can't be done due to prior errors") )
| RET :: cs -> (match s with ans :: sdash -> (match d with (D (sOLD,eOLD,cOLD) :: ds) -> stackmc (ans :: sOLD) eOLD cOLD ds | _ -> raise (Error "Dump malfunction")) | _ -> raise (Error "Stackpop = NULL"))
| _ -> raise (Error "Not Implemented");;
