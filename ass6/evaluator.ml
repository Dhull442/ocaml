exception Error of string;;

type typ = Tint | Tunit ;;

type expr = N of int | VARIABLE of string * typ | ASSIGN of string * expr | CALL of string * (expr list) | DEFINE of procedure | RET | DCL of expr list | V of string | Program of string | ViewStack
and procedure = P of string * (expr list);;
type register = string * typ * expr;;
type stackframe = F of string * (register list) * (register list) * (register list) * (stackframe list) * stackframe | NULL;;
(* stackframe = name * decl vbls in this frame * input vbls * vbls from prev * stackframe list callable from here * head frame  *)

let rec contains name rl = match rl with
  ri :: rs ->( match ri with (n,_,_) -> if (n=name) then true else contains name rs | _ -> raise (Error "Invalid Argument"))
| [] -> false
;;
let rec change name value rl ret = match rl with
  ri :: rs -> (match ri with (name,Tint,_) -> ret @ [(name,Tint,value)] @ rs | _ -> change name value rs (ret @ [ri]))
| [] -> raise (Error ("No variable named \'"^name^"\' defined"));;
let rec get name l = match l with
  li :: ls -> (match li with F (n,_,_,_,_,_) -> if n=name then li else get name ls | _ -> raise (Error "Invalid Search"))
| [] -> raise (Error ("No such element \'"^name^"\'"))
;;
let rec eval ex (sf,cstack) =
let rec map invb =
match invb with
  VARIABLE (s,t) :: ls -> (s,t,N 0) :: (map ls)
| [] -> []
| _ -> raise (Error "Shouldn't be this")
in
let rec printstack c = match c with
ci :: cs -> print_endline ci; printstack cs
| [] -> print_endline "~~~~~~~~~~~~~~~~~~~~~"
in
match sf with F (fname,r,invb,prevr,sfl,head) ->(
  match ex with
  VARIABLE (s,t) -> if (contains s r) then raise (Error (s^" already declared.")) else (F (fname,(s,t,N 0) :: r,invb,prevr,sfl,head),cstack)
| ASSIGN (s,v) -> (F (fname,change s v r [],invb,prevr,sfl,head),cstack)
| DEFINE (P (pname,inputv)) -> (F (pname,[],map inputv,r@invb@prevr,[],sf),cstack)
| CALL (pname, ls) -> let f = get pname sfl in ( match f with F (_,_,f3,_,_,_) -> if(List.length ls = List.length f3) then (sf,pname :: cstack) else raise (Error "Input variables don't match function parameters") | _ -> raise (Error "Not a callable frame"))
| RET -> (match head with F(hname,hr,hinvb,hprevr,hsfl,hhead) -> (F(hname,hr,hinvb,hprevr,sf :: hsfl,hhead),cstack) | NULL -> raise (Error "Program Over"))
| ViewStack -> print_endline ("VIEWSTACK "^(string_of_int (List.length cstack))); printstack cstack; (sf,cstack)
| _ -> raise (Error "Not Yet implemented")
(* improve calling for checking variable scoping *)
)
| NULL -> (
  match ex with
  Program s -> (F(s,[],[],[],[],NULL),cstack)
  | _ -> raise (Error "Unexpected flow")
);;


let g = (F ("main", [("Z", Tint, N 10)], [], [],
  [F ("fib", [], [("A", Tint, N 0); ("B", Tint, N 0)], [("Z", Tint, N 10)], [], F ("main", [("Z", Tint, N 10)], [], [], [], NULL))], NULL),
 []);;
