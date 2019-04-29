(* open Type *)
let rec contains name rl = match rl with
  ri :: rs ->( match ri with (name,_,_) -> true | _ -> contains name rs)
| [] -> false
;;
let rec change name value rl ret = match rl with
  ri :: rs -> (match ri with (name,Tint,_) -> ret @ [(name,Tint,value)] @ rs | _ -> change name value rs (ret @ [ri]))
| [] -> raise (Error ("No variable named \'"^name^"\' defined"));;
let rec get name l = match l with
  li :: ls -> (match li with F (n,_,_,_,_,_) -> if n=name then li else get name ls)
| [] -> raise (Error ("No such element \'"^name^"\'"))
;;
let rec eval ex (sf,cstack) =
let rec map invb =
match invb with
  VAR (s,t) :: ls -> (s,t,N 0) :: (map ls)
| [] -> []
| _ -> raise (Error "Shouldn't be this")
in
match sf with F (fname,r,invb,prevr,sfl,head) ->(
  match ex with
  VAR (s,t) -> if (contains s r) then raise (Error (s^" already declared.")) else (F (fname,(s,t,N 0) :: r,invb,prevr,sfl,head),cstack)
| ASSIGN (s,v) -> (F (fname,change s v r [],invb,prevr,sfl,head),cstack)
| DEFINE (P (pname,inputv)) -> (F (pname,[],map inputv,r@invb@prevr,[],sf),cstack)
| CALL (pname, ls) -> let f = get pname sfl in ( match f with F (_,_,f3,_,_,_) -> if(List.length ls = List.length f3) then (sf,pname :: cstack) else raise (Error "Input variables don't match function parameters") | _ -> raise (Error "Not a callable frame"))
| RET -> (match head with F(hname,hr,hinvb,hprevr,hsfl,hhead) -> (F(hname,hr,hinvb,hprevr,sf :: hsfl,hhead),cstack) | NULL -> raise (Error "Program Over"))
| _ -> raise (Error "Not Yet implemented")
)
| NULL -> (
  match ex with
  Program s -> (F(s,[],[],[],[],NULL),cstack)
  | _ -> raise (Error "Unexpected flow")
);;


let g = (F ("main", [("Z", Tint, N 10)], [], [],
  [F ("fib", [], [("A", Tint, N 0); ("B", Tint, N 0)], [("Z", Tint, N 10)], [], F ("main", [("Z", Tint, N 10)], [], [], [], NULL))], NULL),
 []);;
