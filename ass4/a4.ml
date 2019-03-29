open A1
exception Not_implemented
exception InvalidArgument
(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t =
let rec checktype g list1 list2 = if (List.length list1 = List.length list2) then
  (match list1 with
  li1::ls1 -> match list2 with li2::ls2 -> (hastype g li1 li2) && (checktype ls1 ls2)
  | [] -> true)
  else false
  in
let rec get n l = match l with
  li :: ls -> if n = 0 then li else get n-1 ls
  | [] -> raise InvalidArgument
  in
 match e with
  N a -> t = Tint
| B a -> t = Tbool
| Var a -> (g a) = t
| Abs a -> (t = Tint)&&(hastype g a t)
| Negative a ->( t= Tint) && (hastype g a t)
| Not a -> (t=Tint) && hastype g a t
| Add(a,b) -> (t=Tint) && (hastype g a t) && (hastype g a t)
| Sub(a,b) -> (t=Tint) && (hastype g a t) && (hastype g a t)
| Mult(a,b) -> (t=Tint) && (hastype g a t) && (hastype g a t)
| Div(a,b) -> (t=Tint) && (hastype g a t) && (hastype g a t)
| Rem(a,b) -> (t=Tint) && (hastype g a t) && (hastype g a t)
| Conjunction(a,b) -> (t=Tbool) && (hastype g a t) && (hastype g a t)
| Disjunction(a,b) -> (t=Tbool) && (hastype g a t) && (hastype g a t)
| Equals(a,b) -> (hastype g a Tint) && (hastype g a Tint) && (t = Tbool)
| GreaterTE(a,b) -> (hastype g a Tint) && (hastype g a Tint) && (t = Tbool)
| LessTE(a,b) -> (hastype g a Tint) && (hastype g a Tint) && (t = Tbool)
| GreaterT(a,b) -> (hastype g a Tint) && (hastype g a Tint) && (t = Tbool)
| LessT(a,b) -> (hastype g a Tint) && (hastype g a Tint) && (t = Tbool)
| InParen a -> hastype g a t
| IfThenElse (a,b,c) -> (hastype a Tbool) && (hastype b t) && (hastype c t)
| Tuple (a,b) -> (match t with Ttuple explist -> checktype g b explist | _ -> false)
| Project ((i,n),tree) ->( match tree with Tuple(a,b) -> hastype (get (i-1) b) t | _ -> false )
| Let (def, tree) -> (hastype g tree t)
| FunctionAbstraction (a,b) -> (t=Tunit)
| FunctionCall (a,b) -> match t with Tfunc(at,bt) -> (checktype g a bt) && (checktype g b at)
| _ -> raise Not_implemented
;;

(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = raise Not_implemented
