open A1
exception Not_implemented;;
exception InvalidArgument;;
exception Error of string;;


let rec findtype g str = match g with
  li :: ls -> (match li with (a,b) -> (if a = str then b else findtype ls str) | _ -> raise (Error "G is not defined properly as a list of tuples"))
  | [] -> raise (Error "No type defined for the given variable")
;;

let rec get n l = match l with
  li :: ls -> if n = 0 then li else get n-1 ls
  | [] -> raise InvalidArgument;;


let rec getexptype g e =
let rec getexptypelist g tuplist = match tuplist with
  li :: ls -> (getexptype g li) :: (getexptypelist g ls)
  | [] -> []
in
 match e with
  N a -> Tint
| B a -> Tbool
| Var a -> (findtype g a)
| Abs a -> if(getexptype g a = Tint)  then Tint else raise InvalidArgument
| Negative a ->if(getexptype g a = Tint) then Tint else raise InvalidArgument
| Not a -> if getexptype g a = Tbool then Tbool else raise InvalidArgument
| Add(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tint else raise InvalidArgument
| Sub(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tint else raise InvalidArgument
| Mult(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tint else raise InvalidArgument
| Div(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tint else raise InvalidArgument
| Rem(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tint else raise InvalidArgument
| Conjunction(a,b) -> if (getexptype g a = Tbool) && (getexptype g b = Tbool) then Tbool else raise InvalidArgument
| Disjunction(a,b) -> if (getexptype g a = Tbool) && (getexptype g b = Tbool) then Tbool else raise InvalidArgument
| Equals(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tbool else raise InvalidArgument
| GreaterTE(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tbool else raise InvalidArgument
| LessTE(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tbool else raise InvalidArgument
| GreaterT(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tbool else raise InvalidArgument
| LessT(a,b) -> if (getexptype g a = Tint) && (getexptype g b = Tint) then Tbool else raise InvalidArgument
| InParen a -> getexptype g a
| IfThenElse (a,b,c) -> let k = getexptype g b in (if (getexptype g a= Tbool) && (k = getexptype g c) then k else raise InvalidArgument)
| Tuple (a,b) ->  Ttuple ( getexptypelist g b )
| Project ((i,n),tree) -> (match tree with Tuple (a,b) -> getexptype g (get (i-1) b) | _ -> raise InvalidArgument)
| Let (def, tree) -> getexptype (augment g (gettypegdash g def)) tree
(* | FunctionAbstraction (a,b) -> (t=Tunit)
| FunctionCall (a,b) -> match t with Tfunc(at,bt) -> (checktype g a bt) && (checktype g b at) *)
| _ -> raise Not_implemented
;;

let rec augment g g_dash =
let rec contains key l = match l with
  li :: ls -> (match li with (a,b) -> if ( a = key ) then true else contains key ls )
  | [] -> false
  in
  match g with
  li :: ls -> (match li with (key,value) -> if contains key g_dash then augment ls g_dash else augment ls (li :: g_dash))
  | [] -> g_dash
;;

let rec present x l = match l with
  li :: ls -> (if x = li then true else present x ls)
  | [] -> false;;
let rec intersectionNull a b = match a with
li :: ls -> if present li b then false else intersectionNull ls b
| [] -> true;;
let rec gettypegdash g def =
match def with
  Simple (a,b) -> [(a,getexptype g b)]
  | Sequence l -> (
      match l with
        li :: ls -> let g_D = gettypegdash g li
        in
        augment g_D (gettypegdash (augment g (g_D)) (get 0 ls)))
  | Parallel l -> (
      let g_d1 = gettypegdash g (get 0 l)
      in let g_d2 = gettypegdash g (get 1 l)
      in if(intersectionNull g_d1 g_d2) then g_d1 @ g_d2 else raise (Error "Parallel definitions include common definitions")
    )
  | Local (l1,l2) -> (
      let g_d = gettypegdash g l1 in
      augment g_d (gettypegdash (augment g g_d)) l2)
    );;

(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t =

let rec checktype g list1 list2 = if (List.length list1 = List.length list2) then
  (match list1 with
  li1::ls1 -> match list2 with li2::ls2 -> (hastype g li1 li2) && (checktype ls1 ls2)
  | [] -> true)
  else false
  in
 match e with
  N a -> t = Tint
| B a -> t = Tbool
| Var a -> (findtype g a) = t
| Abs a -> (t = Tint)&&(hastype g a t)
| Negative a ->( t= Tint) && (hastype g a t)
| Not a -> (t=Tbool) && hastype g a t
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

let rec equal a b =
match a with
li :: ls -> if (present li b) then equal ls b else false
| [] -> true;;
(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = let f = (gettypegdash g d) in if (List.length f = List.length g_dash) then equals f g_dash else false;;
