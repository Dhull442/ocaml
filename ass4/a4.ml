open A1
exception Not_implemented;;
exception InvalidArgument;;
exception Error of string;;


let rec findtype g str = match g with
  li :: ls -> (match li with (a,b) -> (if a = str then b else findtype ls str) )
  | [] -> Tunit
  (* raise (Error "No type defined for the given variable") *)
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

let rec get n l = match l with
  li :: ls -> if n = 0 then li else get (n-1) ls
  | [] -> raise InvalidArgument;;


let rec present x l = match l with
    li :: ls -> (if x = li then true else present x ls)
    | [] -> false;;
let rec intersectionNull a b = match a with
  li :: ls -> if present li b then false else intersectionNull ls b
  | [] -> true;;

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
| Project ((i,n),tree) -> (match (getexptype g tree) with Ttuple ls -> (get (i-1) ls) | _ -> raise InvalidArgument)
| Let (def, tree) -> getexptype (augment g (gettypegdash g def)) tree
| FunctionAbstraction (a,t,b) -> Tfunc (t, getexptype (augment g [(a,t)]) b)
| FunctionCall (a,b) -> let atype = getexptype g a and btype = getexptype g b in (match atype with Tfunc(at,bt) -> if (at = btype) then bt else raise InvalidArgument | _ -> raise InvalidArgument)

and gettypegdash g def =
match def with
  Simple (a,t,b) -> if (t = getexptype g b) then [(a,t)] else raise (Error "Undefined variable being used");
  | Sequence l -> (
      match l with
        li :: ls -> let g_D = gettypegdash g li
        in
        augment g_D (gettypegdash (augment g (g_D)) (get 0 ls))
      | [] -> [] )
  | Parallel l -> (
      let g_d1 = gettypegdash g (get 0 l)
      in let g_d2 = gettypegdash g (get 1 l)
      in if(intersectionNull g_d1 g_d2) then g_d1 @ g_d2 else raise (Error "Parallel definitions include common definitions")
    )
  | Local (l1,l2) -> (
      let g_d = gettypegdash g l1 in
      ((gettypegdash (augment g g_d)) l2))
;;
(* hastype : ((string * exptype) list) -> exptree -> exptype -> bool *)
let rec hastype g e t = try (getexptype g e = t) with _ -> false ;;

let rec equal a b =
match a with
li :: ls -> if (present li b) then equal ls b else false
| [] -> true;;
(* yields : ((string * exptree) list) -> definition -> ((string * exptree) list) -> bool *)
let rec yields g d g_dash = try (let f = (gettypegdash g d) in if (List.length f = List.length g_dash) then equal f g_dash else false) with _ -> false;;
