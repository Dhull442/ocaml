

let unwrap ad = match ad with
 VCL (Integer x) -> x | _ -> raise (Error "execution result not integer")
;;
let bunwrap ad = match ad with
  VCL (Bool x) -> x | _ -> raise (Error "execution result not bool");;

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
