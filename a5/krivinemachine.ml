open Types;;

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
  )
| CL (If_Then_Else(case,condT,condF),g) -> (
    kmc (CL (case,g)) (KIFTE (CL (condT,g)) :: KIFTE (CL (condF,g)) :: s )
  )
| CL (InParen (expr), g) -> kmc (CL (expr, g)) s
| _ -> raise InvalidArgument)
;;
