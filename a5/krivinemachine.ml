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
      | KDIV (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KDIV c :: ss)
      | KDIV (VCL (Integer first)) :: ss -> kmc (VCL (Integer (first / a))) ss
      | KREM (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KREM c :: ss)
      | KREM (VCL (Integer first)) :: ss -> kmc (VCL (Integer (first mod a))) ss
      | KCMP (i,CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KCMP (i,c) :: ss)
      | KCMP (i, VCL(Integer first)) :: ss -> (
          match i with
            0 -> kmc (VCL (Bool (first = a))) ss
          | 1 -> kmc (VCL (Bool (first > a))) ss
          | 2 -> kmc (VCL (Bool (not (first < a)))) ss
          | 3 -> kmc (VCL (Bool (first < a))) ss
          | 4 -> kmc (VCL (Bool (not (first > a)))) ss
          | _ -> raise (Error "Invalid CMP value")
        )
      | _ -> KDONE (c) :: s
      )
| VCL ( Bool b ) -> (
      match s with
        KIFTE caseT :: KIFTE caseF :: ss -> if b then kmc caseT ss else kmc caseF ss
      | KAND (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KAND (c) :: ss)
      | KAND (VCL (Bool first)) :: ss -> kmc (VCL (Bool (first && b)))  ss
      | KOR (CL (secondexpr, g)) :: ss -> kmc (CL (secondexpr, g)) (KOR (c) :: ss)
      | KOR (VCL (Bool first)) :: ss -> kmc (VCL (Bool (first || b))) ss
      | KNOT :: ss -> kmc (VCL (Bool (not b))) ss
      | _ -> KDONE (c) :: s
  )
| VCL (_) -> raise (Error "Unexpected VCL")
| CL (And (a,b), g) -> (
      kmc (CL (a,g)) (KAND (CL (b,g)) :: s)
  )
| CL (Or (a,b), g) -> (
      kmc (CL (a,g)) (KOR (CL (b,g)) :: s)
  )
| CL (Not (a), g) -> (
      kmc (CL (a,g)) (KNOT :: s)
  )
| CL (Cmp (a), g) -> (
      kmc (CL (a,g)) (KCMP (1,CL (Integer 0,g)) :: s)
  )
| CL (Equals (a,b), g) -> (
      kmc (CL (a,g)) (KCMP (0,CL (b,g)) :: s)
    )
| CL (GreaterT (a,b), g) -> (
      kmc (CL (a,g)) (KCMP (1,CL (b,g)) :: s)
    )
| CL (GreaterTE (a,b), g) -> (
      kmc (CL (a,g)) (KCMP (2,CL (b,g)) :: s)
    )
| CL (LessT (a,b), g) -> (
      kmc (CL (a,g)) (KCMP (3,CL (b,g)) :: s)
    )
| CL (LessTE (a,b), g) -> (
      kmc (CL (a,g)) (KCMP (4,CL (b,g)) :: s)
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
| CL ( Div (a,b) , g ) -> (
          kmc (CL (a,g)) (KDIV (CL (b,g)) :: s)
      )
| CL ( Rem (a,b) , g ) -> (
          kmc (CL (a,g)) (KREM (CL (b,g)) :: s)
      )
| CL ( Integer a , g ) -> (
      kmc (VCL (Integer a)) s
  )
| CL ( Bool a, g) -> (
      kmc (VCL (Bool a)) s
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
| CL (Proj (i,expr), g) -> kmc (CL (expr,g)) (KPROJ i :: s)
| CL (Tuple (len,ls) , g) -> (
    match s with
    KPROJ i :: ss -> kmc (CL (get i ls,g)) ss
    | _ -> KDONE (c) :: s
  )
| _ -> raise InvalidArgument
)
;;
