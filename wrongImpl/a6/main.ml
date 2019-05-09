exception Error of string;;
type extype = Tint | Tunit;;
type elements = SL of string list | VB of string * extype * int | RET of int | FP of int | SP of int | NAME of string;;
type expr = N of int | V of string;;
type commands = ASSIGN of string * expr | CALL of string * (expr list) | RETURN | VIEW | VIEWSPFP ;;
type frame = F of string * (string list) * ((string * extype) list) * ((string * extype) list);;
let g =[    F ("main",["P";"Q"],[("a",Tint);("b",Tint);("c",Tint)],[]);
            F ("P",["R";"S"],[("z",Tint);("a",Tint)],[("x",Tint);("y",Tint)]);
            F ("Q",["T";"U"],[("x",Tint);("b",Tint)],[("z",Tint);("w",Tint)]);
            F ("R",["V"],[("j",Tint);("b",Tint)],[("w",Tint);("i",Tint)]);
            F ("S",[],[("m",Tint);("n",Tint)],[("c",Tint);("k",Tint)]);
            F ("T",["W"],[("i",Tint);("f",Tint)],[("a",Tint);("y",Tint)]);
            F ("U",[],[("p",Tint);("g",Tint)],[("c",Tint);("z",Tint)]);
            F ("V",[],[("c",Tint)],[("m",Tint);("n",Tint)]);
            F ("W",[],[("j",Tint);("h",Tint)],[("m",Tint);("p",Tint)]);   ];;
let rec get i g = match g with
  gi :: gs -> if (i=0) then gi else get (i-1) gs
| [] -> raise (Error "Not so deep");;
let rec getel s g = match g with
   gi :: gs -> (match gi with F (name,_,_,_) -> if (name = s) then gi else getel s gs)
| [] -> raise (Error "Not present");;
let rec find s st = match st with
  si :: ss -> (match si with VB (name,_,_) -> if (name = s) then true else find s ss | RET (_) -> false | _ -> find s ss)
| [] -> false;;
let rec finds s st = match st with
  si :: ss -> (match si with name -> if (name = s) then true else finds s ss)
| [] -> false;;

let rec findval s st = match st with
  VB (name,t,va)  :: ss -> if (name = s) then va else findval s ss
| _ :: ss-> findval s ss
| [] -> raise (Error ("No variable with name \'"^s^"\' found!"));;
let rec update s i st retst= match st with
  si :: ss -> (match si with VB (name,t,v) -> if (name = s) then retst @ [VB(name,t,i)]@ ss else update s i ss (retst @ [si]) | _ -> update s i ss (retst @ [si]))
| [] -> raise (Error ("No variable named \'"^s^"\' found!") );;
let rec printstack st =
  let rec printlist s = match s with
  si :: ss -> print_bytes ("\""^si^"\"; ") ; printlist ss
| [] -> print_bytes ""
  in
  print_endline "--------";
match st with
  si :: ss -> (match si with
  SL (ls) -> print_bytes "SL [ "; printlist ls ; print_endline "]"
| VB (name,typ,value) -> print_bytes ("VB ( \""^name^"\", "); if typ = Tint then print_bytes "Tint, " else print_bytes "Tunit, " ; print_endline ((string_of_int value)^" )")
| RET (i) -> print_endline ("RET "^(string_of_int i));
| FP (i) -> print_endline ("FP "^(string_of_int i));
| SP (i) -> print_endline ("SP "^(string_of_int i));
| NAME (s) -> print_endline ("NAME "^(s));
); printstack ss
| [] -> print_endline "|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|";;

let st = [SP 0;NAME ("main");FP 0;SL (["P";"Q"]);VB ("a",Tint,0);VB ("b",Tint,0); VB ("c",Tint,0)];;
let sp = 7;;
let fp = 2;;
let rec eval x op =
  let rec alter len st ret = match st with
    si :: ss -> if(len>0) then (alter (len-1) ss [si]@ret) else ret
    | [] -> if (len=0) then ret else raise (Error "Expected longer")
    in
  let rec createargs l invb ret stack= match invb with
  (s,ty) :: invbs ->
  (match l with
    li :: ls -> (match li with N i -> createargs ls invbs (VB (s,ty,i) :: ret) stack | V str -> createargs ls invbs (VB(s,ty,findval str (List.rev stack)) :: ret) stack)
  | [] -> raise (Error "Irregular Length"))
  | [] -> ret
  in
  let rec initialize lv = match lv with
  (s,ty) :: ls -> VB (s,ty,0) :: initialize ls
  | [] -> []
  in
  match op with (st,g,sp,fp) ->
  (match x with
  ASSIGN (s,ex) -> (match ex with (N i) -> (List.rev (update s i (List.rev st) []),g,sp,fp) | V x -> let i = findval x (List.rev st)  in (List.rev (update s i (List.rev st) []),g,sp,fp))
| VIEW ->print_endline "|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|"; printstack st ;(st,g,sp,fp)
| VIEWSPFP -> print_endline "|=|=|=|=|=|=|=|=|"; print_bytes "SP => ";print_endline (string_of_int sp); print_bytes "FP => "; print_endline  (string_of_int fp); print_endline "|=|=|=|=|=|=|=|=|";(st,g,sp,fp)
| CALL (s,ls) ->( match (get (fp+1) st) with SL (lst) ->
  (if finds s lst then
    match getel s g  with
    F (name,child,lvb,invb) ->
    (if (List.length ls = List.length invb) then
      let x = List.length ls in let locals = initialize lvb in
      (st @ (createargs ls invb [] st) @ [SP (sp);NAME (name);FP(fp);SL (child)]@(locals),g,sp+x+4+(List.length locals),sp+x+2)
    else
      raise (Error "Input variables num not equal"))
  else raise (Error "Function not callable!"))
  | _ -> raise (Error "WRONG IMPELE")
  )
| RETURN -> (match get (fp-2) st with SP (spo) ->( match get fp st with FP (fpo) -> (List.rev (alter spo st []),g,spo,fpo) | _ -> raise (Error "Expected FP")) | _ -> raise (Error "Expected sp"))

)
;;
let ans = (st,g,sp,fp);;
