exception Error of string;;
type extype = Tint | Tunit;;
type elements = VB of string * extype * int | NVB of string* extype * int | FP of int | SP of int | NAME of string * int * int * int;;
type expr = N of int | V of string;;
type commands = ASSIGN of string * expr | CALL of string * (expr list) | RETURN | VIEW | VIEWR ;;
type frame = F of string * int * (string list) * ((string * extype) list) * ((string * extype) list);;
let g =[    F ("main", 0, ["P";"Q"],[("a",Tint);("b",Tint);("c",Tint)],[]);
            F ("P", 1, ["Q";"R";"S"],[("z",Tint);("a",Tint)],[("x",Tint);("y",Tint)]);
            F ("Q",1,["P";"T";"U"],[("x",Tint);("b",Tint)],[("z",Tint);("w",Tint)]);
            F ("R",2,["P";"Q";"S";"V"],[("j",Tint);("b",Tint)],[("w",Tint);("i",Tint)]);
            F ("S",2,["P";"R";"Q"],[("m",Tint);("n",Tint)],[("c",Tint);("k",Tint)]);
            F ("T",2,["W";"U";"Q";"P"],[("i",Tint);("f",Tint)],[("a",Tint);("y",Tint)]);
            F ("U",2,["T";"Q";"P"],[("p",Tint);("g",Tint)],[("c",Tint);("z",Tint)]);
            F ("V",3,["P";"R";"S";"Q"],[("c",Tint)],[("m",Tint);("n",Tint)]);
            F ("W",3,["T";"U";"Q";"P"],[("j",Tint);("h",Tint)],[("m",Tint);("p",Tint)]);   ];;
              (* main *)
            (* /      \ *)
           (* P        Q *)
          (* / \      / \ *)
         (* R   S    T   U *)
        (* /        /       *)
       (* V        W         *)
let rec get i g = match g with
  gi :: gs -> if (i=0) then gi else get (i-1) gs
| [] -> raise (Error "Not so deep");;
let rec getel s g = match g with
   gi :: gs -> (match gi with F (name,_,_,_,_) -> if (name = s) then gi else getel s gs)
| [] -> raise (Error "Not present");;
let rec find s st = match st with
  si :: ss -> (match si with VB (name,_,_) -> if (name = s) then true else find s ss | _ -> find s ss)
| [] -> false;;
let rec finds s st = match st with
  si :: ss -> (match si with name -> if (name = s) then true else finds s ss)
| [] -> false;;

let rec findval s fp dr st =
  let rec updatein s st en n stack dr lv retst=
    match stack with
    si :: ss -> if (n>=st && n <=en) then
      (match si with VB (str,t,i) -> if (str = s) then i else updatein s st en (n+1) ss dr lv (retst@[si]) | NVB (str,t,i) -> if (str = s) then i else updatein s st en (n+1) ss dr lv (retst@[si]) | _ -> updatein s st en (n+1) ss dr lv (retst@[si])  )
      else
        if n < st
          then updatein s st en (n+1) ss dr lv (retst@[si])
        else
          if (lv<=0)
            then raise (Error ("No variable \""^s^"\""))
          else
            let k = get (lv-1) dr in
            (match get (k-1) retst with NAME (_,newlvl,up,down) -> updatein s (k - 2 - down) (k+up) 0 (retst@stack) dr newlvl [] | _ -> raise (Error "check implementation"))
  | [] -> if ( n > en ) then if lv <= 0 then raise (Error ("No variable \""^s^"\"")) else let k = get (lv-1) dr in
  (match get (k-1) retst with NAME (_,newlvl,up,down) -> updatein s (k - 2 - down) (k+up) 0 (retst@stack) dr newlvl [] | _ -> raise (Error "check implementation"))
  else
  raise (Error ("No variable \""^s^"\""))
  in
  match get (fp-1) st with
  NAME (name,lvl,lvn,invn) -> (updatein s (fp-2-invn) (fp+lvn) 0 st dr lvl [])
  | _ -> raise (Error "implementation error");;

let rec update s i fp dr st =
  let rec updatein s i st en n stack dr lv retst=
    match stack with
    si :: ss -> if (n>=st && n <=en) then
      (match si with VB (str,t,_) -> if (str = s) then retst @ [NVB (str,t,i)] @ ss else ((*print_endline "var not match incr n";*) updatein s i st en (n+1) ss dr lv (retst@[si]))  | NVB (str,t,_) -> if (str = s) then retst @ [NVB (str,t,i)] @ ss else updatein s i st en (n+1) ss dr lv (retst@[si])  | _ -> ((*print_endline "not a var" ;*) updatein s i st en (n+1) ss dr lv (retst@[si]))  )
      else
        if n < st
          then  ((* print_bytes "Incrwasing n";*)updatein s i st en (n+1) ss dr lv (retst@[si]))
        else
          if (lv<=0)
            then raise (Error ("No variable \""^s^"\""))
          else
            let k = get (lv-1) dr in
            (match get (k-1) retst with NAME (_,newlvl,up,down) -> (*print_endline "trying deeper level" ;*) updatein s i (k - 2 - down) (k+up) 0 (retst@stack) dr newlvl [] | _ -> raise (Error "check implementation"))
  | [] -> if (n>en) then  if lv <= 0 then raise (Error ("No variable \""^s^"\"")) else let k = get (lv-1) dr in
  (match get (k-1) retst with NAME (_,newlvl,up,down) -> (*print_endline "trying deeper level" ;*) updatein s i (k - 2 - down) (k+up) 0 (retst@stack) dr newlvl [] | _ -> raise (Error "check implementation"))
  else
  raise (Error ("No variable \""^s^"\""))
  in
  match get (fp-1) st with
  NAME (name,lvl,lvn,invn) -> (updatein s i (fp-2-invn) (fp+lvn) 0 st dr lvl [])
  | _ -> raise (Error "implementation error");;

let rec printstack st =
  (* let rec printlist s = match s with
  si :: ss -> print_bytes ("\""^si^"\"; ") ; printlist ss
| [] -> print_bytes "" *)
  (* in *)
  print_endline "--------";
match st with
  si :: ss -> (match si with
 VB (name,typ,value) -> print_bytes ("VB ( \""^name^"\", "); if typ = Tint then print_bytes "Tint, " else print_bytes "Tunit, " ; print_endline ((string_of_int value)^" )")
| NVB (name,typ,value) -> print_bytes ("VB ( \""^name^"\", "); if typ = Tint then print_bytes "Tint, " else print_bytes "Tunit, " ; print_endline ((string_of_int value)^" )   <------- NEW")
| FP (i) -> print_endline ("FP "^(string_of_int i));
| SP (i) -> print_endline ("SP "^(string_of_int i));
| NAME (s,lvl,lvb,invb) -> print_endline ("NAME "^(s)^", LEVEL ("^(string_of_int lvl)^")");
); printstack ss
| [] -> print_endline "|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|";;

let st = [SP 0;NAME ("main",0,3,0);FP 0;VB ("a",Tint,0);VB ("b",Tint,0); VB ("c",Tint,0)];;
let sp = 7;;
let fp = 2;;
let dr = [2];;
let rec eval x op=
  let rec removenew st =
  match st with NVB (s,t,i)  :: ss -> VB (s,t,i)::(removenew ss)
  | si :: ss -> si :: (removenew ss)
  | [] -> []
  in
  let rec updatedr dr st fp=
  let rec change i v d =
  match d with di :: ds -> if i=0 then v :: ds else di :: (change (i-1) v ds)
  | [] -> raise (Error "DR Not long enough")
  in
   match st with
  NAME (_,lvl,_,_) :: ss -> updatedr (change lvl (fp+1) dr) ss (fp+1)
  | _ :: ss -> updatedr dr ss (fp+1)
  | [] -> dr
  in
  let rec alter len st ret = match st with
    si :: ss -> if(len>0) then (alter (len-1) ss [si]@ret) else ret
    | [] -> if (len=0) then ret else raise (Error "Expected longer")
    in
  let rec createargs l invb ret stack fp dr= match invb with
  (s,ty) :: invbs ->
  (match l with
    li :: ls -> (match li with N i -> createargs ls invbs (VB (s,ty,i) :: ret) stack fp dr | V str -> createargs ls invbs (VB(s,ty,findval str fp dr (stack)) :: ret) stack fp dr)
  | [] -> raise (Error "Irregular Length"))
  | [] -> ret
  in
  let rec initialize lv = match lv with
  (s,ty) :: ls -> VB (s,ty,0) :: initialize ls
  | [] -> []
  in
  let rec change level v drs = match drs with
  di :: ds -> if (level = 0) then v :: ds else di :: (change (level-1) v ds)
| [] -> if (level = 0) then [v] else (-1) :: (change (level-1) v [])
  in
  let rec printdr i dr = match dr with
  di :: ds -> print_bytes ("LEVEL "^(string_of_int i)^" : ") ; print_endline (string_of_int di) ; printdr (i+1) ds
  | [] -> print_endline "";
  in
  match op with (st,g,sp,fp,dr) ->
  (match x with
  ASSIGN (s,ex) -> (match ex with (N i) -> (update s i fp dr st,g,sp,fp,dr) | V x -> let i = findval x fp dr (st)  in (update s i fp dr st,g,sp,fp,dr))
| VIEW ->print_endline "|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|"; printstack st ;(removenew st,g,sp,fp,dr)
| VIEWR -> print_endline "|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|=|"; print_bytes "SP => ";print_endline (string_of_int sp); print_bytes "FP => " ;print_endline  (string_of_int fp); print_bytes "DR => \n" ;printdr 0 dr; print_endline "|=|=|=|=|=|=|=|=|";(st,g,sp,fp,dr)
| CALL (s,ls) ->( match (get (fp-1) st) with NAME (name,_,_,_) ->
  (match getel name g with F (_,_,lst,_,_) ->
  (if finds s lst then
    match getel s g  with
    F (name,level,child,lvb,invb) ->
    (if (List.length ls = List.length invb) then
      let x = List.length ls in let locals = initialize lvb in
      (st @ (createargs ls invb [] st fp dr) @ [SP (sp);NAME (name,level,List.length lvb,List.length invb);FP(fp)] @ (locals) ,g,sp+x+3+(List.length locals),sp+x+1,change level (sp+x+1) dr);
    else
      raise (Error "Input variables num not equal"))
  else raise (Error "Function not callable!")))
  | _ -> raise (Error "Expected NAME"))
| RETURN -> (match get (fp-2) st with SP (spo) ->( match get fp st with FP (fpo) -> let newst = List.rev (alter (spo-1) st []) in (newst,g,spo,fpo,updatedr dr newst 0) | _ -> raise (Error "Expected FP")) | _ -> raise (Error "Expected sp"))
)
;;
let ans = (st,g,sp,fp,dr);;
