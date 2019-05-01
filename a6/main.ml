exception Error of string;;
type extype = Tint | Tunit;;
type elements = SL of string list | VB of string * extype * int | RET of int | FP of int | SP of int | NAME of string;;
type expr = N of int | V of string;;
type commands = ASSIGN of string * expr | CALL of string * (expr list) | RETURN | VIEW;;
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
let rec find s st = match st with
  si :: ss -> (match si with VB (name,_,_) -> if (name = s) then true else find s ss | RET (_) -> false | _ -> find s ss)
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

let st = [VB("a",Tint,10)];;
let rec eval x op =
  match op with (st,g,sp)
  match x with
  ASSIGN (s,ex) -> (match ex with (N i) -> (update s i st [],g,sp) | V x -> let i = findval x st  in (update s i st [],g,sp))
| VIEW ->print_endline "|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|-|"; printstack st ;(st,g,sp)
| CALL (s,ls) ->
| RETURN
| _ -> raise (Error "Unin")
;;
