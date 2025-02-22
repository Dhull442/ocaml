(* Type definition for the expression parse tree generated by the parser *)
type expr_tree = NULL | VAR of string | NUM of int | PLUS of expr_tree * expr_tree | INTO of expr_tree * expr_tree | SUB of expr_tree * expr_tree | DIV of expr_tree * expr_tree | BRAC of expr_tree | ASSIGN of expr_tree*expr_tree;;

exception Empty
(* Map from string variable names to their integer values. New values can be added *)
module VarTable = Map.Make(String)
let variable_set = VarTable.(empty |> add "ten" 10 |> add "hundred" 100);;

(* Function to evaluate value given the parse tree *)
let rec eval_tree t = match t with
    NULL           -> raise Empty
    | NUM(x)       -> x
    | BRAC(t1)      -> eval_tree t1
    | DIV(t1,t2)   -> (eval_tree t1)/(eval_tree t2)
    | INTO(t1,t2)  -> (eval_tree t1) * (eval_tree t2)
    | PLUS(t1,t2)  -> (eval_tree t1) + (eval_tree t2)
    | SUB(t1,t2)   -> (eval_tree t1) - (eval_tree t2)
    | ASSIGN(x,t1) -> VarTable.find x variable_set (eval_tree t1)
    | VAR(x)       -> VarTable.find x variable_set
;;

(* Function to print the expression tree, each node labelled with its level/depth *)
let rec print_tree t level = match t with
    NULL           -> Printf.printf "Empty Tree\n";
    | NUM(x)       -> Printf.printf "Level %d INT %d " level x;
    | BRAC(t1)     -> Printf.printf "Level %d ( )\n" level; print_tree t1 (level+1); print_newline();
    | DIV(t1,t2)  -> Printf.printf "Level %d /\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | SUB(t1,t2)  -> Printf.printf "Level %d -\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | INTO(t1,t2)  -> Printf.printf "Level %d *\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | PLUS(t1,t2)  -> Printf.printf "Level %d +\n" level; print_tree t1 (level+1); print_tree t2 (level+1); print_newline();
    | VAR(x)       -> Printf.printf "Level %d INT %d " level (VarTable.find x variable_set);
;;

(* TODO
 * - Try writing a compile function that converts given parse tree into a postfix code.
 *    You might have to define a new type
 * - Try evaluating compiled expression tree using stack machine
 * *)
