(* Tutorial: Week 3 *)

(* Module
  Refer: https://www.cs.cornell.edu/courses/cs3110/2019sp/textbook/modules/intro.html
  Read 5.1 to 5.9
*)

(* Signature : Interface *)
module type MyStack = sig
  type 'a stack

  (* Returs the empty stack *)
  val empty    : 'a stack
  (* Is the stack empty *)
  val is_empty : 'a stack -> bool
  (* [push x s] returns the stack [s] with [x] pushed on the top *)
  val push     : 'a -> 'a stack -> 'a stack
  (* [peek s] returns the top element of stack [s] or raises Failure if [s] is empty. *)
  val peek     : 'a stack -> 'a
  (* [pop s] returns stack [s] after discarding its top element or raises Failure if [s] is empty. *)
  val pop      : 'a stack -> 'a stack
end

(*
  Everything defined in the signature should be implemented in the structure
  We can define additional type, exception, function in the struct, but it won't be accessible outside the file
*)

(* Structure : implementation *)
(* implementation #1 : Stack implemented as list *)
module StackAsList : MyStack = struct
  type 'a stack = 'a list
  exception StackEmpty (* This isn't there is the signature*)

  let empty = []

  let is_empty stk = (stk = [])
  (* Note: What is the difference between = and == in Ocaml  *)

  let push item stk = item :: stk

  let peek stk = match stk with
      [] -> raise StackEmpty
      | x::_ -> x

  let pop stk = match stk with
      [] -> raise StackEmpty
      | _::xs -> xs

  (* Note: Is the input stack altered by push, pop, peek?  *)
end

(* Try the following commands in the top level
   #use "week3.ml";;
   open StackAsList;;
   let s1 = push 3 ( push 2 empty );;
   let s2 = pop s1;;
   StackEmpty;; (* why this behaviour *)
 *)

(*  Homework:
    1. Try to use Ocaml's inbuilt module for Arrays
      Refer: https://caml.inria.fr/pub/docs/manual-ocaml/libref/Array.html
      ( In particular try out: make, create, init, append, concat, to_list, of_list, get, set, length )
      E.g:
      let ar = Array.make 3 1;;
      Array.get ar 0
      let ar = Array.of_list [10; 20; 30];;
      Array.get ar 1;;
    2. Implement stack signature (as StackAsArray) using Ocaml's inbuilt the Array type
    3. Queues:
      a. Define the signature for queue.
      b. Implement the queue structure
*)
