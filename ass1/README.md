# A Bigint package
This is a preliminary assignment which will be used in following assignments.  In this assignment, you will write in OCaml a BIGNUM package  where you will implement arithmetic for arbitrarily large numbers, using lists of digits to implement an integer.
<br>
module type BigInt = sig
* Typedef:<br>
  type bigint = sign * int list
    and sign = Neg | NonNeg
  (* Representational invariant of the elements of the int list:
    - the elements of the int list are between 0 and 9
    - presented most significant digit first
    - there are no unnecessary leading zeros. *)

* Function:
  - [x] (* Addition:  *)<br>
  `val add: bigint -> bigint -> bigint`
  - [x] (* Multiplication *)<br>
  `val mult: bigint -> bigint -> bigint`
  - [x] (* Subtraction *)<br>
  `val sub: bigint -> bigint -> bigint`
  - [X] (* Quotient *)<br>
  `val div: bigint -> bigint -> bigint`
  - [x] (* Remainder *)<br>
  `val rem: bigint -> bigint -> bigint`
  - [x] (* Unary negation *)<br>
  `val minus: bigint -> bigint`
  - [X] (* Absolute value *)<br>
  `val abs: bigint -> bigint`

  (* Comparison operations:  *)
  - [X] (* Equal *)<br>
  `val eq: bigint -> bigint -> bool`
  - [X] (* Greater_than. *)<br>
  `val gt:  bigint -> bigint -> bool`
  - [X] (* Less_than. *)<br>
  `val lt:  bigint -> bigint -> bool`
  - [X] (* Great_or_equal. *)<br>
  `val geq:  bigint -> bigint -> bool`
  - [X] (* Less_or_equal.  *)<br>
  `val leq:  bigint -> bigint -> bool`

  - [x] (* Functions to present the result in the form of a string. *)<br>
  `val print_num: bigint -> string`

  - [x] (* Conversion functions from OCaml int to bigint. *)<br>
  `val mk_big:  int -> bigint`

* Helper Functions define: <br>
  - Unsigned List Comparator:<br>
    `val listcomp: 'a list -> 'a list -> int`
  * Gives the modulo of numbers with proper handling for negative numbers:<br>
    `val modulo : int -> int -> int`   
  * Divide Function handling negative numbers as well :<br>
    `val divide : int -> int -> int`
  - Remove waste 0's from the front of list :<br>
    `val checklist : int list -> int list`
  - Addition of 2 unsigned lists ( Takes inverted lists and outputs real value list ):<br>
    `val list_add : int list -> int list -> int list`
  * Negation of Whole list :<br>
    `val negation : int list -> int list`
  - Subtraction of two unsigned lists ( Takes inverted lists and outputs order list ) : <br>
    `val list_subtract : int list -> int list -> int list`
  * Mapping of a list : <br>
    `val map : ('a -> 'b -> 'c) -> 'a -> 'b list -> 'c list`
  - Multiplication of 2 unsigned lists (takes list in ordered way ) :<br>
    `val list_mult : int list -> int list -> int list`
  - Divides first list by second and reports quotient (Takes inverted lists) :<br>
    `val list_div : int list -> int list -> int list`
  - Divides first list by second, handles negative dividend :<br>
    `val neg_list_div : int list -> int list -> int list`
  - Converts normal list to bigint :<br>
    `val cvt_listsub_to_sub : int list -> bigint`

## Instructions for submission:
1 Your submission should have one file named structure_a0.ml which implements the interface as below.<br>
  ``` 
  open Signature_a0 
  module A0 : BigInt = struct
   (* Your code goes here *)
  end
  ```

2 Your code won't compile if all the functions declared in the signature_a0.mli are not implemented in the structure_a0.ml. In case you decide not to implement a function, but want your code to compile and run <br>
 - Mention this in the comments
 - Add "exception Not_implemented" in the structure_a0 (NOT signature_a0.mli)
 - Provide a dummy implementation (for those functions which you aren't implementing) which raises an exception Not_implemented <br>
E.g.: In case you aren't implementing add <br>
  exception Not_implemented <br>
  let add n1 n2 = raise Not_implemented

3 Keep signature_a0.mli and structure_a0.ml in the same folder <br>
  - Compile the signature file as
    ```
    ocamlc signature_a0.mli
    This creates a .cmi files
    ```
  - Use the top level to test your code 
    ```
    #use "structure_a0.ml";;
    open A0;;
    add (mk_big 5) (mk_big 10);;
    ```

4 Grading criteria:
  - Correctness
  - Efficiency
  - Code readability, comments etc
