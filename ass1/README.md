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
  - [ ] (* Arithmetic operations:  *)<br>
  `val add: bigint -> bigint -> bigint`
  - [ ] (* Multiplication *)<br>
  `val mult: bigint -> bigint -> bigint`
  - [ ] (* Subtraction *)<br>
  `val sub: bigint -> bigint -> bigint`
  - [ ] (* Quotient *)<br>
  `val div: bigint -> bigint -> bigint`
  - [ ] (* Remainder *)<br>
  `val rem: bigint -> bigint -> bigint`
  - [ ] (* Unary negation *)<br>
  `val minus: bigint -> bigint`
  - [ ] (* Absolute value *)<br>
  `val abs: bigint -> bigint`

  (* Comparison operations:  *)
  - [ ] (* Equal *)<br>
  `val eq: bigint -> bigint -> bool`
  - [ ] (* Greater_than. *)<br>
  `val gt:  bigint -> bigint -> bool`
  - [ ] (* Less_than. *)<br>
  `val lt:  bigint -> bigint -> bool`
  - [ ] (* Great_or_equal. *)<br>
  `val geq:  bigint -> bigint -> bool`
  - [ ] (* Less_or_equal.  *)<br>
  `val leq:  bigint -> bigint -> bool`

  - [x] (* Functions to present the result in the form of a string. *)<br>
  `val print_num: bigint -> string`

  - [x] (* Conversion functions from OCaml int to bigint. *)<br>
  `val mk_big:  int -> bigint`


## Instructions for submission:
1 Your submission should have one file named structure_a0.ml which implements the interface as below.
  open Signature_a0
  module A0 : BigInt = struct
   (* Your code goes here *)
  end

2 Your code won't compile if all the functions declared in the signature_a0.mli are not implemented in the structure_a0.ml. In case you decide not to implement a function, but want your code to compile and run.
 - Mention this in the comments
 - Add "exception Not_implemented" in the structure_a0 (NOT signature_a0.mli)
 - Provide a dummy implementation (for those functions which you aren't implementing) which raises an exception Not_implemented
E.g.: In case you aren't implementing add
  exception Not_implemented
  let add n1 n2 = raise Not_implemented

3 Keep signature_a0.mli and structure_a0.ml in the same folder
 A. Compile the signature file as
    ocamlc signature_a0.mli
    This creates a .cmi files
 B. Use the top level to test your code
    #use "structure_a0.ml";;
    open A0;;
    add (mk_big 5) (mk_big 10);;

4 Grading criteria:
- Correctness
- Efficiency
- Code readability, comments etc

*)
