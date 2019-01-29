2017CS10370 - [Saksham Dhull](https://github.com/Dhull442)
# A Bigint package
This is a preliminary assignment which will be used in following assignments.  In this assignment, you will write in OCaml a BIGNUM package where you will implement arithmetic for arbitrarily large numbers, using lists of digits to implement an integer.
<br>
module type BigInt = sig
Typedef:<br>
  type bigint = sign \* int list
    and sign = Neg | NonNeg
  Representational invariant of the elements of the int list:
    - the elements of the int list are between 0 and 9
    - presented most significant digit first
    - there are no unnecessary leading zeros.
* Function:
  - Addition: `val add: bigint -> bigint -> bigint`
  - Multiplication: `val mult: bigint -> bigint -> bigint`
  - Subtraction: `val sub: bigint -> bigint -> bigint`
  - Quotient: `val div: bigint -> bigint -> bigint`
  - Remainder: `val rem: bigint -> bigint -> bigint`
  - Unary negation: `val minus: bigint -> bigint`
  - Absolute value: `val abs: bigint -> bigint`
  - Comparison operations:
    - Equal: `val eq: bigint -> bigint -> bool`
    - Greater than: `val gt:  bigint -> bigint -> bool`
    - Less than: `val lt:  bigint -> bigint -> bool`
    - Great or equal: `val geq:  bigint -> bigint -> bool`
    - Less or equal: `val leq:  bigint -> bigint -> bool`
  - to_string: `val print_num: bigint -> string`
  - Conversion from int to bigint: `val mk_big:  int -> bigint`

* Helper Functions define:
  - Unsigned List Comparator: `val listcomp: 'a list -> 'a list -> int`
  - Remove waste 0's from the front of list: `val checklist : int list -> int list`
  - Addition of 2 unsigned lists: `val list_add : int list -> int list -> int list`
  - Negation of Whole list: `val negation : int list -> int list`
  - Subtraction of two unsigned lists: `val list_subtract : int list -> int list -> int list`
  - Multiplication of 2 unsigned lists: `val list_mult : int list -> int list -> int list`
  - Divides first list by second and reports quotient: `val list_div : int list -> int list -> int list`
  - Divides first list by second, handles negative dividend: `val neg_list_div : int list -> int list -> int list`
  - Converts normal list to bigint: `val cvt_listsub_to_sub : int list -> bigint`

## Instructions for Compiling:
Keep signature_a0.mli and structure_a0.ml in the same folder <br>
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
