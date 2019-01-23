(* Generate a lexer from this file by executing the following command:

    $ ocamllex week2.mll

   Above command generates a file named "week2A.ml". Use this file from an
   ocaml top-level as below:

   #use "week2.ml";;
   lexme "hello";;
   lexme "42";;
   lexme "-42";;
   lexpwd "hello$";;
   lexpwd "hello";;
*)

(* This section within the braces is called the header. You can include
  arbitrary OCaml code here and use it in later sections. *)
{
  type token  = Int of int | Float of float | String of string
  type passwd = Password of string
}

(* This section is called the identifiers section. Here, you can define
  variables pointing to regex patterns. *)
let whitespace = [' ' '\t']+
let digit = ['0'-'9']
let digits = digit+
let integer = '-'? digits
let letter = ['a'-'z' 'A'-'Z']
let id = letter+
let spl_char = ['*' '.' '$']
let letter_or_spl_char = letter|spl_char

(* Password that requires a combination of letters and special characters. *)
let password = (letter*)spl_char+(letter_or_spl_char*)

(* (Exercise) Write regex for:
1. Email addresses: at least a single dot after @
2. Floating point numbers
3. Scientific notation of floating point numbers
4. Phone numbers: XXX-XX-XXXX style
5. URLs of pdf files
6. Balanced parentheses: (), (()), ((())), ... . Is it doable?
*)

(* This section is called the rules section. Think of a rule name as a
  function that, in this case, returns a value of type 'token'. Notice that
  everything inside the curly braces can be any arbitrary OCaml expression. *)
rule read = parse
  (digits)'.'digits as f {Float (float_of_string f)}
| integer as n           {Int (int_of_string n)}
| id as s                {String s}
| _                      {read lexbuf}

and readpasswd = parse
  password as p {Password p}

(* This last section within the curly braces is called the trailer. Some useful
functions are defined here to be usable by clients. Notice how the rules are
invoked as function calls with the input string as an argument. *)
{
  let lexme s = read (Lexing.from_string s)
  let lexpwd s = readpasswd (Lexing.from_string s)
}
