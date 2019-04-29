all:
	ocamlc -c evaluator.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c compile.ml
	ocamlc -o machine evaluator.cmo lexer.cmo parser.cmo compile.cmo

clean:
	rm calculate *.cmo *.cmi *.mli lexer.ml parser.ml
