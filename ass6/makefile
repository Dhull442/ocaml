all: run clean
run:
	ocamlc -c main.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c com.ml
	ocamlc -o program main.cmo lexer.cmo parser.cmo com.cmo

clean:
	rm *.cmo *.cmi *.mli lexer.ml parser.ml
allclean:
	rm program *.cmo *.cmi lexer.ml parser.ml
