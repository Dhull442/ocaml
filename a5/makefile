run:
	ocamlbuild types.cmo parser.cmo lexer.cmo secdmachine.cmo krivinemachine.cmo
	# ocamlc -o interpreter evaluator.cmo
runold:
	ocamlbuild a1.cmo a3.cmo parser.cmo
clean:
	rm -rf _build
exe:
	ocamlc -c types.ml
	ocamlc -c krivinemachine.ml
	ocamlc -c secdmachine.ml
	ocamllex lexer.mll       # generates lexer.ml
	ocamlyacc parser.mly     # generates parser.ml and parser.mli
	ocamlc -c parser.mli
	ocamlc -c lexer.ml
	ocamlc -c parser.ml
	ocamlc -c evaluator.ml
	ocamlc -o interpreter evaluator.cmo types.cmo parser.cmo lexer.cmo secdmachine.cmo krivinemachine.cmo
