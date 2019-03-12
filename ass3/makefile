a3:
	ocamlbuild a0.cmo a1.cmo a2.cmo a3.cmo
debug:
	ocamlyacc -v a3.mly
	mv a3.* junk/
	mv junk/a3.mly .
test:
	ocaml test_a3.mly

clean:
	rm -rf _build
