test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

clean:
	ocamlbuild -clean