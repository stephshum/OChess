test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

play:
	ocamlbuild -use-ocamlfind test_engine.byte && ./test_engine.byte

clean:
	ocamlbuild -clean
