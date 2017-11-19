test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

clean:
	ocamlbuild -clean
	rm -f a4src.zip
