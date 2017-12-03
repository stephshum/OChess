test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

js:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -syntax camlp4o boardactions.js

play:
	ocamlbuild -use-ocamlfind test_engine.byte && ./test_engine.byte

clean:
	ocamlbuild -clean
