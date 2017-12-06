OBJS=models.cmo command.cmo state.cmo
NAME=boardactions
OFIND=ocamlfind ocamlc -thread -package str,js_of_ocaml,js_of_ocaml.syntax,yojson -syntax camlp4o

$(NAME).byte: $(OBJS)
		$(OFIND) -linkpkg -o $@ $(OBJS) $(NAME).ml
		js_of_ocaml +nat.js $@
		$(OFIND) -linkpkg -o $(OBJS) $(NAME).ml
		js_of_ocaml +nat.js $(NAME).byte

%.cmo: %.ml
		$(OFIND) -c $<i
		$(OFIND) -c $<

clean:
	ocamlbuild -clean
	rm *.cm*
	rm *.byte
	rm boardactions.js

test:
	ocamlbuild -use-ocamlfind state_test.byte && ./state_test.byte

js:
	ocamlbuild -use-ocamlfind -plugin-tag "package(js_of_ocaml.ocamlbuild)" -syntax camlp4o boardactions.js

play:
	ocamlbuild -use-ocamlfind test_engine.byte && ./test_engine.byte
