TARGETS = demo.byte

default:
	ocamlbuild -use-ocamlfind $(TARGETS)

clean:
	ocamlbuild -clean
