OCAMLBUILD = ocamlbuild -use-ocamlfind

.PHONY: all clean
.SUFFIXES: .ml .native

all: matvec.native

clean:
	$(OCAMLBUILD) -clean

.ml.native:
	$(OCAMLBUILD) $@
