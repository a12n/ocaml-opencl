CTYPES_LIB_DIR != ocamlfind query ctypes
OCAML_LIB_DIR != ocamlc -where

include $(OCAML_LIB_DIR)/Makefile.config

OCAMLBUILD =	\
	CTYPES_LIB_DIR=$(CTYPES_LIB_DIR)	\
	OCAML_LIB_DIR=$(OCAML_LIB_DIR)	\
		ocamlbuild -use-ocamlfind

.PHONY: all clean lib utop

all: lib

clean:
	$(OCAMLBUILD) -clean

lib:
	$(OCAMLBUILD) cl.cma cl.cmxa

utop: lib
	utop	\
		-safe-string	\
		-I _build	\
		-I _build/gen	\
		-require ctypes.foreign	\
		-require ctypes.stubs	\
		-require ctypes.top
