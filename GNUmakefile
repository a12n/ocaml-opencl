CTYPES_LIB_DIR != ocamlfind query ctypes
OCAML_LIB_DIR != ocamlc -where

include $(OCAML_LIB_DIR)/Makefile.config

OCAMLBUILD =	\
	CTYPES_LIB_DIR=$(CTYPES_LIB_DIR)	\
	OCAML_LIB_DIR=$(OCAML_LIB_DIR)	\
		ocamlbuild -use-ocamlfind

.PHONY: all clean install lib uninstall utop

all: lib

clean:
	$(OCAMLBUILD) -clean

install: lib
	ocamlfind install cl	\
		META	\
		_build/cl$(EXT_LIB)	\
		_build/cl.cma	\
		_build/cl.cmi	\
		_build/cl.cmxa	\
		_build/cl.mli	\
		_build/dllcl_stubs$(EXT_DLL)	\
		_build/libcl_stubs$(EXT_LIB)

lib:
	$(OCAMLBUILD) cl.cma cl.cmxa

uninstall:
	ocamlfind remove cl

utop: lib
	utop	\
		-safe-string	\
		-I _build	\
		-I _build/gen	\
		-require ctypes.foreign	\
		-require ctypes.stubs	\
		-require ctypes.top
