FSEVENTS=$(shell opam config var osx-fsevents:installed)

all:
	ocaml pkg/pkg.ml build --with-fsevents $(FSEVENTS)
