FSEVENTS=$(shell opam config var osx-fsevents:installed)
INOTIFY=$(shell opam config var inotify:installed)

all:
	ocaml pkg/pkg.ml build \
	  --with-fsevents $(FSEVENTS) --with-inotify $(INOTIFY)
