FSEVENTS=$(shell opam config var osx-fsevents:installed)
INOTIFY=$(shell opam config var inotify:installed)

.PHONY: all test

all:
	ocaml pkg/pkg.ml build \
	  --with-fsevents $(FSEVENTS) --with-inotify $(INOTIFY)

test:
	ocaml pkg/pkg.ml build --tests true \
	  --with-fsevents $(FSEVENTS) --with-inotify $(INOTIFY)
	ocaml pkg/pkg.ml test
