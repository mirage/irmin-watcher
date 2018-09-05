.PHONY: all test clean

all:
	dune build --dev

test:
	dune runtest --dev

clean:
	dune clean
