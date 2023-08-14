world:
	dune build

switch:
	opam switch create . --deps-only --locked -y

deps:
	opam install . --deps-only --locked -y

lock:
	dune build ./*.opam
	opam lock . -w

watch:
	dune build -w

install:
	dune install --release

uninstall:
	dune uninstall

clean:
	dune clean

benchmarks:
	dune exec -- benchmarks

.PHONY: all switch deps watch install uninstall lock clean
