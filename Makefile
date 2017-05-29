test:
	ocamlbuild -lib str -r test.native

bdd:
	ocamlbuild -lib str -r main.native
	cp -L main.native bdd

