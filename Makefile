types: types.ml
	ocamlbuild types.native

formule: formule.ml types.ml
	ocamlbuild types.native formule.native
