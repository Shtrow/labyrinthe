COMPILATOR = ocamlfind ocamlc

main : ./main.ml
	$(COMPILATOR) -package camomile -o a.out camomileLibrary.cma $<

clean : 
	rm ./*.cm*
