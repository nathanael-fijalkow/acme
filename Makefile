win:
	ocamlc -o acme.exe str.cma Structure.mli Structure.ml Automata.mli Automata.ml Monoid.mli Monoid.ml Parser.mli Parser.ml Minimization.mli Minimization.ml EquivalenceChecking.mli EquivalenceChecking.ml FinitePowerProperty.mli FinitePowerProperty.ml Dotty.mli Dotty.ml Main.ml
	del *.cmi 
	del *.cmo

linux: 	
	ocamlc -o acme str.cma Structure.mli Structure.ml Automata.mli Automata.ml Monoid.mli Monoid.ml Parser.mli Parser.ml Minimization.mli Minimization.ml EquivalenceChecking.mli EquivalenceChecking.ml FinitePowerProperty.mli FinitePowerProperty.ml Dotty.mli Dotty.ml Main.ml 
	rm *.cmi *.cmo

