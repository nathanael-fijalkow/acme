open Automata
open Monoid
open Minimization

exception Not_Equivalent
exception Finished

val equivalence : monoid -> monoid -> bool 
val eq_aut : automaton -> automaton -> bool
