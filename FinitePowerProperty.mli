open Structure
open Automata
open Monoid

type automaton_eps = {
  aut             : automaton ;
  epsilon         : int array array ;
} 

val print_automaton_eps : automaton_eps -> unit
val remove_epsilon : automaton_eps -> automaton
val automaton2Bautomaton_fpp : automaton -> automaton
