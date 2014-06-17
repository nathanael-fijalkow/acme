open Structure
open Automata

type monoid = {
  structure                : structure ;

  alpha                    : char array ;

  size_m                   : int ;
  morphism                 : int array ;
  product                  : int array array ;
  stabilization            : int array ;
  ideal                    : bool array ;

  matrix                   : int array array array ;
  name                     : string array ;
  attribute                : string array ;
  info                     : string ;
}
    
val print_monoid : monoid -> unit
val automata2monoid : automaton -> monoid

