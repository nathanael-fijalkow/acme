open Structure

type automaton = {
  structure            : structure ;
  alphabet             : char array ;

  size                 : int ;

  initial              : bool array ;
  final                : bool array ;
  transition           : int array array array ;
}

val print_automaton : automaton -> unit

