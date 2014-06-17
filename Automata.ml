open Structure
  
type automaton = {
  structure            : structure ;
  alphabet             : char array ;

  size                 : int ;

  initial              : bool array ;
  final                : bool array ;
  transition           : int array array array ;
}
    
let print_automaton automaton  = 
  let n = automaton.size in
  print_string "This is a " ; 
  print_string 
    (match automaton.structure.type_s with
      | P -> "probabilistic automaton."
      | B1 -> "B-automaton with one counter."
      | BN -> "B-automaton with multiple counters."
      | C -> " (classical) automaton.") ;
  print_string "\nIt has " ; 
  print_int n ; 
  print_string " states.\nThe initial states are:\n" ;
  for i = 0 to n - 1 do
    if automaton.initial.(i) then (print_int i ; print_string " " ;)
  done ;
  print_string "\nThe final states are:\n" ;
  for i = 0 to n - 1 do
    if automaton.final.(i) then (print_int i ; print_string " " ;)
  done ;
  print_string "\nThe transitions are:\n" ;
  Array.iteri
    (fun i -> 
      (fun a ->
	print_char a ; print_newline() ; print_string (print_matrix automaton.structure n automaton.transition.(i)) ; print_newline()
      )
    )
    automaton.alphabet ;
		       

