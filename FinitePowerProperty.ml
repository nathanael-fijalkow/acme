open Structure
open Automata
open Monoid

type automaton_eps = {
  aut             : automaton ;
  epsilon         : int array array ;
} 

let print_automaton_eps automaton_eps =
print_automaton automaton_eps.aut ;
print_string "\nThe epsilon-transitions are:\n" ;
print_string (print_matrix automaton_eps.aut.structure automaton_eps.aut.size (automaton_eps.epsilon)) ;
print_newline ()

(***************************************
EPSILON-REMOVAL
***************************************)

let remove_epsilon automaton_eps =
  let structure = automaton_eps.aut.structure in
  let n = automaton_eps.aut.size in
  let prod_here = structure.prod_matrix n in
  let equal_here = equal_matrix n in

  let new_eps = Array.init n (fun i -> Array.init n (fun j -> if i = j then min structure.one automaton_eps.epsilon.(i).(j) else automaton_eps.epsilon.(i).(j))) in
  let m_eps = ref new_eps and b = ref true in
  while(!b) do
    let m_next = prod_here !m_eps !m_eps in
    (b := not (equal_here !m_eps m_next) ; m_eps := m_next)
  done ;
  let new_transition_matrix = Array.init (Array.length automaton_eps.aut.alphabet) (fun i -> prod_here !m_eps (prod_here automaton_eps.aut.transition.(i) !m_eps))
  in
  
  {
    size                 = n ;
    alphabet             = automaton_eps.aut.alphabet ;
    initial              = automaton_eps.aut.initial ;
    final                = automaton_eps.aut.final ;
    transition           = new_transition_matrix ;
    structure            = automaton_eps.aut.structure ;
  }

(*************************
FINITE POWER PROPERTY
***************************)

let automaton2Bautomaton_fpp automaton =
  let n = automaton.size in 
  let eps = Array.make_matrix n n 4 in
  for i = 0 to n - 1 do
      for j = 0 to n - 1 do
	if automaton.final.(i) && automaton.initial.(j) then eps.(i).(j) <- 2 ;
	for k = 0 to (Array.length automaton.alphabet - 1) do 
		if automaton.transition.(k).(i).(j) = 0 then automaton.transition.(k).(i).(j) <- 4 ;
	done ;
      done ;
  done ;
  let automaton2 =
    {
      size                 = automaton.size ;
      alphabet             = automaton.alphabet ;
      initial              = automaton.initial ;
      final                = automaton.final ;
      transition           = automaton.transition ;
      structure            = one_counter_structure ;
    } in 
  let automaton_eps =
      {
	aut                  = automaton2 ;
	epsilon              = eps ;
      } in
  remove_epsilon automaton_eps
      
      
