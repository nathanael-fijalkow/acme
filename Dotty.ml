open Structure
open Automata
open Monoid

let create_dotty_file_aut (automaton:automaton) h =
  let structure = automaton.structure in
  let n = automaton.size in
  
  output_string h "digraph G {\nrankdir = LR ; overlap = false ; \n" ;
  output_string h "labelloc=\"t\";\nlabel=\"Automaton\";\n";
  
  let b = ref false in
  for i = 0 to n - 1 do if automaton.final.(i) && automaton.initial.(i) then b := true ; done ;
  if !b then 
    begin
      output_string h "node [shape = doublecircle, color = navy, style = filled] ; " ;
      for i = 0 to n - 1 do if automaton.final.(i) && automaton.initial.(i) then output_string h ((string_of_int i)^" ; ") ; done ;
      output_string h "\n" ;
    end ;
  
  b := false ;
  for i = 0 to n - 1 do if automaton.final.(i) && (not automaton.initial.(i)) then b := true ; done ;
  if !b then 
    begin
      output_string h "node [shape = doublecircle, color = lightblue, style = filled] ; " ;
      for i = 0 to n - 1 do if automaton.final.(i) && (not automaton.initial.(i)) then output_string h ((string_of_int i)^" ; ") ; done ;
      output_string h "\n" ;
    end ;
  
  b := false ;
  for i = 0 to n - 1 do if (not automaton.final.(i)) && automaton.initial.(i) then b := true ; done ;
  if !b then 
    begin
      output_string h "node [shape = circle, color = navy, style = filled] ; " ;
      for i = 0 to n - 1 do if (not automaton.final.(i)) && automaton.initial.(i) then output_string h ((string_of_int i)^" ; ") ; done ;
      output_string h "\n" ;
    end ;
  
  output_string h "node [shape = circle, color = lightblue, style = filled];\n" ;
  for k = 0 to (Array.length automaton.alphabet - 1) do
	for i = 0 to n - 1 do
	for j = 0 to n - 1 do
	  match structure.type_s with
	    | P | C -> if automaton.transition.(k).(i).(j) = structure.one then 
	output_string h ((string_of_int i) ^ " -> " ^ (string_of_int j) ^ " [ label = \"" ^ (String.make 1 automaton.alphabet.(k)) ^ "\" ];\n") ;
	    | B1 | BN -> if automaton.transition.(k).(i).(j) <> 4 then 
	output_string h ((string_of_int i) ^ " -> " ^ (string_of_int j) ^ " [ label = \"" ^ (String.make 1 automaton.alphabet.(k)) ^ " : " ^ structure.print_s.(automaton.transition.(k).(i).(j)) ^ "\" ];\n") ;
	done ;
      done ;
  done ;
  
  output_string h "}\n"

let create_dotty_file_mon (automaton:automaton) (monoid:monoid) h =
  let structure = automaton.structure in
  let n = automaton.size in
  let m = monoid.size_m in

for k = 0 to m-1 do
  
  output_string h "digraph G {\nrankdir = LR ; overlap = false ; \n" ;
  output_string h ("labelloc = \"t\" ;\nlabel = \"" ^ monoid.name.(k) ^ " " ^ monoid.attribute.(k) ^"\" ; \n") ; 

(*  print_newline() ; print_string (print_matrix monoid.structure n monoid.matrix.(k)) ; print_string monoid.attribute.(k) ; print_string "\n\n" ; *)

  let b = ref false in
  for i = 0 to n - 1 do if automaton.final.(i) && automaton.initial.(i) then b := true ; done ;
  if !b then 
    begin
      output_string h "node [shape = doublecircle, color = navy, style = filled] ; " ;
      for i = 0 to n - 1 do if automaton.final.(i) && automaton.initial.(i) then output_string h ((string_of_int i)^" ; ") ; done ;
      output_string h "\n" ;
    end ;
  
  b := false ;
  for i = 0 to n - 1 do if automaton.final.(i) && (not automaton.initial.(i)) then b := true ; done ;
  if !b then 
    begin
      output_string h "node [shape = doublecircle, color = lightblue, style = filled] ; " ;
      for i = 0 to n - 1 do if automaton.final.(i) && (not automaton.initial.(i)) then output_string h ((string_of_int i)^" ; ") ; done ;
      output_string h "\n" ;
    end ;
  
  b := false ;
  for i = 0 to n - 1 do if (not automaton.final.(i)) && automaton.initial.(i) then b := true ; done ;
  if !b then 
    begin
      output_string h "node [shape = circle, color = navy, style = filled] ; " ;
      for i = 0 to n - 1 do if (not automaton.final.(i)) && automaton.initial.(i) then output_string h ((string_of_int i)^" ; ") ; done ;
      output_string h "\n" ;
    end ;
  
  output_string h "node [shape = circle, color = lightblue, style = filled] ;\n" ;
  for i = 0 to n - 1 do
    for j = 0 to n - 1 do
      match structure.type_s with
	| B1 -> if monoid.matrix.(k).(i).(j) <> structure.zero then 
	    output_string h ((string_of_int i) ^ " -> " ^ (string_of_int j) ^ " [ label = \"" ^ structure.print_s.(monoid.matrix.(k).(i).(j)) ^ "\" ];\n") ;
	| P | C -> 
	  if monoid.matrix.(k).(i).(j) = 2 then output_string h ((string_of_int i) ^ " -> " ^ (string_of_int j) ^ " ;\n") ;
	  if monoid.matrix.(k).(i).(j) = 1 then output_string h ((string_of_int i) ^ " -> " ^ (string_of_int j) ^ " [ label = \"+\" ] ;\n") ;
	| BN -> ()
    done ;
  done ;
  output_string h "}\n"
    
done ;
  

