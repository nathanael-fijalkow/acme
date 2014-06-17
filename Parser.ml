open Structure
open Automata
open Monoid

(*
  The automaton file format line by line:
  size: int
  type: char or int (c: classical, p: probabilistic, a number n: B-automaton with n counters)
  alphabet: string (no separator, each char is a letter of the alphabet)
  initial states: int Array separated by spaces or anything
  final states: int Array separated by spaces or anything
  transition matrices: int Array, one for each letter in the input order.
  
  Each matrix is preceded by a single character line, the letter (for readability and checking purposes)
*)

(* returns the index of a in the list l *)
let ind_in_list a l = 
  let rec search i = function
    | [] -> failwith "not found"
    | t::q -> if a = t then i else search (i+1) q
  in search 0 l
  
(* returns the index of a in the tab t *)
let ind_in_tab a t = 
  let rec search i = if t.(i) = a then i else search (i+1)
  in search 0
  
(* is s a substring of str *)
let is_sub s str = 
  let k = String.length s and n = String.length str in
  if k > n then false else
    let i = ref 0 and cherche = ref true in
    while (!i + k <= n && !cherche) do
      if (String.compare s (String.sub str !i k) = 0) then cherche := false else i := !i + 1 ;
    done;
    not !cherche
      
(* read from chan, but lines starting with % and blank lines are ignored *)
let data_line chan =
  let str = ref (input_line chan) in
  while (!str = "" || int_of_char (!str).[0] = 13 || int_of_char (!str).[0] = 37) do str := input_line chan done;
  if (int_of_char (!str).[(String.length !str) - 1] = 13) then str := String.sub (!str) 0 ((String.length !str) - 1) ;
  !str
    
(* reads formatted automaton from channel chan *)
let read_automaton chan = 
  let n = int_of_string (data_line chan) 
  in
  
  let structure =
    let line = data_line chan in
    if line.[0] = 'c' then boolean_structure
    else if line.[0] = 'p' then probabilistic_structure
    else (let nb_counters = int_of_string line in
	  if nb_counters = 1 then one_counter_structure
	  else multiple_counter_structure)
  in
  
  let alph_string = data_line chan in
  let alphabet_size = String.length alph_string in
  let alph = Array.init alphabet_size (fun i -> alph_string.[i]) 
  in
  
  let initstring = data_line chan in
  let init = Array.init n (fun p -> is_sub (string_of_int p) initstring) 
  in
  
  let finstring = data_line chan in
  let fin = Array.init n (fun p -> is_sub (string_of_int p) finstring) 
  in
  
  let trans = Array.init alphabet_size 
    (fun i -> 
      let astring = data_line chan in
      if astring.[0] <> alph.(i) then failwith "Formatting problem: in transition matrices, letter expected" ;
      let m = Array.make_matrix n n (-1) in
      for j = 0 to n - 1 do
	let line = data_line chan in
	let slist = Str.split (Str.regexp "[ \t]+") line in
	if List.length slist <> n then failwith "Formatting problem: wrong number of transitions" ;
	List.iteri (fun k -> fun act -> m.(j).(k) <- ind_in_tab act structure.print_s) slist ; 
      done ; 
      m
    )
  in
				       
  {
    size = n ;
    alphabet = alph ;
    initial = init ;
    final = fin ;
    transition = trans ;
    structure = structure ;
  }

