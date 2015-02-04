open Structure
open Automata
open Monoid
open Parser
open Minimization
open EquivalenceChecking
open FinitePowerProperty
open Dotty    

let help = "Usage:
\t acme [FUNCTION] [FILENAME] [DISPLAY]\n

[FUNCTION] The available functions are:
-sm: \t\t checks whether a B-automaton is limited by computing its stabilization monoid, and then minimize it,
-mma: \t\t runs the Markov Monoid Algorithm on a probabilistic automaton,
-equ: \t\t checks whether two B-automata are equivalent,
-fpp: \t\t checks whether a classical automaton has the finite power property,
-dotty: \t displays an automaton using graphviz.\n

[FILENAME] The input file should be a .txt file formatted as specified in Data.txt.
It should contain the description of one automaton (or two in the case of equivalence checking).\n

[DISPLAY] (OPTIONAL, irrelevant for -dotty) The available options are:
-text: \t verbose (default option),
-silent: \t less verbose,
-dotty: \t creates intermediate steps in output.ps.\n"
  
let _ = 
  print_string "The tool ACME (Automata with Counters, Monoids and Equivalence) has been written in OCaml by Nathanael Fijalkow and Denis Kuperberg.\n" ;
  
  let func = ref "" and nb_param = Array.length Sys.argv in
  if (nb_param < 2 || nb_param > 4) 
  then print_string help
  else func := Sys.argv.(1) ;
  
  let data = ref (open_in "Data.txt") and display = ref "-text" and n = ref 4 and p = ref 0.2 and m = ref 6 in () ;
  let h = ref (open_out "output.gv") in () ; 

(* Function computing the stabilization monoid of a B-automaton *)
  if (!func = "-sm") then
    begin
      if (nb_param = 3) then data := open_in Sys.argv.(2) ;
      if (nb_param = 4) then (data := open_in Sys.argv.(2) ; display := Sys.argv.(3)) ;

      let auto = read_automaton !data in
      if (!display = "-text") then print_automaton auto ;
      if (!display = "-dotty") then create_dotty_file_aut auto !h ;

      let mon = automata2monoid auto in
      if (!display = "-text") then 
	begin
	  print_string "\n**********************************\n" ;
	  print_string "Computing the stabilization monoid:\n" ;
	  print_string "**********************************\n" ;
          print_monoid mon ;
          let min_mon = minimize mon in
	  begin
	  print_string "\n**********************************\n" ;
	  print_string "Minimizing the stabilization monoid:\n" ;
	  print_string "**********************************\n" ;
          print_monoid min_mon ;
          end ;
	end ;

      if mon.info = "l" 
      then print_string "\n\n-----> This automaton is limited.\n\n" 
      else print_string "\n\n-----> This automaton is not limited.\n\n" ;

      if (!display = "-dotty") then 
	begin
   	  create_dotty_file_mon auto mon !h ; 
	  close_out !h ; 
	  let _ = Sys.command "dot -Tps output.gv -o output.ps" in () ;
          print_string "**********************************\n" ;
          print_string "The files \"output.gv\" and \"output.ps\" have been created.\n" ;
          print_string "**********************************\n" ;
	end ;
    end ;

(* Function running the Markov Monoid Algorithm on a probabilistic automaton *)
  if (!func = "-mma") then	   
    begin
      if (nb_param = 3) then data := open_in Sys.argv.(2) ;
      if (nb_param = 4) then (data := open_in Sys.argv.(2) ; display := Sys.argv.(3)) ;
      let auto_prob = read_automaton !data in
      if (!display = "-text") then
	begin
	  print_string "**********************************\n" ;
	  print_string "Markov Monoid Algorithm:\n" ;
	  print_string "**********************************\n" ;
	  print_automaton auto_prob ;
	end ;
      if (!display = "-dotty") then create_dotty_file_aut auto_prob !h ;
      let mon = automata2monoid auto_prob in
      if (!display = "-dotty") then create_dotty_file_mon auto_prob mon !h ;
      if (!display = "-text") then print_monoid mon ;
      if (mon.info = "l0") then print_string "\n-----> This automaton is leaktight and does not have value 1.\n" ;
      if (mon.info = "l1") then print_string "\n-----> This automaton is leaktight and has value 1.\n" ;
      if (mon.info = "n0") then print_string "\n-----> This automaton is not leaktight and (most probably) does not have value 1.\n" ;
      if (mon.info = "n1") then print_string "\n-----> This automaton is not leaktight but has value 1.\n" ;
      if (!display = "-dotty") then 
	begin
	  close_out !h ; 
          let _ = Sys.command "dot -Tps output.gv -o output.ps" in () ;
          print_string "**********************************\n" ;
          print_string "The files \"output.gv\" and \"output.ps\" have been created.\n" ;
          print_string "**********************************\n" ;
	end ;
    end ;

(* Function checking the equivalence of two B-automata *)
  if (!func = "-equ") then
    begin
      if (nb_param = 3) then data := open_in Sys.argv.(2) ;
      if (nb_param = 4) then (data := open_in Sys.argv.(2) ; display := Sys.argv.(3)) ;
      let auto1 = read_automaton !data in
      let mon1 = automata2monoid auto1 in
      let auto2 = read_automaton !data in
      let mon2 = automata2monoid auto2 in
      if (!display = "-text") then
	begin
	  print_string "**********************************\n" ;
	  print_string "Computing the stabilization monoid of both automata:\n" ;
	  print_string "**********************************\n" ;
	  print_string "\nFirst automaton:\n" ;
	  print_automaton auto1 ;
	  print_string "\nStabilization monoid of the first automaton:\n" ;
	  print_monoid mon1 ;
	  print_string "\nSecond automaton:\n" ;
	  print_automaton auto2 ;
	  print_string "\nStabilization monoid of the second automaton:\n" ;
	  print_monoid mon2 ;
	end ;
      if (!display = "-dotty") then (create_dotty_file_aut auto1 !h ; create_dotty_file_aut auto2 !h) ;
      let min_mon1 = minimize mon1 in
      let min_mon2 = minimize mon2 in
      if (!display = "-text") then
	begin
	  print_string "\n**********************************\n" ;
	  print_string "Minimizing the stabilization monoids:\n" ;
	  print_string "**********************************\n" ;
	  print_string "\nMinimal stabilization monoid for the first automaton:\n" ;
	  print_monoid min_mon1 ;
	  print_string "\nMinimal stabilization monoid for the second automaton:\n" ;
	  print_monoid min_mon2 ;
	end ;
      if (!display = "-dotty") then (create_dotty_file_mon auto1 min_mon1 !h ; create_dotty_file_mon auto2 min_mon2 !h) ;
      if equivalence min_mon1 min_mon2
      then print_string "\n\n-----> They are equivalent.\n\n" 
      else print_string "\n\n-----> They are not equivalent.\n\n" ;
      if (!display = "-dotty") then 
	begin
	  close_out !h ; 
          let _ = Sys.command "dot -Tps output.gv -o output.ps" in () ;
          print_string "**********************************\n" ;
          print_string "The files \"output.gv\" and \"output.ps\" have been created.\n" ;
          print_string "**********************************\n" ;
	end ;
    end ;

(* Function checking the Finite Power Property for classical automata *)
  if (!func = "-fpp") then
    begin
      if (nb_param = 3) then data := open_in Sys.argv.(2) ;
      if (nb_param = 4) then (data := open_in Sys.argv.(2) ; display := Sys.argv.(3)) ;
      let auto = read_automaton !data in
      if (!display = "-text") then
	begin
	  print_string "**********************************\n" ;
	  print_string "Checking the finite power property:\n" ;
	  print_string "**********************************\n" ;
	  print_automaton auto ;
	end ;
      if (!display = "-dotty") then create_dotty_file_aut auto !h ;
      let autoB = automaton2Bautomaton_fpp auto in
      if (!display = "-text") then (print_string "\nThis is the B-automaton about to be tested for limitedness:\n" ; print_automaton autoB) ;
      if (!display = "-dotty") then create_dotty_file_aut autoB !h ;
      let mon = automata2monoid autoB in
      if (!display = "-dotty") then create_dotty_file_mon autoB mon !h ;
      if (!display = "-text") then print_monoid mon ;
      if mon.info = "l"
      then print_string "\n\n-----> This automaton has the finite power property.\n\n"
      else print_string "\n\n-----> This automaton does not have the finite power property.\n\n" ;
      if (!display = "-dotty") then 
	begin
	  close_out !h ; 
          let _ = Sys.command "dot -Tps output.gv -o output.ps" in () ;
          print_string "**********************************\n" ;
          print_string "The files \"output.gv\" and \"output.ps\" have been created.\n" ;
          print_string "**********************************\n" ;
	end ;
    end ;

  if (!func = "-dotty") then
    begin
      if (nb_param > 2) then data := open_in Sys.argv.(2) ;
      let auto = read_automaton !data in create_dotty_file_aut auto !h ;
      let _ = Sys.command "dot -Tps output.gv -o output.ps" in () ;
      print_string "**********************************\n" ;
      print_string "The files \"output.gv\" and \"output.ps\" have been created.\n" ;
      print_string "**********************************\n" ;
      close_out !h ;
      let _ = Sys.command "dot -Tps output.gv -o output.ps" in () ;
    end ;

  if (!func = "-mma_stat") then	   
    begin 
      if (nb_param = 3) then failwith "Two parameters expected: n and p.\n" ;
      if (nb_param = 4) then (n := int_of_string Sys.argv.(2) ; p := float_of_string Sys.argv.(3)) ;   
      let random_bool () = (Random.int 100) < (int_of_float (100. *. !p)) in
      let random_mat () =
	let mat = Array.init !n (fun i -> Array.make !n 0) in
	for i = 0 to !n - 1 do 
		let j = Random.int !n in mat.(i).(j) <- 2 ;
		for j = 0 to !n - 1 do
			if random_bool () then mat.(i).(j) <- 2 ;
		done ;
	done ;
      mat
	in
      
      print_string "**********************************\n" ;
      print_string "Statistics on the Markov Monoid Algorithm:\n" ;
      print_string "**********************************\n" ;
      
      let nb_l0 = ref 0 and nb_l1 = ref 0 and nb_n0 = ref 0 and nb_n1 = ref 0 in
      for k = 1 to 10 do
	Random.self_init () ; 
	let auto_prob = {
	  size            = !n ;
	  alphabet        = [|'a';'b'|] ;
	  initial         = Array.init !n (fun i -> i = 0) ;
	  final           = Array.init !n (fun i -> i = !n-1) ;
	  transition      = [| random_mat () ; random_mat () |] ;
	  structure       = probabilistic_structure ;
	} in
	let mon = automata2monoid auto_prob in
	if (mon.info = "l0") then nb_l0 := !nb_l0 + 1 ;
	if (mon.info = "l1") then nb_l1 := !nb_l1 + 1 ;
	if (mon.info = "n0") then nb_n0 := !nb_n0 + 1 ;
	if (mon.info = "n1") then nb_n1 := !nb_n1 + 1 ;
      done ;
      print_string "Number of automata that are leaktight and do not have value 1: " ; print_int !nb_l0 ; print_newline() ;
      print_string "Number of automata that are leaktight and have value 1: " ; print_int !nb_l1 ; print_newline() ;
      print_string "Number of automata that are not leaktight and may have value 1: " ; print_int !nb_n0 ; print_newline() ;
      print_string "Number of automata that are not leaktight and do not have value 1: " ; print_int !nb_n1 ; print_newline() ;
    end ;
  
  if (!func = "-mma_stat2") then	   
    begin 
      if (nb_param = 3) then failwith "Two parameters expected: n and m.\n" ;
      if (nb_param = 4) then (n := int_of_string Sys.argv.(2) ; m := int_of_string Sys.argv.(3)) ;   
      let random_mat () =
	let mat = Array.init !n (fun i -> Array.make !n 0) in
	for i = 0 to !n - 1 do let j = Random.int !n in mat.(i).(j) <- 2 done ;
	for k = 1 to !m do
		let i = Random.int !n and j = Random.int !n in mat.(i).(j) <- 2 ;
	done ;
      mat
      in
      
      print_string "**********************************\n" ;
      print_string "Statistics on the Markov Monoid Algorithm:\n" ;
      print_string "**********************************\n" ;
      
      let nb_l0 = ref 0 and nb_l1 = ref 0 and nb_n0 = ref 0 and nb_n1 = ref 0 in
      for k = 1 to 100 do
	Random.self_init () ; 
	let auto_prob = {
	  size            = !n ;
	  alphabet        = [| 'a' ; 'b' |] ;
	  initial         = Array.init !n (fun i -> i = 0) ;
	  final           = Array.init !n (fun i -> i = !n-1) ;
	  transition      = [| random_mat () ; random_mat () |] ;
	  structure       = probabilistic_structure ;
	} in
	let mon = automata2monoid auto_prob in
	if (mon.info = "l0") then nb_l0 := !nb_l0 + 1 ;
	if (mon.info = "l1") then nb_l1 := !nb_l1 + 1 ; 
	if (mon.info = "n0") then nb_n0 := !nb_n0 + 1 ;
	if (mon.info = "n1") then nb_n1 := !nb_n1 + 1 ;
      done ;
      print_string "Number of automata that are leaktight and do not have value 1: " ; print_int !nb_l0 ; print_newline() ;
      print_string "Number of automata that are leaktight and have value 1: " ; print_int !nb_l1 ; print_newline() ;
      print_string "Number of automata that are not leaktight and may have value 1: " ; print_int !nb_n0 ; print_newline() ;
      print_string "Number of automata that are not leaktight and have value 1: " ; print_int !nb_n1 ; print_newline() ;
    end ;
      
