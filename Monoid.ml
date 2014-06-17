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
    
type sharp_expression = Char of int | Sharp of sharp_expression | Concat of sharp_expression list


(* printing functions *)
    
let rec logten n = if n < 10 then 1 else 1 + logten (n/10)
    
let rec print_int_pretty k n =
  if k > 0 then (print_int_pretty (k-1) (n/10) ; print_int (n mod 10))
    
let print_monoid monoid = 
  let n = monoid.size_m in
  let k = logten n in
  let m = Array.length monoid.matrix.(0) in
  print_string "The monoid has " ; print_int n ; print_string " elements.\nHere is a list:\n" ;
  for i = 0 to n-1 do
    print_string monoid.name.(i) ; print_newline() ; print_string (print_matrix monoid.structure m monoid.matrix.(i)) ; print_string monoid.attribute.(i) ; print_string "\n\n" ;
  done ;
  print_string "\nThe morphism is:\n" ;
  Array.iteri 
    (fun i -> 
      (fun a -> 
	print_char a ; print_string " : " ; print_int_pretty k monoid.morphism.(i) ; print_newline() ;
      )
    )
    monoid.alpha ;
  print_string "\nThe product matrix is:\n" ;
  for j = 1 to k+3 do print_string " " ; done ;
  for j = 0 to n-1 do
    print_int_pretty k j ; print_string " " ;
  done ;
  print_newline() ;
  for j = 1 to (n * (k + 1) + k + 2) do print_string "_" ; done ;
  for i = 0 to n-1 do
    print_newline() ; print_int_pretty k i ; print_string " | " ;
    for j = 0 to n-1 do
      print_int_pretty k monoid.product.(i).(j) ; print_string " " ;
    done ;
  done ;
  print_string "\n\nThe stabilization is:\n" ;
  for i = 0 to n-1 do
    print_int_pretty k i ; print_string " : " ; if monoid.stabilization.(i) = -1 then print_string "undefined" else print_int_pretty k monoid.stabilization.(i) ; print_newline() ;
  done 
    
(* Structure-depend functions *)

let check m automaton = 
let n = automaton.size in match automaton.structure.type_s with
    | P ->
      let b = ref true and i = ref 0 and j = ref 0 in
      let next () = if (!j = n - 1) then (i := !i + 1 ; j := 0) else j := !j + 1 in
      while (!b && !i < n) do
	if automaton.initial.(!i)
	then (b := m.(!i).(!j) <> 2 || automaton.final.(!j) ; next ())
	else (i := !i + 1)
      done ;
      let ergodic i = 
	let erg = ref true and j = ref 0 in
	while (!erg && !j < n) do
	  erg := m.(i).(!j) <> 2 || m.(!j).(i) = 2 ;
	  j := !j + 1 ;
	done;
	!erg 
      in
      let b2 = ref false in
      for j = 0 to n-1 do
	if ergodic j then (for i = 0 to n-1 do b2 := !b2 || (m.(j).(i) = 1 && m.(i).(j) <> 2) done)
      done ;
      if !b then (* value 1 witness *)
	begin
	  if !b2 
	  then "n1" (* leak *)
	  else "l1" (* not a leak *)
	end
      else (* not a value 1 witness *)
	if !b2
	then "n0" (* leak *)
	else "l0" (* not a leak *)
    | B1 ->
      let b_om = ref false and b_acc = ref false in
      for i = 0 to n - 1 do
	if automaton.initial.(i) then
	  for j = 0 to n - 1 do
	    if automaton.final.(j) then 
	      begin
		if m.(i).(j) = 3 then b_om := true ;
		if m.(i).(j) < 3 then b_acc := true ;
	      end
	  done ;
      done ;
      if (!b_acc || (not !b_om)) then "l" else "n" ; (* "l" stands for limited *)     
    | BN ->
      let b_om = ref false and b_acc = ref false in
      for i = 0 to n - 1 do
	if automaton.initial.(i) then
	  for j = 0 to n - 1 do
	    if automaton.final.(j) then 
	      begin
		if m.(i).(j) = 3 then b_om := true ;
		if m.(i).(j) < 3 then b_acc := true ;
	      end
	  done ;
      done ;
      if (!b_acc || (not !b_om)) then "l" else "n" ; (* "l" stands for limited *)
    | _ -> failwith "Error"
      
let update_info info i structure =
  match structure.type_s with
    | P -> if i = "l0" then info
      else if i = "l1" then (if info = "n0" || info = "n1" then "n1" else "l1")
      else if i = "n0" then (if info = "l1" || info = "n1" then "n1" else "n0")
      else "n1" 
    | B1 | BN -> if i = "l" then info else "n"  (* "l" stands for limited *)
    | _ -> failwith "Error"
      
let init_info structure  =
  match structure.type_s with
    | P ->  "l0" (* leaktight and no value 1 witness *)
    | B1 | BN -> "l" (* limited *)
    | _ -> failwith "Error" 
      
let rec print_se automaton = function
  | Char i -> String.make 1 automaton.alphabet.(i)
  | Concat [] -> "epsilon"
  | Concat l -> let s = ref "" in List.iter (fun exp -> s := !s^(print_se automaton exp)) l ; !s
  | Sharp (Char i) -> (String.make 1 automaton.alphabet.(i))^"^#"
  | Sharp s -> "("^(print_se automaton s)^")^#"
    
(* The main function, automata2monoid *)    
    
let prime = 32609
  
let hash m = (Hashtbl.hash m) mod prime
  
let automata2monoid automaton =
  let n = automaton.size in
  let k = Array.length automaton.alphabet in
  let structure = automaton.structure in
  let prod_here = structure.prod_matrix n in
  let equal_here = equal_matrix n in
  let stab_here = structure.stab_matrix n in
    
  let mat_id = Array.init n (fun i -> Array.init n (fun j -> if i = j then structure.one else structure.zero))
  in  
  
  let info = ref (init_info structure) in
  let tab_monoid = Array.make 10000 (mat_id, Concat [], !info) and ind_max = ref 0 and tab_hash = Array.make prime [] in
  let queue = Queue.create () in Queue.add (Concat [],mat_id) queue ;
  
for j = 0 to (Array.length automaton.alphabet) - 1 do
	Queue.add (Char j, automaton.transition.(j)) queue ;
done ;
    
  let insert (m,se) = 
(*    print_string (print_se automaton se) ; print_newline() ; print_string (print_matrix structure n m) ; print_newline() ;  *)
    let i = check m automaton in (tab_monoid.(!ind_max) <- (m,se,i) ; info := update_info !info i structure) ;
    ind_max := !ind_max + 1 ;
    let h = hash m in tab_hash.(h) <- m :: tab_hash.(h) ;
  in
    
  let rec treat () = 
    try 
      let (s,m) = Queue.take queue in
      let h = hash m in
      if not (List.mem m tab_hash.(h)) then 
	begin 
	  insert (m,s) ;	  
	  let m_square = prod_here m m in
    	  if (equal_here m m_square) 
    	  then let p = stab_here m in let h = hash p in if not (List.mem p tab_hash.(h)) then Queue.add (Sharp s, p) queue ;
          else let h = hash m_square in if not (List.mem m_square tab_hash.(h)) then Queue.add (Concat [s ; s], m_square) queue ;

	  for i = 0 to !ind_max - 1 do
  	let (n,sn,_) = tab_monoid.(i) in
	let p = prod_here m n in let h = hash p in if not (List.mem p tab_hash.(h)) then Queue.add (Concat [s ; sn], p) queue ;
	let p = prod_here n m in let h = hash p in if not (List.mem p tab_hash.(h)) then Queue.add (Concat [sn ; s], p) queue ;
	  done ;
	end ;      
      treat () ;
      
    with Queue.Empty -> ()
  in
  
  treat () ;
  
  (* post treatment *)

  let find m =
    let rec foo i =
	if i = !ind_max then print_string "oooops" ; 
      let (m',_,_) = tab_monoid.(i) in 
      if equal_here m m' then i else foo (i+1)
    in foo 0
  in
  
  let morph = Array.init k (fun i -> find automaton.transition.(i))
  in

  let mat_prod = Array.init !ind_max (fun _ -> Array.make !ind_max (-1)) in
  for i = 0 to !ind_max - 1 do
    let (m,s,_) = tab_monoid.(i) in
(*     print_string "i : " ; print_string (print_se automaton s) ; print_newline() ; print_string (print_matrix structure n m) ; print_newline() ; print_newline() ; print_newline() ; print_newline() ; *)
    for j = 0 to !ind_max - 1 do
      let (m',s',_) = tab_monoid.(j) in
(*    print_string "j : " ; print_string (print_se automaton s') ; print_newline() ; (* print_string (print_matrix structure n m') ; print_newline() ; *) *)
(*    print_string "prod : " ; print_newline() ; print_string (print_matrix structure n (prod_here m m')) ; print_newline() ; *)
      let k = find (prod_here m m') in
      mat_prod.(i).(j) <- k ;
    done ;
  done ;

  let vect_stab = Array.make !ind_max (-1) in
  let rec fill i m = 
    let m_square = prod_here m m in
    if (equal_here m m_square) 
    then (let p = stab_here m in let j = find p in vect_stab.(i) <- j)
    else (let j = find m_square in (if vect_stab.(j) <> -1 then fill j m_square ; vect_stab.(i) <- vect_stab.(j)))
  in
  for i = 0 to !ind_max - 1 do
    let (m,_,_) = tab_monoid.(i) in fill i m ;
  done ;
  
  let vect_ideal = Array.make !ind_max false and vect_attribute = Array.make !ind_max "" and vect_name = Array.make !ind_max "" and vect_matrix = Array.make !ind_max automaton.transition.(0) in
  for i = 0 to !ind_max - 1 do
    let (m,s,inf) = tab_monoid.(i) in 
    begin
      vect_name.(i) <- print_se automaton s ;
      vect_matrix.(i) <- m ;
      match structure.type_s with
	| P -> 
	  vect_ideal.(i) <- (inf = "l0" || inf = "n0") ; (* not a value 1 witness *)
	  vect_attribute.(i) <- if inf = "n0" then "leak" else if inf = "n1" then "leak and value 1 witness" else if inf = "l1" then "value 1 witness" else "" ;
	| B1 | BN -> vect_ideal.(i) <- inf = "l" (* limited *)
	| _ -> failwith "Error" ;
    end ;
  done ;
  
  {
    alpha          = automaton.alphabet ;
    size_m         = !ind_max ;
    morphism       = morph ; 
    product        = mat_prod ;
    stabilization  = vect_stab ;
    ideal          = vect_ideal ;
    structure      = automaton.structure ;
    matrix         = vect_matrix ;
    name           = vect_name ;
    attribute      = vect_attribute ;
    info           = !info ;
  }

