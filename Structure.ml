type selector = C | P | B1 | BN 

type structure = {
  type_s      : selector ; (* type of the semiring: probabilistic, one counter, several counters ... *)
  size_s      : int ; (* number of different actions *)
  ideal_s     : bool array ; 
  print_s     : string array ;
  
  zero        : int ;
  one         : int ;
  
  prod_matrix   : int -> int array array -> int array array -> int array array ;
  stab_matrix   : int -> int array array -> int array array ;
}
    
    
let print_matrix structure n matrix = 
  let s = ref "" in
  for i = 0 to n-1 do
    for j = 0 to n-1 do
      s := !s^(structure.print_s.(matrix.(i).(j)))^" " ;
    done ;
    s := !s^"\n" ;
  done ;
  !s
    
let equal_matrix n ma mb = 
  let b = ref true and i = ref 0 and j = ref 0 in
  while(!b && !i < n) do
    b := ma.(!i).(!j) = mb.(!i).(!j) ;
    if (!j = n - 1) then (i := !i+1 ; j := 0) else j := !j+1 ;
  done ;
  !b
    
(* Probabilistic structure *)

let product_matrix_prob n ma mb =
  let m = Array.init n
    (fun i -> Array.init n
      (fun j -> 
	let b = ref 0 and k = ref 0 in
	while(!b <> 2 && !k < n) do
	  b := max !b (min ma.(i).(!k) mb.(!k).(j)) ;
	  k := !k + 1;
	done ;
	!b ;
      )
    ) 
  in 
  m
    
let stabilization_matrix_prob n m = 
  let ergodic i = 
    let erg = ref true and j = ref 0 in
    while (!erg && !j < n) do
      erg := m.(i).(!j) <> 2 || m.(!j).(i) = 2 ;
      j := !j + 1 ;
    done;
    !erg 
  in
  let m_sharp = Array.init n (fun _ -> Array.make n 0) in 
  for j = 0 to n-1 do
    if ergodic j
	then (for i = 0 to n-1 do m_sharp.(i).(j) <- m.(i).(j) done)
	else (for i = 0 to n-1 do m_sharp.(i).(j) <- min 1 m.(i).(j) done)
  done ;
  m_sharp

let probabilistic_structure = {
  type_s               = P ;
  size_s               = 3 ;
  ideal_s              = [|false;false;true|] ;
  print_s              = [|"_";"+";"1"|] ;
  
  zero                 = 0 ;
  one                  = 2 ;
  
  prod_matrix          = product_matrix_prob ;
  stab_matrix          = stabilization_matrix_prob ;
}
  
let boolean_structure = {
  type_s               = C ;
  size_s               = 2 ;
  ideal_s              = [|false;true|] ;
  print_s              = [|"_";"1"|] ;

  zero                 = 0 ;
  one                  = 1 ;
  
  prod_matrix          = product_matrix_prob ;
  stab_matrix          = (fun _ -> fun m -> m) ;
}

(* One counter structure *)
  
let mat_p = 
[| [|0;0;0;3;4|] ;
   [|0;1;2;3;4|] ;
   [|0;2;2;3;4|] ;
   [|3;3;3;3;4|] ;
   [|4;4;4;4;4|] |]

let product_matrix_one_counter n ma mb =
  let m = Array.init n
    (fun i -> Array.init n
      (fun j -> 
	let b = ref 4 and k = ref 0 in
	while(!k < n && !b <> 0) do
	  b := min !b mat_p.(ma.(i).(!k)).(mb.(!k).(j)) ;
	  k := !k + 1;
	done ;
	!b ;
      )
    ) 
  in m
  
let tab_stab = [|0;1;3;3;4|]
  
let stabilization_matrix_one_counter n m = 
  let m_sharp = Array.init n
    (fun i -> Array.init n 
      (fun j -> 
	let b = ref 4 and k = ref 0 in
	while(!k < n && !b <> 0) do
	  b := min !b mat_p.(m.(i).(!k)).(mat_p.(m.(!k).(j)).(tab_stab.(m.(!k).(!k)))) ;
	  k := !k + 1 ;
	done ;
	!b ;
      )
    )
  in m_sharp
  
let one_counter_structure = {
  type_s               = B1 ;
  size_s               = 5 ;
  ideal_s              = [|true;true;true;false;false|] ;
  print_s              = [|"R";"E";"I";"O";"_"|] ;

  zero                 = 4 ; (* no transition *)
  one                  = 1 ; 
 
  prod_matrix          = product_matrix_one_counter ;
  stab_matrix          = stabilization_matrix_one_counter ;
}

(* Multiple counter structure *)

(*
(* Atomic actions *)
type b_act = E | IC | R

(* Table of atomic ations, i.e. action of several counters *)
type t_act = TSTOP | TOM | TAB of (b_act array)

type act = STOP | OM | L of (t_act list)

let bact_to_string = function
	| E -> "E"
	| IC -> "I"
	| R -> "R"

let act_to_string nbcount = function
  | L [TAB t] -> if nbcount = 0 then "1 " else 
      let s = ref "" in Array.iter (fun a -> s := !s ^ (bact_to_string a)) t ; !s 
  | _ -> "0 "

let print_act nbcount = function
	| STOP -> if nbcount = 0 then print_string "0 " else for i = 1 to nbcount do print_string "_" done
	| OM -> for i = 1 to nbcount do print_string "O" done
	| L [TAB t] -> print_string (act_to_string nbcount (L [TAB t])) ; 
	| L l -> print_int (List.length l)

let prod_Bact act1 act2 = match act1 with
  | R -> R
  | E -> act2
  | IC -> if act2 = E then IC else act2 ;;

let best_Bact act1 act2 = match act1 with
  | IC -> act2
  | E -> if act2 = R then R else E
  | R -> R ;;

(* act1 better or equal to act2 *)
let order_Bact act1 act2 = (best_Bact act1 act2) = act1 ;;

let detab = function 
	| TOM -> failwith "compression error OM" 
	| TSTOP -> failwith "compression error STOP"
	| TAB t -> t ;;

let compare_tact tact1 tact2 = match (tact1,tact2) with 
	| (TAB y1, TAB y2) -> 
	let b = ref true and i = ref 0 and n = Array.length y1 in 
		while (!b && !i < n) do 
			b := order_Bact y1.(!i) y2.(!i) ;
			i := !i + 1 ;
		done ;
	!b
	| _ -> failwith "comparison TOM or TSTOP" ;;
			
let  delist act = match act with L l -> l | _ -> failwith "delist fail" ;;
  
(* adding a single t-action in an action *)
let rec inser tact act = match tact with
  | TSTOP -> if act = L [] then STOP else act
  | TOM -> if (act=STOP || act=L []) then OM else act
  | _ -> match act with 
	|L [] -> L [tact]
	|L (h::q) -> if compare_tact h tact then L (h::q) else 
				  if compare_tact tact h then L (tact::q) (* assuming lists are always antichains *)
				  else L (h::(delist (inser tact (L q))))
	| _ -> L [tact] ;;

(* union of paths, removing inefficient ones *)
let best_act act1 act2 = match act1 with
  | STOP -> act2
  | OM -> if act2 = STOP then OM else act2
  | _ -> match act2 with
		| STOP -> act1
		| OM -> act1
		| _ -> List.fold_left (fun lst -> fun tact -> inser tact lst) act1 (delist act2)

		
let prod_tact tact1 tact2=match tact1 with
	|TSTOP -> TSTOP
	|TOM -> if tact2=TSTOP then TSTOP else TOM
	|TAB t1 -> match tact2 with
		| TAB t2->let n = Array.length t1 in let tres=Array.copy t1 in
				for i=0 to n-1 do tres.(i)<-prod_Bact t1.(i) t2.(i) done;
				TAB tres
		| x -> x
		
(* path concatenation *)
let prod_act act1 act2 = match act1 with
	| STOP -> STOP
	| OM -> if act2 = STOP then STOP else OM
	| L l1->match act2 with
		|STOP -> STOP
		|OM -> OM
		|L l2 -> let rec produits lst1 lst2=match (lst1,lst2) with
			|(_,[])->STOP
			|([],t::q)-> produits l1 q
			|((t1::q1),(t2::q2))->inser (prod_tact t1 t2) (produits q1 lst2)
			in produits l1 l2
			
let stab_tact = function
	| TAB t->    let n=Array.length t and i=ref 0 in
				while ( !i < n && t.(!i) <> IC) do i:= !i+1 done;
				if !i = n then TAB t else TOM
	| x->x

(* on idempotents *)
let stab_act = function 
	|L (t::q) -> inser (stab_tact t) (L q)
	|x -> x 
		
let bounded_act act = (act <> OM) && (act <> STOP)
let accepting_act act = (act <> STOP) ;; (* boolean version, ignoring values *)

let stab n_aut m = 
  let m_sharp = Array.init n_aut 
    (fun i -> Array.init n_aut 
      (fun j -> 
	let b = ref STOP and k = ref 0 in
	while(!k < n_aut) do
	  b := best_act !b (prod_act m.(i).(!k) (prod_act (stab_act m.(!k).(!k)) m.(!k).(j))) ;
	  k := !k+1 ;
	done ;
	!b ;
      )
    )
  in m_sharp

  let check_ideal aut m =
    let b = ref false and i = ref 0 and j = ref 0 in
    while(not !b && !i < n_aut && !j < n_aut) do
      if not (aut.initial.(!i)) then (i := !i+1 ; j := 0)
      else if not (aut.final.(!j)) then (if (!j = n_aut - 1) then (i := !i+1 ; j := 0) else j := !j+1)
      else (b := bounded_act m.(!i).(!j) ; if (!j = n_aut - 1) then (i := !i+1 ; j := 0) else j := !j+1 ;)
    done ;
    !b 
*)

let multiple_counter_structure = {
  type_s               = BN ;
  size_s               = 5 ;
  ideal_s              = [|true;true;true;false;false|] ;
  print_s              = [|"R";"E";"I";"O";"_"|] ;

  zero                 = 4 ; (* no transition *)
  one                  = 1 ; 
 
  prod_matrix          = product_matrix_one_counter ;
  stab_matrix          = stabilization_matrix_one_counter ;
}

