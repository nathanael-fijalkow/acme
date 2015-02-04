open Structure
open Automata
open Monoid
open Minimization

(********************************
CHECKING EQUIVALENCE OF MINIMAL STABILIZATION MONOIDS
********************************)

exception Not_Equivalent
exception Finished
  
let equivalence mon1 mon2 =
  let n = mon1.size_m in
  let tab_enum = Array.make n None in
  (* tab_enum.(i) = (x,y) means that x in mon1 and y in mon2 are matched as the i-th element *)
  let tab_checked = Array.init n (fun x -> Array.init n (fun y -> false)) in
  let index = ref 0 in
  
  let rec check (x,y) =
    tab_checked.(x).(y) <- true ;
    let k = ref 0 and boo = ref true in
    while (!k < !index && !boo) do
      match tab_enum.(!k) with
	| Some (a,b) ->
	  boo := a <> x || b <> y ;
	  if (a = x && b <> y) || (a <> x && b = y) then raise Not_Equivalent ;
	  k := !k + 1 ;
	| _ -> failwith "Impossible" ;
    done ;
    if !boo then
      begin 
	tab_enum.(!index) <- Some (x,y) ;
	index := !index + 1 ;
	if !index = n then raise Finished ; 
      end ;
    if not tab_checked.(mon1.stabilization.(x)).(mon2.stabilization.(y)) 
	then check (mon1.stabilization.(x), mon2.stabilization.(y)) ;
    for i = 0 to !index - 1 do
      match tab_enum.(i) with
	| Some (a,b) -> 
	  if not tab_checked.(mon1.product.(a).(x)).(mon2.product.(b).(y)) then check (mon1.product.(a).(x), mon2.product.(b).(y)) ; 
	  if not tab_checked.(mon1.product.(x).(a)).(mon2.product.(y).(b)) then check (mon1.product.(x).(a), mon2.product.(y).(b)) ; 
	| _ -> failwith "Impossible" ;
    done ;
  in 
  
  let check_equ x y =
    let b = ref false and k = ref 0 in
    while ((not !b) && !k < n) do
      match tab_enum.(!k) with
	| Some (x2,y2) -> b := x = x2 && y = y2 ; k := !k+1 ;
	| _ -> raise Not_Equivalent ;
    done ;
  in 
  
  let final_check () =
    for i = 0 to n-1 do
      match tab_enum.(i) with
	| Some (a,b) -> 
          if mon1.stabilization.(a) <> -1 && mon2.stabilization.(b) <> -1 then check_equ mon1.stabilization.(a) mon2.stabilization.(b) ;
	  for j = 0 to n-1 do
	    match tab_enum.(j) with
	      | Some (c,d) -> check_equ mon1.product.(a).(c) mon2.product.(b).(d) ;
	      | _ -> raise Not_Equivalent ;
	  done ;
	| _ -> raise Not_Equivalent
    done ;
  in
  
  try (if (mon1.size_m <> mon2.size_m) then raise Not_Equivalent ;
    check (0,0) ;
    for i = 0 to (Array.length mon1.alpha) - 1 do check (mon1.morphism.(i),mon2.morphism.(i)) done ; 
    false)
  with 
    | Not_Equivalent -> false
    | Finished -> try final_check () ; true
      with | _ -> false
	
let eq_aut aut1 aut2 =
  let m1 = minimize (automata2monoid aut1) and m2 = minimize (automata2monoid aut2) in 
  equivalence m1 m2
    
