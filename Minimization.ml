open Structure
open Automata
open Monoid

let rec classe tab i =
  if tab.(i) = i then i 
  else let j = classe tab tab.(i) in tab.(i) <- j ; j
    
let minimize monoid =
  let n = monoid.size_m in
  
  let nb_classes = ref 0 in
  let init_tab_partition () =
    let tab_partition = Array.make n 0 in
    let min_in_ideal = ref None and min_out_ideal = ref None in
    let rec foo i =
      if i < n then
	begin
	  if monoid.ideal.(i) 
	  then 
	    (match !min_in_ideal with
	      | None -> min_in_ideal := Some i ; nb_classes := !nb_classes + 1 ; tab_partition.(i) <- i ; foo (i+1)
	      | Some j -> tab_partition.(i) <- j ; foo (i+1))
	  else 
	    (match !min_out_ideal with
	      | None -> min_out_ideal := Some i ; nb_classes := !nb_classes + 1 ; tab_partition.(i) <- i ; foo (i+1)
	      | Some j -> tab_partition.(i) <- j ; foo (i+1))
	end
      else tab_partition
    in foo 0
  in
  
  let tab_partition = init_tab_partition () in
  let m = 2*(n+1) in
  let type_vect = Array.init n (fun _ -> Array.make m 0) in
  
  let update i =
    type_vect.(i).(0) <- classe tab_partition i ;
    type_vect.(i).(1) <- classe tab_partition monoid.stabilization.(i) ;
    for j = 0 to n-1 do
      type_vect.(i).(j+2) <- classe tab_partition monoid.product.(i).(j) ;
      type_vect.(i).(j+2+n) <- classe tab_partition monoid.product.(j).(i) ;
    done ;
  in
  
  let vect_equal t1 t2 =
    let rec foo i = (i = m) || (t1.(i) = t2.(i) && foo (i+1)) in foo 0
  in
  
  let nv_tab_partition = Array.make n 0 and b = ref true and new_nb_classes = ref 0 in
  while(!b) do
    new_nb_classes := n ;
    for i = 0 to n-1 do nv_tab_partition.(i) <- i ; update i done ;
    for i = 0 to n-2 do
      for j = i+1 to n-1 do
	if (classe nv_tab_partition i) <> (classe nv_tab_partition j) then
	  if vect_equal type_vect.(i) type_vect.(j) then 
	    (nv_tab_partition.(classe nv_tab_partition i) <- j ; new_nb_classes := !new_nb_classes - 1)
      done ;
    done ;
    b := !new_nb_classes > !nb_classes ;
    for i = 0 to n-1 do tab_partition.(i) <- nv_tab_partition.(i) done ;
    nb_classes := !new_nb_classes ;
  done ;
  
  let tab_map = Array.make n (-1) and tab_inverse = Array.make !nb_classes (-1) and vect_name = Array.make !nb_classes "" and vect_attribute = Array.make !nb_classes "" in
  let i = ref 0 and b = ref false in
  for j = 0 to n-1 do
    b := false ;
    for k = 0 to n-1 do
      if classe tab_partition k = j then 
	begin
	b := true ; 
	tab_map.(k) <- !i ; 
	tab_inverse.(!i) <- j ; 
	vect_attribute.(!i) <- monoid.attribute.(k) ; 
	vect_name.(!i) <- if (vect_name.(!i) = "" || String.length vect_name.(!i) > String.length monoid.name.(k)) then monoid.name.(k) else vect_name.(!i) ; 
	end ;
    done ;
    if !b then i := !i + 1 ;
  done ;
  
  let vect_morphism = Array.init (Array.length monoid.alpha) (fun i -> tab_map.(monoid.morphism.(i))) in
  let mat_prod = Array.init !nb_classes (fun i -> Array.init !nb_classes (fun j -> tab_map.(classe tab_partition (monoid.product.(tab_inverse.(i)).(tab_inverse.(j)))))) in
  let vect_stab = Array.init !nb_classes (fun i -> tab_map.(classe tab_partition (monoid.stabilization.(tab_inverse.(i))))) in
  let vect_ideal = Array.init !nb_classes (fun i -> monoid.ideal.(tab_inverse.(i))) in
  let vect_matrix = Array.init !nb_classes (fun i -> monoid.matrix.(tab_inverse.(i))) in
  {
    alpha 	   = monoid.alpha ;
    size_m	   = !nb_classes ;
    morphism       = vect_morphism ;
    product        = mat_prod ;
    stabilization  = vect_stab ;
    ideal          = vect_ideal ;
    structure      = monoid.structure ;
    matrix         = vect_matrix ;
    name           = vect_name ;
    attribute      = vect_attribute ;
    info           = monoid.info ;
  }

