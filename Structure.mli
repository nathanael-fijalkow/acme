type selector = C | P | B1 | BN 

type structure = {
  type_s      : selector ; (* type of the semiring: probabilistic, one counter, several counters ... *)
  size_s      : int ; (* number of different actions *)
  ideal_s     : bool array ; 
  print_s     : string array ;

  zero        : int ;
  one         : int ;

  prod_matrix   : int -> int array array -> int array array -> int array array ;
  stab_matrix      : int -> int array array -> int array array ;
}

val print_matrix : structure -> int -> int array array -> string
val equal_matrix : int -> int array array -> int array array -> bool

val probabilistic_structure : structure
val boolean_structure : structure
val one_counter_structure : structure
val multiple_counter_structure : structure

