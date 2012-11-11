type hexagone = P|M|E

val getField : string -> hexagone list list

val from_list_to_grid : 'a list list -> 'a array array

val premier_case_non_vide : hexagone array array -> int*int

val getTypeCase : int -> int -> hexagone array array -> hexagone

val isCaseUtile : int -> int -> hexagone array array -> bool

val cost_between : hexagone -> hexagone -> int option 

val neighbor : int*int -> hexagone array array ->(int*int) list
