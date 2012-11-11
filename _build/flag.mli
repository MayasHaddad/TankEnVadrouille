type flag= {mutable position_f: int*int; mutable taille_f : int}

val create_flag : int -> int -> flag

val change_position_flag : flag -> int*int -> unit

val bonne_position_flag : int -> int -> float -> flag -> int*int
