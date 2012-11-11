
type orientation = Bas_gauche|Bas_droite|Haut_gauche|Haut_droite|Droite|Gauche

type tank = {mutable position : int*int ; mutable orien : orientation; mutable taille : float; mutable vitesse : int}

val create_tank : int -> int -> tank

val change_orien : tank -> orientation -> unit

val change_taille : tank -> float -> unit

val change_position : tank -> int*int -> unit

val bonne_position : int -> int -> float -> tank -> int*int  

val slide_tank : tank -> int ref -> int ref -> int ref -> int ref -> bool ref ->float ->  unit

val setVitesse : tank -> Field.hexagone array array-> int -> int -> unit 
