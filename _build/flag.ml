type flag = {mutable position_f : int*int; mutable taille_f : int} (* Position en terme d'indice dans la matrice de la carte, les coordonnées étant calculés juste pour l'affichage *)

let create_flag i j = 
  {position_f = (i,j); taille_f = 1} 

let change_position_flag f new_position = 
  f.position_f <- new_position

(* rend une coordonnée de flag coherente à partir d'indices de sa case *)
let bonne_position_flag deb_map_i deb_map_j zoom f = 
let i = fst f.position_f and j = snd f.position_f
    in if i mod 2 = 0 then (((deb_map_j+j)*int_of_float(54.*.zoom)),((deb_map_i+i)*int_of_float(48.*.zoom))) 
                        else (((deb_map_j+j)*int_of_float(54.*.zoom))+int_of_float(27.*.zoom),((deb_map_i+i)*int_of_float(48.*.zoom)))
