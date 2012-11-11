open Field 

type orientation = Bas_gauche|Bas_droite|Haut_gauche|Haut_droite|Droite|Gauche

type tank = {mutable position : int*int ; mutable orien : orientation; mutable taille : float; mutable vitesse : int }

let create_tank i j = (* i et j etant des indices de cases *)
    {position = (i,j); orien = Droite; taille = 1.; vitesse = 1}

let change_orien t new_orien =
    t.orien <- new_orien

let change_taille t new_taille =
    t.taille <- new_taille

let change_position t new_position = 
    t.position <- new_position

(* rend une coordonnée a partir d'indices de cases *)
let bonne_position deb_map_i deb_map_j zoom t = 
let i = fst t.position and j = snd t.position
    in if i mod 2 = 0 then (((deb_map_j+j)*int_of_float(54.*.zoom)),((deb_map_i+i)*int_of_float(48.*.zoom))) 
                      else (((deb_map_j+j)*int_of_float(54.*.zoom))+int_of_float(27.*.zoom),((deb_map_i+i)*int_of_float(48.*.zoom)))



let setVitesse t carte i_dest j_dest  = 
let vitesse = match (getTypeCase i_dest j_dest carte),(getTypeCase (fst t.position) (snd t.position) carte) with
                                        |P,P ->3
					|M,M ->1
					|_,_ ->2
					in t.vitesse <- vitesse

(* rapproche le tank de sa position finale, appellée plusieurs fois dans une boucle elle
 constitue l'animation de deplacement du tank *)

let slide_tank t initial_x initial_y final_x final_y listen  zoom =
  let diff_x = !initial_x - !final_x 
      and diff_y= !initial_y - !final_y
      and vitesse = if zoom >= 1. then int_of_float(float(t.vitesse)*.zoom) else t.vitesse
          in if diff_x =0 && diff_y= 0 then listen:=true
             else 
                 begin
                   if diff_x <= (-1)*vitesse then
	             initial_x:=(!initial_x)+vitesse
                   else 
	             if diff_x >= vitesse then initial_x:=(!initial_x)-vitesse
                     else initial_x := !final_x;

                   if diff_y <= (-2)*vitesse then  
                     initial_y := (!initial_y)+2*vitesse
                   else 
	             if diff_y>= 2*vitesse then
                       initial_y:=(!initial_y)-2*vitesse
	             else initial_y:=!final_y;
                 end

