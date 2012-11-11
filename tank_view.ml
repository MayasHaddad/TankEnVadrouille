(*ouverture des modules *)
open IO
open Sprite
open Tank

let blinde = Sprite.load ~align:Center "data/tank.png" (*image du tank*)

let draw_tank t position=
  let x= fst(position) and y = snd(position) in
  let image=
    match t.orien with
      |Bas_gauche ->  rotozoom blinde 145.  (t.taille)
      |Bas_droite -> rotozoom blinde (-148.)  (t.taille) 
      |Haut_gauche -> rotozoom blinde 33.  (t.taille) 
      |Haut_droite -> rotozoom blinde (-33.) ( t.taille)
      |Gauche ->  rotozoom blinde 90. (t.taille)
      |Droite -> rotozoom blinde (-90.)  (t.taille) 
  in draw image x y
