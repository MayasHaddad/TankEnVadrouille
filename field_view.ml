(*ouverture des modules *)
open IO
open Sprite
open Field

let () = init 800 600
(*chargement des images d'hexagonne *)
let copie_ref x =ref (!x)

let verdure =ref (Sprite.load ~align:Center "data/hexa_green.png")(* chargement le vert*)
let bou =ref (Sprite.load ~align:Center "data/hexa_orange.png")(*le maron*)
let new_state_verdure = copie_ref verdure
let new_state_bou = copie_ref bou


let black = Sprite.screenshot ()(* ecran noir*)

(*affiche un hexagone*)
                        

let draw_hex a x y = match a with 
                    |P-> draw !new_state_verdure x y 
                    |M-> draw !new_state_bou x y
                    |_-> ()

(*------------------------------------------------fonction pour liste de liste-------------------------------------- *)
(*affiche une ligne de la liste*)
let draw_line l y x zoom_par = 
                  let rec liner ll yy acc zoom_par=
                                 match ll with
                                  |[]->()
                                  |a::q-> draw_hex a acc yy;liner q yy (acc+int_of_float(54.*.zoom_par)) zoom_par
                  in liner l y x zoom_par




(*affiche la carte en utilisant la liste de liste*)
let  draw_map map zoom_par =
     let rec tracer m acc zoom_par =
          match m with
           |[]->()
           |a::q->if acc mod 2 = 0 then draw_line a (acc*int_of_float(45.*.zoom_par)) 54 zoom_par
                                   else 
                                    draw_line a (acc*int_of_float(45.*.zoom_par)) (int_of_float(54.+.27.*.zoom_par)) zoom_par;
                                    tracer q (acc+1) zoom_par
                                  
           in tracer map 0 zoom_par                 
(*----------------------------------------------------fonction pour la matrice carte---------------------------------------*)

let draw_array_map carte zoom_par x y =
    for i=0 to Array.length carte-1 do
         for j=0 to Array.length carte.(i) -1 do
         if i mod 2 = 0 then draw_hex (carte.(i)).(j) ((x+j)*int_of_float(54.*.zoom_par))  ((y+i)*int_of_float(48.*.zoom_par)) 
                        else draw_hex (carte.(i)).(j) ((x+j)*int_of_float(54.*.zoom_par)+int_of_float(27.*.zoom_par))  ((y+i)*int_of_float(48.*.zoom_par))
         done  
    done       
(*------------------------------------------------------fonction autre-----------------------------------------------------*)
(*tronsforme un hexagone en forme zoomé is good for this time*)

let trans_state_in zoom_par  =
        if zoom_par =1. then new_state_verdure := !verdure
                        else new_state_verdure := rotozoom !new_state_verdure 0. 2.;
        if zoom_par =1. then new_state_bou := !bou
                        else new_state_bou := rotozoom !new_state_bou 0. 2.

let trans_state_out zoom_par  =
        if zoom_par = 1. then new_state_verdure := !verdure
                         else new_state_verdure := rotozoom !new_state_verdure 0. 0.5;
        if zoom_par = 1. then new_state_bou := !bou
                         else new_state_bou := rotozoom !new_state_bou 0. 0.5
