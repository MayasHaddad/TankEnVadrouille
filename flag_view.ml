open Flag
open IO
open Sprite

let flag = Sprite.load ~align:Center "data/red-flag.png"  

let draw_flag f position =  
 let x= fst(position) and y = snd(position) in
 draw flag x y

let draw_flag2 deb_map_i deb_map_j i j zoom =  
 let flag_pos debi debj i j zoom =
 if i mod 2 = 0 then (((debj+j)*int_of_float(54.*.zoom)),((debi+i)*int_of_float(48.*.zoom))) 
                else (((debj+j)*int_of_float(54.*.zoom))+int_of_float(27.*.zoom),((debi+i)*int_of_float(48.*.zoom)))in
  let position= flag_pos deb_map_i deb_map_j i j zoom in
 let x= fst(position) and y = snd(position) in
 draw (rotozoom flag 0. zoom)  x y


        
