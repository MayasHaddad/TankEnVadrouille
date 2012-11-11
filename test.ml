open IO
open Sprite

let () = init 200 200

let tank = Sprite.load ~align:Center "data/mire.png"
let black = Sprite.screenshot ()


let () =
  for i=0 to 40 do
    draw black 0 0;
    draw (rotozoom tank (float_of_int (i*90)) 8.) 100 100;
    update ();
    frame_delay 100
  done
