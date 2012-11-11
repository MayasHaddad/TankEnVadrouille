open Spath_engine
open Tank
open Tank_view
open Field
open Field_view
open Flag
open Flag_view
open IO
open Sprite

let rec affiche liste = match liste with [] ->()
  |a::q -> Printf.printf "%i , %i | \n" (fst a) (snd a); affiche q

module MyAction = struct

type t =
     |Quit
     |Zoom_in
     |Zoom_out
     |Mov_map_up
     |Mov_map_down
     |Mov_map_right
     |Mov_map_left
     |Mov_tank_up_left
     |Mov_tank_down_left
     |Mov_tank_right
     |Mov_tank_left
     |Mov_tank_up_right
     |Mov_tank_down_right
     |Appear_flag
     |Disappear_flag 
     |Indices of int*int
          
end


module Mouse_Action = struct

type t =Indices of int*int

end


let map = ref "data/plain.map"

let args =
  ["-map", Arg.Set_string map, "set the map to use (default data/plain.map)"]

let usage = "A tank wandering"

let () = Arg.parse args (fun _ -> Arg.usage args usage; exit 1) usage

let carte = Field.getField !map

let grid = Field.from_list_to_grid carte

let taille = ref 0.

let black = Sprite.screenshot ()

let zoom_par = ref 1.

let deb_map_x = ref 1 

let deb_map_y = ref 1

let move_by_it_self = ref false

let liste = ref []

let ll =ref []

let find_case x y =  
  let w = int_of_float(54. *. !zoom_par) and h = int_of_float(48. *. !zoom_par)in
  let ny= y-((!deb_map_y) * h) in
  let nx=  x - ((!deb_map_x) * w) in
  let trouve cord  di =let dif = cord mod di in 
              if ((dif) <= (di/2)) then (cord / di) else (cord / di) +1
  in let i = trouve ny h in let j = match (i mod 2) with 
    |0-> trouve nx w;
    |1-> trouve (nx-int_of_float(27. *. !zoom_par)) w;
                            in MyAction.Indices( i, j) 
                            
let prem_crdn_nn_vid = premier_case_non_vide grid


(* creation d'une occurrence de tank *)
let tank_one = create_tank (fst prem_crdn_nn_vid) (snd prem_crdn_nn_vid)

(* et aussi imediatement le flag qui va avec *)
let flag_one = create_flag (fst prem_crdn_nn_vid) (snd prem_crdn_nn_vid)

let bposition = ref (bonne_position !deb_map_x !deb_map_y !zoom_par tank_one)

let bposition_f = ref !bposition

let deb_tank_x = ref (fst !bposition) 

let deb_tank_y = ref (snd !bposition)

let ar_tank_x = ref (!deb_tank_x)

let ar_tank_y = ref (!deb_tank_y)

let listen = ref true

let focus_on_tank = ref true 

module Action_reader = MakeReader(MyAction)

module Mouse_reader = MakeReader(Mouse_Action)


let bind () =
 (* pour quiter*)
 Action_reader.quit_action MyAction.Quit;
 Action_reader.key_down Action_reader.KEY_ESCAPE MyAction.Quit;

 (* pour zoomer*)
 Action_reader.key_down Action_reader.KEY_p MyAction.Zoom_in;
 Action_reader.key_down Action_reader.KEY_m MyAction.Zoom_out;
 
 (* pour deplacer la carte*)
 Action_reader.key_down Action_reader.KEY_UP MyAction.Mov_map_up;
 Action_reader.key_down Action_reader.KEY_DOWN MyAction.Mov_map_down;
 Action_reader.key_down Action_reader.KEY_RIGHT MyAction.Mov_map_right;
 Action_reader.key_down Action_reader.KEY_LEFT MyAction.Mov_map_left;

 (* pour deplacer le tank *)
 Action_reader.key_down Action_reader.KEY_g MyAction.Mov_tank_left;
 Action_reader.key_down Action_reader.KEY_j MyAction.Mov_tank_right;
 Action_reader.key_down Action_reader.KEY_y  MyAction.Mov_tank_up_left;
 Action_reader.key_down Action_reader.KEY_u  MyAction.Mov_tank_up_right;
 Action_reader.key_down Action_reader.KEY_n  MyAction.Mov_tank_down_right;
 Action_reader.key_down Action_reader.KEY_b  MyAction.Mov_tank_down_left;

(* Pour manipuler le flag *)

 Action_reader.key_auto 50 50 Action_reader.KEY_d  MyAction.Appear_flag;
 Action_reader.key_up Action_reader.KEY_d  MyAction.Disappear_flag;


(* trouver la souris *)
 Action_reader.mouse_down Mouse_reader.BUTTON_LEFT find_case
 
let give_direction case_arriv pos =
  let i = (fst pos) and j =(snd pos) in
     if (i mod 2 = 0) then match (i-fst case_arriv),(j-snd case_arriv) with
                                |1,1   ->Haut_gauche    
                                |0,1   ->Gauche
                                |(-1),1->Bas_gauche
                                |1,0   ->Haut_droite    
                                |0,(-1)->Droite
                                |(-1),0->Bas_droite
                      else match (i-fst case_arriv),(j-snd case_arriv) with
                                |1,0   ->Haut_gauche    
                                |0,1   ->Gauche
                                |(-1),0->Bas_gauche
                                |1,(-1)   ->Haut_droite    
                                |0,(-1)   ->Droite
                                |(-1),(-1)->Bas_droite


let traite_chemain case pos =
  match (give_direction case pos) with 
    | Gauche       -> MyAction.Mov_tank_left
    | Droite       -> MyAction.Mov_tank_right
    | Bas_gauche   -> MyAction.Mov_tank_down_left
    | Bas_droite   -> MyAction.Mov_tank_down_right
    | Haut_gauche  -> MyAction.Mov_tank_up_left
    | Haut_droite  -> MyAction.Mov_tank_up_right

let rec route list_c pos =
    match list_c with 
      |[]->[]
      |a::q -> (traite_chemain a pos)::(route q a)  

let reaction ac_list applicator = 
             match ac_list with
               |[]->()
               |a::q-> applicator a


let action_applicator ac =
 match ac with
                |MyAction.Zoom_in  when !listen && !zoom_par < 8. ->
                                                          change_taille tank_one (tank_one.taille*.2.);
                                                          zoom_par := !zoom_par *. 2.; 
                                                          trans_state_in (!zoom_par)
                |MyAction.Zoom_out  when !listen && !zoom_par > 0.25 ->
                                                               change_taille tank_one (tank_one.taille*.0.5);
                                                               zoom_par := !zoom_par *. 0.5; 
                                                               trans_state_out (!zoom_par)
                |MyAction.Mov_map_up  when !listen -> deb_map_y := !deb_map_y + 1; 
                |MyAction.Mov_map_down  when !listen -> deb_map_y := !deb_map_y - 1;
                |MyAction.Mov_map_right when !listen -> deb_map_x := !deb_map_x - 1;
                |MyAction.Mov_map_left  when !listen -> deb_map_x := !deb_map_x + 1;
                 (* Actions sur tank *)
                 (* Lui donner la position du centre de l'hexagone voisin destination *)
                |MyAction.Mov_tank_left  when !listen ->   if !focus_on_tank then 
		                             begin 
		                              ar_tank_x := !deb_tank_x- int_of_float (!zoom_par *. 54.); 
                                              change_orien tank_one Gauche;
					      let next_i = (fst tank_one.position) and next_j= (snd tank_one.position-1) in 
                                              if (isCaseUtile next_i next_j grid) then                     
						 begin
						  setVitesse tank_one grid next_i next_j; 
						  change_position tank_one (next_i,next_j);	  
						  change_position_flag flag_one (next_i,next_j);
						  listen:=false
						 end	 
					    end 
		                         else let next_i = (fst flag_one.position_f) and next_j= (snd flag_one.position_f-1) in 
			                  if (isCaseUtile next_i next_j grid) then
		                             (change_position_flag flag_one (next_i,next_j))

                |MyAction.Mov_tank_right  when !listen ->if !focus_on_tank then 
		                             begin 
		                           ar_tank_x := !deb_tank_x + int_of_float (!zoom_par *. 54.);
                                           change_orien tank_one Droite;
					    let next_i = (fst tank_one.position) and next_j= (snd tank_one.position+1) in 
                                           if (isCaseUtile next_i next_j grid) then 
					     begin
					        setVitesse tank_one grid next_i next_j;
					   change_position tank_one (next_i,next_j);
					    change_position_flag flag_one (next_i,next_j);
                                             listen:=false
					     end
					        end 
		                         else let next_i = (fst flag_one.position_f) and next_j= (snd flag_one.position_f+1) in 
			                  if (isCaseUtile next_i next_j grid) then
		                             (change_position_flag flag_one (next_i,next_j))

                |MyAction.Mov_tank_up_left when !listen -> if !focus_on_tank then 
		                             begin 
		                             ar_tank_x := !deb_tank_x - int_of_float (!zoom_par *. 27.); 
                                              ar_tank_y := !deb_tank_y - int_of_float (!zoom_par *. 48.);
                                              change_orien tank_one Haut_gauche;
					      (* Pour aller a une case de la ligne suivante de la meme colonne (ou non) on doit prendre en compte la parité de la ligne actuelle *)
					       let next_i = (fst tank_one.position-1) and next_j= (snd tank_one.position-(1-(fst tank_one.position mod 2))) in 
                                           if (isCaseUtile next_i next_j grid) then 
					     begin
					        setVitesse tank_one grid next_i next_j;
					       change_position tank_one (next_i,next_j);
					        change_position_flag flag_one (next_i,next_j);
                                               listen:=false
					     end
					        end 
		                         else let next_i = (fst flag_one.position_f-1) and next_j= (snd flag_one.position_f-(1-(fst flag_one.position_f mod 2))) in 
			                  if (isCaseUtile next_i next_j grid) then
		                             (change_position_flag flag_one (next_i,next_j))

                |MyAction.Mov_tank_up_right  when !listen -> if !focus_on_tank then 
		                             begin 
					       ar_tank_x := !deb_tank_x + int_of_float (!zoom_par *. 27.); 
                                               ar_tank_y := !deb_tank_y - int_of_float (!zoom_par *. 48.);
                                               change_orien tank_one Haut_droite;
					       let next_i = (fst tank_one.position-1) and next_j= (snd tank_one.position+(fst tank_one.position mod 2)) in 
                                           if (isCaseUtile next_i next_j grid) then 
					     begin
					        setVitesse tank_one grid next_i next_j;
					       change_position tank_one (next_i,next_j);
					       change_position_flag flag_one (next_i,next_j);
                                               listen:=false
					     end
					        end 
		                         else let next_i = (fst flag_one.position_f-1) and next_j= (snd flag_one.position_f+(fst flag_one.position_f mod 2)) in 
			                  if (isCaseUtile next_i next_j grid) then
		                             (change_position_flag flag_one (next_i,next_j))

                |MyAction.Mov_tank_down_left  when !listen -> if !focus_on_tank then 
		                             begin 
		                             ar_tank_x := !deb_tank_x -int_of_float (!zoom_par *. 27.);
                                                ar_tank_y := !deb_tank_y + int_of_float (!zoom_par *. 48.); 
                                                change_orien tank_one Bas_gauche;
						let next_i = (fst tank_one.position+1) and next_j= (snd tank_one.position-(1-(fst tank_one.position mod 2))) in 
						if (isCaseUtile next_i next_j grid) then 
						  begin
						     setVitesse tank_one grid next_i next_j;
						    change_position tank_one (next_i,next_j);
						    change_position_flag flag_one (next_i,next_j);
                                                    listen:=false
						  end
						     end 
		                         else let next_i = (fst flag_one.position_f+1) and next_j= (snd flag_one.position_f-(1-(fst flag_one.position_f mod 2))) in 
			                  if (isCaseUtile next_i next_j grid) then
		                             (change_position_flag flag_one (next_i,next_j))

                |MyAction.Mov_tank_down_right  when !listen ->if !focus_on_tank then 
		                             begin 
		                              ar_tank_x := !deb_tank_x + int_of_float(!zoom_par *. 27.); 
                                                 ar_tank_y := !deb_tank_y + int_of_float (!zoom_par *. 48.);
                                                 change_orien tank_one Bas_droite;
						 let next_i = (fst tank_one.position+1) and next_j= (snd tank_one.position+(fst tank_one.position mod 2)) in 
						if (isCaseUtile next_i next_j grid) then 
						  begin
						     setVitesse tank_one grid next_i next_j;
						    change_position tank_one (next_i,next_j);
						    change_position_flag flag_one (next_i,next_j);
                                                    listen:=false
						  end
						     end 
		                         else let next_i = (fst flag_one.position_f+1) and next_j= (snd flag_one.position_f+(fst flag_one.position_f mod 2)) in 
			                  if (isCaseUtile next_i next_j grid) then
		                             (change_position_flag flag_one (next_i,next_j))

		|MyAction.Appear_flag ->  bposition_f := (bonne_position_flag !deb_map_y !deb_map_x !zoom_par flag_one); 
		                          draw_flag flag_one !bposition_f;
					  focus_on_tank := false ;move_by_it_self:=false

                |MyAction.Indices(i,j)->if isCaseUtile i j grid 
                                        then begin 
                                             liste := dijkstra grid tank_one.position (i,j);
                                             ll := (route !liste (tank_one.position));
                                             draw_flag2 !deb_map_y !deb_map_x i j !zoom_par;
                                             focus_on_tank := true;move_by_it_self :=true
				             end
					 
		|MyAction.Disappear_flag ->affiche ( dijkstra grid tank_one.position flag_one.position_f);
                  liste := dijkstra grid tank_one.position flag_one.position_f;
                  ll := (route !liste (tank_one.position));
		  change_position_flag flag_one (fst tank_one.position,snd tank_one.position);
		  focus_on_tank := true ;
                  move_by_it_self :=true

                |MyAction.Quit->exit 10 
                |_->()


let loop ()  = 
while (true) do
  draw black 0 0;     
    Field_view.draw_array_map grid !zoom_par !deb_map_x !deb_map_y; 
     	bposition := (bonne_position !deb_map_y !deb_map_x !zoom_par tank_one);
     
         
     (* listen = true ==> le tank n'est pas en mouvement, lire au clavier *)
    if !listen then 
      begin
	(* mise à jour des references de x_debut et y_debut et ar_x et ar_y*)
      deb_tank_x := (fst !bposition);
      deb_tank_y := (snd !bposition);
      ar_tank_x := (!deb_tank_x);
      ar_tank_y := (!deb_tank_y);

      draw_tank tank_one !bposition;

      if !move_by_it_self = true then begin                                
                                 reaction !ll action_applicator;
                                 if  !ll <> [] then ll:=List.tl !ll
                                 else move_by_it_self :=false;   
      end;
              
           let l = Action_reader.read() in reaction l action_applicator;
          
      
      end 

  else 
      begin
	slide_tank tank_one deb_tank_x deb_tank_y ar_tank_x ar_tank_y listen !zoom_par;
        draw_tank tank_one (!deb_tank_x,!deb_tank_y);
       end;
  
     
 update ();
  frame_delay 100
done
