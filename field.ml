type hexagone = P|M|E (* type des hexagone du terain*)

(* cree une liste a partir d'un string *)
let create_list_from_line ligne=
 let rec add_car ligne cpt list = if cpt <= 0 then list
                              else 
                               match ligne.[cpt-1] with
                                               |'P'-> add_car ligne (cpt-1) (P::list)
                                               |'M'-> add_car ligne (cpt-1) (M::list)
                                               |'E'-> add_car ligne (cpt-1) (E::list)
                                               |_-> add_car ligne (cpt-1) list (** ignore when has other value *)
      in add_car ligne (String.length ligne) []

(* retourne une liste de liste d'hexagone (carte)*)
let getField file =  let canal_entree = open_in file  
in let rec read_channel canal_entree map =
 try 
    let ligne = input_line canal_entree
    in let list = create_list_from_line ligne in 
     read_channel canal_entree (list::map)
 with End_of_file -> begin
                     close_in canal_entree;
                      List.rev  map      
                      end 
 in read_channel canal_entree []
(*------------------------------------------------*)

let from_list_to_grid carte =
    Array.of_list (List.map Array.of_list carte)

(* Pas de carte vide ou contenant que des Empty *)
(* retourne l'indice de la premiere case dont la valeur est differente de Empty *)
let premier_case_non_vide carte = let i = ref 0 and j = ref 0 in 
while carte.(!i).(!j) = E do j:=!j+1; if !j = (Array.length carte.(!i)) then begin j:=0; i:=!i+1 
end 
 done;
(!i,!j)

let getTypeCase i j carte = carte.(i).(j)

let isCaseUtile i j carte = if (i>=0 && i< Array.length carte) && (j>= 0 && j<Array.length carte.(i)) then 
				  let case = carte.(i).(j) in case  =P || case = M
				else false


let cost_between c1 c2 = match c1,c2 with 
  |P,P->Some 1
  |M,M->Some 3
  |_,E->None
  |_,_->Some 2

let neighbor case grid= 
  let l = ref [] in 
if isCaseUtile (fst case) (snd case+1) grid then 
l:=(fst case,snd case+1)::!l;
if isCaseUtile (fst case+1) (snd case+(fst case mod 2)) grid then 
l:=(fst case+1,snd case+(fst case mod 2))::!l;
if isCaseUtile (fst case+1) (snd case-(1-(fst case mod 2))) grid then 
l:=(fst case+1,snd case-(1-(fst case mod 2)))::!l;
if isCaseUtile (fst case) (snd case-1) grid then 
l:=(fst case,snd case-1)::!l;
if isCaseUtile (fst case-1) (snd case-(1-(fst case mod 2))) grid then 
l:=(fst case-1,snd case-(1-(fst case mod 2)))::!l;
if isCaseUtile (fst case-1) (snd case+(fst case mod 2)) grid then 
l:=(fst case-1,snd case+(fst case mod 2))::!l;!l

