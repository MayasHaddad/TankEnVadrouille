open Field 

type heap = Empty | Node of ((int)*(int*int)) * heap * heap

let init_heap () = ref Empty 

let rec merge h1 h2 = match h1,h2 with 
  |_,Empty -> h1
  |Empty,_ -> h2 
  |Node(r1,h11,h12),Node(r2,h21,h22)-> if fst r1 <= fst r2 then Node(r1,h12,merge h11 h2)
    else Node(r2,h22,merge h21 h1)

let heap_add h e = match h with
  |Empty -> Node(e,Empty,Empty)
  |_ -> merge (Node(e,Empty,Empty)) h

let heap_extract_min h = match !h with 
  |Empty -> raise Not_found
  |Node(r,g,d)->h:= merge g d;r

let heap_is_empty h = if (h = Empty) then true else false 

let init_mat grid a = Array.init (Array.length grid) (fun i -> Array.init (Array.length grid.(i)) (fun _ -> a))

let dijkstra grid start stop = 
 
  let chemin= ref [] in
  let seenlist = ref [] in 
  let mat1 = ref (init_mat grid (0,0)) in 
  let mat = ref (init_mat grid (10000)) in
  let get_dist ij = !mat.(fst ij).(snd ij) in
  let set_dist ij dist = !mat.(fst ij).(snd ij)<-dist in								    
  let theap = init_heap ()  in
    let rec seen ij l = match l with []-> false 
  | a::q -> if ij = a then true else seen ij q
 in
let set_seen ij l = ij::!l
  in
let oracle ij1 ij2 = 
int_of_float (sqrt (float((fst ij2 - fst ij2)*(fst ij2 - fst ij2)+(snd ij2 - snd ij2)*(snd ij2 - snd ij2))))
in 
(** [find] traite les cases une par une *) 
  let rec find ij2 = 
  (** Reste-t-il des cases à voir? *) 
  (** On prend celle qui est le plus près. *) 
  (** On est sûre que son temps n’augmentera pas. *) 
  let ij = try (snd (heap_extract_min theap)) with Not_found -> ij2 in 
  let d = get_dist ij in 

  if ij = ij2 then (** Plus petit chemin trouvé *) 
  ()
  else 
   (** Est-ce qu’on la déjà vu ? *) 
   if seen ij !seenlist then 
    (** Si c’est le cas on en prend pas compte *) 
    find ij2
  else begin
  (** Sinon on le marque et on regarde ces voisins *) 
  set_seen ij seenlist; 
  let neighbors = neighbor ij grid in 
  List.iter (fun vij->
   if not (seen vij !seenlist) then begin 
    match (cost_between (getTypeCase (fst vij) (snd vij) grid)
(getTypeCase (fst ij)(snd ij) grid)) with 
     | None -> (* Ce voisin n’est en faite pas accessible *) ()
     | Some cb -> 
      let dvij = d + cb in 
      (** Est-ce que le nouveau temps est plus petit que l’ancien ? *) 
 if get_dist vij > dvij then begin 
       (** Le chemin pourra être reconstruit en allant de vij vers ij *)  
		!mat1.(fst vij).(snd vij) <- ij;
	set_dist vij dvij; 
       (** vij sera traité avec la priorité dvij (plus petit plus prioritaire) *) 
      theap:= heap_add !theap (dvij+ oracle vij ij2,vij); 
      end
   end 
  ) neighbors;
  find ij2
  end
in let rec parent chemin point=
     if point = start then chemin
     else parent (point::chemin) (!mat1.(fst point).(snd point))
   in 
 theap:=heap_add !theap (0,start);
 !mat.(fst start).(snd start)<-0;
 find stop; 
 (** retourner le plus court chemin *) 
parent [] stop
