let init_mat grid = Array.init (Array.length grid) (fun i -> Array.init (Array.length grid.(i)) (fun _ -> (-1)))

let init_dep mat ij = mat.(fst i).(snd j) := 0

let seen mat ij = (mat.(fst i).(snd j) > (-1))

let set_dist mat ij dist = mat.(fst i).(snd j):=dist 

 
let rec fill_dists ij = 
  let voisins = neighbors ij in
  List.iter (fun vij-> 
    match (cost_between ij vij) with 
      | 0 -> (* Ce voisin n’est en faite pas accessible *) () 
      | Some cb -> 
 let dist = d + cb in 
     (** Est-ce que le nouveau temps est plus petit que l’ancien ? *) 
 if get_dist vij > dist || get_dist vij = (-1) then begin 
         set_dist mat vij dvij;  
 end 
   fill_dists vij;
  end ) voisins


let dijkstra grid start stop = 
  let m = (init_mat grid) in 
  init_dep m ;
  fill_dists start
