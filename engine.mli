module MyAction : sig 
  type t 
     
end

val action_applicator :  MyAction.t -> unit

val reaction : 'a list -> ('a -> unit) -> unit 

val loop : unit -> unit

val bind : unit -> unit   
