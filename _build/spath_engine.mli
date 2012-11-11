type heap =  Empty | Node of ((int)*(int*int)) * heap * heap

val init_heap : unit -> heap ref

val merge : heap -> heap -> heap

val heap_add : heap -> ((int)*(int*int)) -> heap

val heap_extract_min : heap ref -> ((int)*(int*int))

val heap_is_empty : heap -> bool

val init_mat : 'a array array -> 'b -> 'b array array

val dijkstra : Field.hexagone array array -> int*int -> int*int -> (int*int) list
