(* Part A. *)

val replicate : int -> 'a -> 'a list
val repln : int -> 'a list -> 'a list
val repln2 : int -> 'a list -> 'a list
val repln3 : int -> 'a list -> 'a list
val group : 'a list -> 'a list list
val rle : 'a list -> ('a * int) list
val rle2 : 'a list -> ('a * int) list
val rld : ('a * int) list -> 'a list
val make_grouper :
  ('a -> 'b) -> ('a -> 'b -> 'b) -> ('a -> 'b -> bool) -> 'a list -> 'b list
val group2 : 'a list -> 'a list list
val rle3 : 'a list -> ('a * int) list
val remove_nth : int -> 'a list -> 'a * 'a list
val shuffle : 'a list -> 'a list
val gray_codes : int -> int list list

(* Part B. *)

type tree =
    Leaf
  | Node2 of tree * int * tree
  | Node3 of tree * int * tree * int * tree

val tree_search : int -> tree -> bool
type insertion = Ok of tree | Split of tree * int * tree
val insert_helper : int -> tree -> insertion
val tree_insert : int -> tree -> tree

