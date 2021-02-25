(*A.1*)
let rec replicate n r = 
  match n with
  | n when n < 1 -> []
  | _ -> r :: replicate (n - 1) r

let rec repln n l =
  match l with
  | [] -> []
  | h :: t -> replicate n h @ repln n t

let rec repln2 n l =
  match n with
  | n when n < 1 -> []
  | _ -> l @ repln2 (n - 1) l

let repln3 n l = 
  List.concat (replicate n l)

(*A.2*)

let group l =
  if l = [] then [] else
  let rec group_iter curr_i l lst_i  =
    match l with
    |[] -> [lst_i]
    |h :: t when h != curr_i -> lst_i :: group_iter h t ([h])
    |h :: t -> group_iter h t (h :: lst_i)
in group_iter (List.hd(l)) l []


(*A.3*)

let rle lst =
  match lst with
  |[] -> []
  |x :: _ -> 
  let rec rle_iter l lst_i  =
    match l with
    |[] -> [lst_i]
    |h :: t when h != fst lst_i -> lst_i :: rle_iter t ((h, 1))
    |h :: t -> rle_iter t ((h, snd lst_i + 1))
in rle_iter lst (x, 0)

let rle2 l =
  List.map (fun x -> List.hd(x), List.length x) (group l)

let rec rld = function
 | [] -> []
 | h :: t -> replicate (snd h) (fst h) @ rld t

 (*A.4*)
 let make_grouper init update pred lst =
  match lst with
  |[] -> []
  |x :: y -> 
  let rec iter_make_grouper l lst_i =
  match l with
  |[] -> [lst_i]
  |h :: t when pred h lst_i = false -> lst_i :: iter_make_grouper t (init h)
  |h :: t -> iter_make_grouper t (update h lst_i)
in iter_make_grouper y (init (x))

let group2 lst =
  make_grouper
    (fun h -> [h])                    (* init function *)
    (fun h curr -> h :: curr)         (* update function *)
    (fun h curr -> h = List.hd curr)  (* pred function *)
    lst

let rle3 lst =
  make_grouper
    (fun h -> (h, 1))                    (* init function *)
    (fun h curr -> (h, snd curr + 1))         (* update function *)
    (fun h curr -> h = fst curr)  (* pred function *)
    lst

(*A.5*)

let remove_nth i l =
  let rec iter_remove_nth i l c acc =
    match l with
    | l when i < 0 -> invalid_arg "Exception: Invalid_argument remove_nth"
    | [] -> failwith "Exception: Failure nth element not present"
    | h :: t when c = i -> (h, acc @ t)
    | h :: t -> iter_remove_nth i t (c + 1) (acc @ [h])
  in iter_remove_nth i l 0 []

let rec shuffle l =
  match l with
  |[] -> []
  | l -> let t = remove_nth 
  (Random.int(List.length(l))) l in fst t :: shuffle (snd t)

(*asymptotic time question
The asymptotic time complexity of the shuffle function is O(n^2) as the method
remove_nth is O(n) and it is being called n times. What this means is we are calling
the remove_nth method (which on average goes through half the remaining list because the random
number) List.length(l) times. So it acts as almost a nested loop that goes through
the smaller list each time until there is no list left.
*)

(*A.6*)
let gray_codes n =
  let rec iter_gray_codes n l =
  match n with
  | n when n < 1 -> invalid_arg "Exception: Invalid_argument gray_codes"
  | n when n = 1 -> l
  | _ -> iter_gray_codes (n - 1) 
  (List.map (fun x -> 0 :: x) l @ List.map (fun x -> 1 :: x) (List.rev(l)))
  in iter_gray_codes n [[0]; [1]]

(*B.1*)
type tree =
  | Leaf
  | Node2 of tree * int * tree   (* left tree, value, right tree *)
  | Node3 of tree * int * tree * int * tree
    (* left tree, left value, middle tree, right value, right tree *)

let rec tree_search i = function
  |Leaf -> false
  |Node2 (_, j, _) when i = j -> true
  |Node3 (_, j, _, k, _) when i = j || i = k -> true
  |Node2 (t1, j, _) when i < j -> tree_search i t1
  |Node2 (_, j, t2) when i > j -> tree_search i t2
  |Node3 (t1, j, _, _, _) when i < j -> tree_search i t1
  |Node3 (_, j, t2, k, _) when i > j && i < k -> tree_search i t2
  |Node3 (_, _, _, k, t3) when i > k -> tree_search i t3

(*B.2*)
type insertion =
| Ok of tree                   (* we didn't have to split a node *)
| Split of tree * int * tree   (* we had to split a node *)

let rec insert_helper i t =
match t with

  (* Base cases. *)

  | Leaf -> Ok (Node2 (Leaf, i, Leaf))

  | Node2 (_, j, _) when i = j -> Ok t  (* i is already in tree *)
  | Node3 (_, j, _, k, _) when i = j || i = k -> Ok t  (* ditto *)

  | Node2 (Leaf, j, Leaf) when i < j ->   (* add i to tree; change 2-node to 3-node *)
      Ok (Node3 (Leaf, i, Leaf, j, Leaf))
  | Node2 (Leaf, j, Leaf) ->   (* i > j *)
      Ok (Node3 (Leaf, j, Leaf, i, Leaf))

  | Node3 (Leaf, j, Leaf, k, Leaf) when i < j ->  (* split; watch the order! *)
      Split (Node2 (Leaf, i, Leaf), j, Node2 (Leaf, k, Leaf))
  | Node3 (Leaf, j, Leaf, k, Leaf) when i > j && i < k ->
      Split (Node2 (Leaf, j, Leaf), i, Node2 (Leaf, k, Leaf))
  | Node3 (Leaf, j, Leaf, k, Leaf) ->   (* i > k *)
      Split (Node2 (Leaf, j, Leaf), k, Node2 (Leaf, i, Leaf))

  (* Recursive cases. *)

  | Node2 (t1, j, t2) when i < j ->  (* insert into t1 *)
      begin
        match insert_helper i t1 with
          | Ok t1' -> Ok (Node2 (t1', j, t2))
          | Split (t1a, i', t1b) -> Ok (Node3 (t1a, i', t1b, j, t2))
      end

  | Node2 (t1, j, t2) ->  (* i > j; insert into t2 *)
      begin
        match insert_helper i t2 with
          | Ok t2' -> Ok (Node2 (t1, j, t2'))
          | Split (t2a, i', t2b) -> Ok (Node3 (t1, j, t2a, i', t2b))
      end

  | Node3 (t1, j, t2, k, t3) when i < j ->  (* insert into t1 *)
      begin
        match insert_helper i t1 with
          | Ok t1' -> Ok (Node3 (t1', j, t2, k, t3))
          | Split (t1a, i', t1b) ->  Split(Node2(t1a, i', t1b), j, Node2 (t2, k, t3))
      end

  | Node3 (t1, j, t2, k, t3) when i > j && i < k ->  (* insert into t2 *)
      begin
        match insert_helper i t2 with
          | Ok t2' -> Ok (Node3 (t1, j, t2', k, t3))
          | Split (t2a, i', t2b) ->  Split(Node2(t1, j, t2a), i', Node2 (t2b, k, t3))
          
      end

  | Node3 (t1, j, t2, k, t3) ->  (* i > k; insert into t3 *)
      begin
        match insert_helper i t3 with
          | Ok t3' -> Ok (Node3 (t1, j, t2, k, t3'))
          | Split (t3a, i', t3b) ->  Split(Node2(t1, j, t2), k, Node2 (t3a, i', t3b))
      end

let tree_insert i t =
match insert_helper i t with
  | Ok t' -> t'
  | Split (t1, j, t2) -> Node2 (t1, j, t2)