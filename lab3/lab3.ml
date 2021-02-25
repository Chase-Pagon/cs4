(*A.1*)
let rec last_sublist = function
 |[] -> invalid_arg "last_sublist: empty list"
 |[x] -> [x]
 |h :: t -> last_sublist t

(*A.2*)
let reverse l =
  let rec reverse_iter l curr_l =
     match l with
      | [] -> curr_l
      | h :: t -> reverse_iter t (h::curr_l)
  in reverse_iter l [];;

(*A.3*)
let rec square_list = function
  | [] -> []
  | h :: t -> h*h :: square_list t

let square_list2 items = List.map (fun x -> x*x) items

(*A.4.1*)
(* If we look at the reverse function we wrote we can see the same operation occuring.
The function is getting the head of the list then adding it to an empty list then repeating
the process. So each time the new head of the old list is added to the front of the new list, reversing
the order similar to the reverse function we wrote.*)

(*A.4.2*)
(*He is then trying to add an entire list to front of an element which is not something supported by ocaml.
To fix this we could use the append function @*)
let square_list items =
  let rec iter things answer =
    match things with
      | [] -> answer
      | h :: t -> iter t (answer @ [(h * h)])
  in iter items []
(*The resulting function would not be very effecient as we are creating an entirely new list
for each element and then using append which is not a efficient function*)

(*A.5*)
let count_negative_numbers l =
  let rec cne_iter l c =
     match l with
      | [] -> c
      | h :: t when h < 0 -> cne_iter t (c+1)
      | h :: t -> cne_iter t c
  in cne_iter l 0;;

(*A.6*)
let power_of_two_list n =
  let rec pow b n =
    let is_even m = m mod 2 = 0 in
    let square m = m * m in
      match n with
      | (n) when n = 0 -> 1
      | (n) when is_even n -> square (pow b (n / 2))
      | (_) -> b * pow b (n - 1)
  in
  let rec poftl_iter n l =
    match n with
    | n when n < 0 -> l 
    | _ -> pow 2 n :: poftl_iter (n - 1) l
  in reverse(poftl_iter (n-1) [])

(*A.7*)

let prefix_sum l =
  let rec ps_iter l curr_l s =
    match l with
    | [] -> curr_l
    | h :: t -> (h+s) :: ps_iter t curr_l (h+s)
  in ps_iter l [] 0

(*A.8*)

let deep_reverse l =
  let rec deep_reverse_iter l curr_l =
    match l with
     | [] -> curr_l
     | h :: t -> deep_reverse_iter t ((reverse h)::curr_l)
 in deep_reverse_iter l []

 (*A.9*)
 type 'a nested_list =
 | Value of 'a
 | List of 'a nested_list list

 let rec deep_reverse_nested l =
	let rec deep_reverse_nested_iter l curr_l =
		match l with
			| [] -> curr_l
			| h :: t -> deep_reverse_nested_iter t ((deep_reverse_nested h) :: curr_l)
	in
		match l with
			| Value i -> Value i
			| List l -> List (deep_reverse_nested_iter l []) 

 (*B.1*)

let rec quicksort l cmp =
  match l with
 | [] -> []
 | h :: t -> let left = List.filter (fun x -> cmp x h) t in let right = List.filter (fun x -> not (cmp x h)) t
in quicksort left cmp @ (h :: quicksort right cmp)

(*B.2*)
(*The quicksort function is an instance of generative recursion because with each recursion it is calling it on
smaller subparts of itself each time. Instead structural recursion which would just be adding an element to the
next recursion. Essentially there are multiple different threads of recursion which then combine to be one*)

(*B.3*)
(*Essentially it creates an infinite loop. The infinite loop is caused by trying to split a list
with length one into [x] and []. But the issue here is it then calls the function again and again
it will try to split it into [x] and [] because there is no case that checks if the array length is 1*)

(*B.4*)
let rec insert_in_order new_result a_list cmp =
  match a_list with
    | [] -> [new_result]
    | h :: t when cmp new_result h -> new_result :: a_list
    | h :: t ->  h :: insert_in_order new_result t cmp

let rec insertion_sort a_list cmp =
  match a_list with
    | [] -> []
    | h :: t -> insert_in_order h (insertion_sort t cmp) cmp

(*This time the recursion is a structural recursion because each time we call the function recursively
we are just adding a value to the head of the list. We are recursing on the tail
which then just gets appened at the end of the recursive process. Essentially
h :: h :: h... :: h until the last 'head' is added.*)

(*C.1*)
let rec subsets =
  function
   | [] -> [[]]
   | h :: t ->
     let rest = subsets t
     in rest @ (List.map (fun x -> h :: x) rest)

(*Each time the recursive function is called we are splitting the list between the
values of the head and the tail. Then we get all of the sublists of the tail and
append it to the head of the list. Thus we have all the combinations of the various
sublists of the tails appended to the head. The process of seperating the head and
the tail and getting all of the sublists of the tail is then repeated until their is
nothing left in the tail returning [[]] ending the recursive call*)

(*C.2*)

let rec accumulate op initial sequence =
  match sequence with
    | [] -> initial
    | h :: t -> op h (accumulate op initial t)

let map p sequence =
  accumulate (fun x r -> (p x) :: r) [] sequence

let append seq1 seq2 =
  accumulate (fun x r -> x :: r) seq2 seq1

let length sequence =
  accumulate (fun x r -> 1 + r) 0 sequence

(*C.3*)
let rec accumulate_n op init seqs =
  match seqs with
    | [] -> failwith "empty list"
    | [] :: _ -> []   (* assume all sequences are empty *)
    | h :: t -> accumulate op init (map List.hd seqs) :: accumulate_n op init (map List.tl seqs)

(*C.4*)

let rec map2 f x y =
  match (x, y) with
    | ([], []) -> []
    | ([], _) -> failwith "unequal lists"
    | (_, []) -> failwith "unequal lists"
    | (xh::xt, yh::yt) -> (f xh yh) :: map2 f xt yt

let dot_product v w = accumulate (+) 0 (map2 ( * ) v w)

let matrix_times_vector m v = map (fun row -> (dot_product v row)) m

let transpose mat = accumulate_n (fun x y -> x :: y) [] mat

let matrix_times_matrix m n =
  let cols = transpose n in
     map (fun row -> (matrix_times_vector cols row)) m
