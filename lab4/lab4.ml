(*A.1*)
type point = {x : float; y : float}
type segment = {startp : point; endp : point}

let make_point x y = {x; y}

let get_coords p = (p.x, p.y)

let make_segment startp endp = {startp; endp}

let get_points s = (s.startp, s.endp)

let midpoint_segment s =
  let x = (s.startp.x +. s.endp.x) /. 2. in
  let y = (s.startp.y +. s.endp.y) /. 2. in
  {x ; y}

let segment_length s=
  let x_length = s.startp.x -. s.endp.x in
  let y_length = s.startp.y -. s.endp.y in
  sqrt (x_length *. x_length +. y_length *. y_length)

let print_point p =
  Printf.printf "%f, %f" p.x p.y

(*A.2*)
type rectangle = {llc : point; urc : point}

let rectangle_lower_segment rect =
  let startp = rect.llc in
  let endp = {x=rect.urc.x; y = rect.llc.y} in
  {startp; endp}

let rectangle_upper_segment rect =
  let startp = {x=rect.llc.x; y = rect.urc.y} in
  let endp = rect.urc in
  {startp; endp}

let rectangle_left_segment rect =
  let startp = rect.llc in
  let endp = {x=rect.llc.x; y = rect.urc.y} in
  {startp; endp}

let rectangle_right_segment rect =
  let startp = {x=rect.urc.x; y = rect.llc.y} in
  let endp = rect.urc in
  {startp; endp}

let rectangle_perimeter rect =
  segment_length(rectangle_left_segment rect) +.
  segment_length(rectangle_right_segment rect) +.
  segment_length(rectangle_upper_segment rect) +.
  segment_length(rectangle_lower_segment rect)

let rectangle_area rect =
  segment_length(rectangle_right_segment rect) *.
  segment_length(rectangle_upper_segment rect) 

type rectangle2 = {lx : float; ux : float; ly : float; uy : float}

let rectangle_lower_segment2 rect =
  let startp = {x=rect.lx; y=rect.ly} in
  let endp = {x=rect.ux; y = rect.ly} in
  {startp; endp}

let rectangle_upper_segment2 rect =
  let startp = {x=rect.lx; y=rect.uy} in
  let endp = {x=rect.ux; y = rect.uy} in
  {startp; endp}

let rectangle_left_segment2 rect =
  let startp = {x=rect.lx; y=rect.ly} in
  let endp = {x=rect.lx; y = rect.uy} in
  {startp; endp}

let rectangle_right_segment2 rect =
  let startp = {x=rect.ux; y=rect.ly} in
  let endp = {x=rect.ux; y = rect.uy} in
  {startp; endp}

let rectangle_perimeter2 rect =
  segment_length(rectangle_left_segment2 rect) +.
  segment_length(rectangle_right_segment2 rect) +.
  segment_length(rectangle_upper_segment2 rect) +.
  segment_length(rectangle_lower_segment2 rect)
  
let rectangle_area2 rect =
  segment_length(rectangle_right_segment2 rect) *.
  segment_length(rectangle_upper_segment2 rect)
  
let make_rectangle llc urc = {llc ; urc}

let make_rectangle2 lx ly ux uy = {lx; ly; ux; uy}

(*A.3*)

let make_pair x y = fun m -> m x y
let first z = z (fun x y -> x)
let second z = z (fun x y -> y)

(*
For first (make_pair x y) I first checked in the console to make sure it was
right (it was 1) but then checked the code. So first (make_pair x y) ->
first (fun m -> m x y) -> (fun x y -> x) x y which then goes x y and finally
evaluates to x

Evaluate second (make_pair 1 2)
  evaluate make_pair 1 2
    evaluate 1 -> 1
    evaluate 2 -> 2
    evaluate make_pair
      fun m -> m x y
      subsitute 1 for x and 2 for y
        fun m -> m 1 2
        second (fun m -> m 1 2)
          evaluate second (fun m -> m 1 2)
            evaluate 1 -> 1
            evaluate 2 -> 2
            evaluate second
              z (fun x y -> y)
              subsitute (fun m -> m 1 2) for z
               (fun m -> m 1 2) (fun x y -> y)
               evaluate (fun m -> m 1 2) -> (fun m -> m 1 2)
               evaluate (fun x y -> y) -> (fun x y -> y)
               subsitute (fun x y -> y) for m
                (fun x y -> y) 1 2
                  evaluate 1 -> 1
                  evaluate 2 -> 2
                  evaluate (fun x y -> y) -> (fun x y -> y)
                    subsitute 1 for x and 2 for y
                    fun 1 2 -> 2
                     evaluate 1 -> 1
                     evaluate 2 -> 2
                      Result: 2

*)

(*A.4*)

let pow a b =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter a b n =
    match n with
    | 0 -> a
    | (n) when is_even n -> iter a (square b) (n / 2)
    | (_) -> iter (a * b) b (n - 1)
  in iter 1 a b

let int_log a b =
  let rec iter_int_log a b c =
    match b with
    | 1 -> c
    | b when b mod a = 0 -> iter_int_log a (b/a) (c+1)
    | _ -> c
  in iter_int_log a b 0

let make_pairi a b = (pow 2 a) * (pow 3 b)

let firsti p = int_log 2 p

let secondi p = int_log 3 p

(*A.5*)
let zero = []

let is_zero = function
  | [] -> true
  | () :: _ -> false

let succ u = () :: u

let prev = function
  |[] -> invalid_arg "Must be greater than 0"
  |_ :: t -> t

let rec integer_to_unary a =
  if a = 0
    then zero
  else succ (integer_to_unary (a - 1))

let rec unary_to_integer u =
  if u = zero
    then 0
  else 1 + unary_to_integer (prev u)

let rec unary_add u1 u2 =
  if u1 = zero
    then u2
  else unary_add (List.tl u1) ((List.hd u1) :: u2)


type nat = Zero | Succ of nat

let zero' = Zero
  
let is_zero' = function
  | Zero -> true
  | Succ _ -> false
  
let succ' u = Succ u

let prev' = function
  |Zero -> invalid_arg "Must be greater than 0"
  |Succ u -> u

let rec integer_to_unary' a =
  if a = 0
    then zero'
  else succ' (integer_to_unary' (a - 1))

let rec unary_to_integer' u =
  if u = zero'
    then 0
  else 1 + unary_to_integer' (prev' u)

(*I wasn't sure how else to do it other than convert, add then convert back
because there are no longer list operations*)
let rec unary_add' u1 u2 =
  integer_to_unary'( unary_to_integer' u1 + unary_to_integer' u2)

(*I only had to change the last defintion because there was no longer list
operations so I had to do it a different way. I already wrote above I'm not
sure if this is allowed but its the only way I could think of*)


(*A.6*)
let zerof = fun s -> fun z -> z
let add1 n = fun s -> fun z -> s (n s z)

let one = fun s -> fun z -> s z
let two = fun s -> fun z -> s (s z)
let three = fun s -> fun z -> s (s (s z))
let four = fun s -> fun z -> s (s (s (s z)))
let five = fun s -> fun z -> s (s (s (s (s z))))
let six = fun s -> fun z -> s (s (s (s (s (s z)))))
let seven = fun s -> fun z -> s (s (s (s (s (s (s z))))))
let eight = fun s -> fun z -> s (s (s (s (s (s (s (s z)))))))
let nine = fun s -> fun z -> s (s (s (s (s (s (s (s (s z))))))))
let ten = fun s -> fun z -> s (s (s (s (s (s (s (s (s (s z)))))))))

let add m n s z = m s (n s z)

let church_to_integer n = n (fun num -> 1 + num) 0

(*A.7*)
(*

val church_to_integer : ((int -> int) -> (int -> 'c)) -> 'c = <fun>
val zerof : 'a -> ('b -> 'b) = <fun>
val one : ('a -> 'b) -> ('a -> 'b) = <fun>

When doing church_to_integer zerof we get 'a is the (int -> int) as
when substituting it is the function (fun num -> 1 + num), then 'b
is an int as in church_to_integer (int -> 'c) goes to the ('b -> 'b)
in zerof, thus type 'c must also be an int.

When doing church_to_integer one we get 'a is an int and 'b is an int
when subsituting for (int -> int) in church_to_integer, then 'b is
an int as in church_to_integer (int -> 'c) goes to ('a -> 'b) in one,
thus type 'c must also be an int.

*)

(*B.1.a*)
type mobile = Mobile of branch * branch  (* left and right branches *)
and branch =
  | Weight    of int * int     (* length and weight *)
  | Structure of int * mobile  (* length and sub-mobile *)

let make_mobile l r = Mobile (l, r)
let make_weight l w = Weight (l, w)
let make_structure l m = Structure (l, m)

let left_branch (Mobile (l, _)) = l

let right_branch (Mobile (_, r)) = r

let branch_length = function
  |Weight (l, _) -> l
  |Structure (l, _) -> l
let branch_structure = function
  |Weight (_, w) -> `Weight w
  |Structure (_, m) -> `Structure m

(*B.1.b*)

let rec branch_weight1 = function
  |Weight (_, w) -> w
  |Structure (_, m) -> total_weight1 m
  and total_weight1 (Mobile (l, r)) =
    branch_weight1 l + branch_weight1 r

let rec branch_weight2 b =
  let b_struct = branch_structure b in
  match b_struct with
    |`Weight w -> w
    |`Structure m -> total_weight2 m
  and total_weight2 m =
    branch_weight2 (left_branch m) + branch_weight2 (right_branch m)

(*B.1.c*)

let rec is_balanced m =
  let is_branch_balanced b =
    let b_struct = branch_structure b in
    match b_struct with
    |`Weight w -> true
    |`Structure m -> is_balanced m
  in is_branch_balanced (left_branch m) && is_branch_balanced (right_branch m) &&
  (branch_weight2 (left_branch m) * branch_length (left_branch m) = branch_weight2 (right_branch m) * branch_length (right_branch m))

(*B.1.d*)
type mobile'  = { left: branch'; right: branch' }
and  branch'  = Branch' of int * contents
and  contents = Weight' of int | Structure' of mobile'

let make_mobile' left right = {left; right}
let make_weight' l w = Branch' (l, (Weight' w))
let make_structure' l m = Branch' (l, (Structure' m))

let left_branch' {left} = left

let right_branch' {right} = right

let branch_length' (Branch' (l, _)) = l
let branch_structure' (Branch' (_, c)) =
  match c with
  |Weight' w -> `Weight w
  |Structure' m -> `Structure m

let rec branch_weight' b =
  let b_struct = branch_structure' b in
  match b_struct with
    |`Weight w -> w
    |`Structure m -> total_weight' m
  and total_weight' m =
    branch_weight' (left_branch' m) + branch_weight' (right_branch' m)

let rec is_balanced' m =
  let is_branch_balanced b =
    let b_struct = branch_structure' b in
    match b_struct with
    |`Weight w -> true
    |`Structure m -> is_balanced' m
  in is_branch_balanced (left_branch' m) && is_branch_balanced (right_branch' m) &&
  (branch_weight' (left_branch' m) * branch_length' (left_branch' m) = branch_weight' (right_branch' m) * branch_length' (right_branch' m))


(*B.2*)
type tree = Tree of elem list
and elem =
  | Num of int
  | Sub of tree

let rec square_tree (Tree l) =
  let rec square_inner_list = function
    | [] -> []
    | (Num num) :: t -> (Num (num * num)) :: square_inner_list t
    | (Sub sub_tree) :: t -> (Sub (square_tree sub_tree)) :: square_inner_list t
  in Tree (square_inner_list l)

let rec square_tree' (Tree l) =
  let square_number = function
    | Num num -> Num (num * num)
    | Sub sub_tree -> Sub (square_tree' sub_tree)
  in Tree(List.map square_number l)

(*B.3*)

let rec tree_map f (Tree l) =
  let function_on_num = function
    | Num num -> Num (f num)
    | Sub sub_tree -> Sub (tree_map f sub_tree)
  in Tree(List.map function_on_num l)

(*C.1*)

type expr =
  | Int of int           (* constant *)
  | Var of string        (* variable *)
  | Add of expr * expr   (* expr1 + expr2 *)
  | Mul of expr * expr   (* expr1 * expr2 *)
  | Pow of expr * int    (* expr^n *)

let rec simplify1 expr =
  match expr with
    |Int _
    |Var _ -> expr
    |Add (Int a, Int b) -> Int(a + b)
    |Mul (Int a, Int b) -> Int(a * b)
    |Pow (Int a, b) -> Int(pow a b)
    |Add (Int 0, rest_of_e) -> rest_of_e
    |Add (rest_of_e, Int 0) -> rest_of_e
    |Mul (Int 0, _) -> Int 0
    |Mul (_, Int 0) -> Int 0
    |Mul (Int 1, rest_of_e) -> rest_of_e
    |Mul (rest_of_e, Int 1) -> rest_of_e
    |Pow (expr, 0) -> Int 1
    |Pow (expr, 1) -> expr
    |Add (expr1, expr2) -> Add (simplify1 expr1, simplify1 expr2)
    |Mul (expr1, expr2) -> Mul (simplify1 expr1, simplify1 expr2)
    |Pow (expr, p) -> Pow (simplify1 expr, p)


(*C.2*)

let rec deriv var expr =
  match expr with
    |Int _ -> Int 0
    |Var v when v = var -> Int 1
    |Var _ -> Int 0
    |Add (expr1, expr2) -> Add (deriv var expr1, deriv var expr2)
    |Mul (expr1, expr2) -> Add ( (Mul ((deriv var expr1), expr2)), (Mul (expr1, (deriv var expr2))))
    |Pow (expr, p) -> Mul (Int p, Mul (Pow (expr, p - 1), deriv var expr))
