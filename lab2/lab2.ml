open Num

let ni = num_of_int     (* convert int -> num *)

(*A.1
The space complexity for the fib function is O(n). This is because ocaml is using applicative-order evaluation. So each fib(n - 1) is evaluated before a single f(n - 2) is
calculated. Using the fib 7 example there are only ever 5 pending operations (fib (6) - fib(2)) that come from the fib (n - 2) part of the function. So each
time fib evaluates one of the fib (n - 1) its memory is freed up thus only ever being O(n) max operations pending*)

(*A.2.1
5 times*)

(*A.2.2
The number of times p is run can be written as a function of a and n where a/3^n < 0.1. 
For example it is seen that 12/3^5 < 0.1 but 12/3^4 </ (isn't less than) .1. Whenever a function is 1/some number^a it can be expressed as a logarithmic function as
the larger the number is, the more the divide by three makes the number small. For example 23 and 12 (n=5) have the name number of iterations and so do 24 and 72 (n=6).
Thus the growth of space it takes up can be expressed as O(logA) since the growth formula is lograthmic. There is then a similar logic for the growth of steps
(or time complexity) as the function is run a/3^n < 0.1 times or O(logA) after some rearrangement*)

(*A.3.a*)
let rec fast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
    match n with
    | 0 -> 1
    | (n) when is_even n -> square (fast_expt b (n / 2))
    | (_) -> b * fast_expt b (n - 1)

(*A.3.b*)
let ifast_expt b n =
  let is_even m = m mod 2 = 0 in
  let square m = m * m in
  let rec iter a b n =
    match n with
    | 0 -> a
    | (n) when is_even n -> iter a (square b) (n / 2)
    | (_) -> iter (a * b) b (n - 1)
  in iter 1 b n

(*A.4*)
let rec fast_mult a b =
  let double x = x + x in
  let halve y = y / 2 in
  match b with
  | 0 -> 0
  | b when b mod 2 = 0 -> fast_mult (double a) (halve b)
  | (_) -> a + fast_mult a (b - 1)

(*A.5*)
  let ifast_mult a b =
    let double x = x + x in
    let halve y = y / 2 in
    let rec ifast_mult_helper x y z =
    match y with
    | 0 -> z
    | y when y mod 2 = 0 -> ifast_mult_helper (double x) (halve y) z
    | (_) -> ifast_mult_helper x (y - 1) (x + z)
    in ifast_mult_helper a b 0

(*A.6
The worst time complexity is O(n) and space complexity of O(logN). By the functions definition it is a
recursive tree since it is foo f (n / 2) + foo f (n / 2). We can then calculate the depth of the tree to be log (n)
giving us the space complexity. To get the time complexity we then can do some math and see that the total
number of nodes in the tree at a maximum (each place we would have to visit) would be 2^(log base 2 (n)) 
which is just n (it is log base 2 as we are dividing by 2).*)

(*A.7.1
It is a linear recursive process as each iteration of the function last_two calls itself only once. Thus
it does some calculation then calls itself once making it a linear recursive process.*)

(*A.7.2
Since we know the function is a linear recursive process both the time and space complexity must be O(n).
This is because it is a linear process so it will run n + 1 times and since the recursion is linear
it will take up at most O(n) space in pending last_two calls*)

(*B.1.a 
(fun x y -> x * (2 + y)) 20 8
*)

(*B.1.b
(fun a b c -> sqrt (b *. b -. 4.0 *. a *. c)) 1.0 20.0 3.0
*)

(*B.1.c
(fun x -> (fun y -> (fun z -> x * y * z) 3) 2) 1
*)

(*B.1.d
(fun x -> (fun x -> (fun x -> x * x * x) 3) 2) 1
*)

(*B.2
-> (fun x y -> let y = 14 in let z = 22 in x * y * z) (2 * 10) (3 + 4)
-> (fun x y -> (fun y -> let z = 22 in x * y * z) 14) (2 * 10) (3 + 4)
-> (fun x y -> (fun y -> (fun z -> x * y * z) 22) 14) (2 * 10) (3 + 4)
   Evaluate fun x y -> (20) (7)
   Substitute in 20 for x, and 7 for y, but since there is lambda shielding 7 is not substituted
  -> fun y -> (fun z -> 20 * y * z) 22) 14)
  Evaluate fun y -> 14
  Subsitute 14 for y
  -> fun z -> 20 * 14 * z) 22
  Evaluate fun z -> 22
  Subsitute 22 for z
  -> 20 * 14 * 22
  Evaluate multiplication
    -> 6160

*)

(*B.3
(fun x y z -> x + y + z) (10) (x * 2) (y + 3)

Once we desugar it is pretty clear to see that the issue is that
we are using it all in one function. What this means is that when 
tring to evaluate fun x y z it first evaluates the values to be put into
the function 10, (x * 2), and (y + 3) but since it doesn't know what x is
yet it throws and error because x hasn't been defined. To fix this issue we
simply need to change the 'and's to 'in's in order to nest the operation, and
the values (x * 2) and (y + 3) can properly be evaluated.

let x = 10 
in let y = x * 2 
in let z = y + 3 
in x + y + z
*)

(*C.1*)

let isum term a next b =
  let rec iter a result =
    if a >/ b then result
    else iter (next a) (term a +/ result)
  in iter a (ni 0)

(*C.2.1*)
let rec product_rec term a next b =
  if a >/ b
     then (ni 1)
     else term a */ (product_rec term (next a) next b)

let factorial_rec n =
  let add_one num = num +/ (ni 1) in
  product_rec (fun x -> x) (ni 1) add_one n

let pi_product n =
  let add_one num = num +/ (ni 1) in
  let numerator_next curr_num = curr_num +/ (ni 2) -/ (mod_num (curr_num) (ni 2)) in
  let denominator_next curr_dem = curr_dem +/ (ni 2) -/ (mod_num (curr_dem +/ (ni 1)) (ni 2)) in
  let numerator = product_rec numerator_next (ni 1) add_one n in
  let denominator = product_rec denominator_next (ni 1) add_one n in
  (ni 4) */ numerator // denominator

let pi_approx = float_of_num(pi_product (ni 1000))

(*C.2.2*)

let product_iter term a next b =
  let rec iter a result =
    if a >/ b then result
    else iter (next a) (term a */ result)
  in iter a (ni 1)

let factorial_iter n =
  let add_one num = num +/ (ni 1) in
  product_iter (fun x -> x) (ni 1) add_one n

(*C.3.1*)

let rec accumulate_rec combiner null_value term a next b =
  if a >/ b then null_value
  else combiner (term a) (accumulate_rec combiner null_value term (next a) next b)

let sum term a next b =
  accumulate_rec (+/) (ni 0) term a next b
  
let product term a next b =
  accumulate_rec ( */ ) (ni 1) term a next b

(*C.3.2*)

let accumulate_iter combiner null_value term a next b =
  let rec iter a result =
    if a >/ b then result
  else iter (next a) (combiner (term a) result) in
  iter a null_value

(*C.4*)

let compose fun1 fun2 =
  fun a -> fun1 (fun2 a)

(*C.5*)

let rec repeated fun1 a =
  if a = 0 then fun x -> x
  else compose fun1 (repeated fun1 (a - 1))

(*C.6*)

let smooth dx f =
  fun x -> (f (x -. dx) +. f x +. f (x +. dx)) /. 3.0

let nsmoothed dx f n =
  (repeated (smooth dx) n) f

(*D.1*)

let is_prime n =
  if n < 2 then false
  else
  let sqrt_number = int_of_float (sqrt (float_of_int n)) in
  if n = 2 then true
  else if n mod 2 = 0 then false
  else
    let rec iter i = 
    if i > sqrt_number
      then true
    else if n mod i = 0
      then false
    else iter (i + 2)
    in iter 3

(*D.2*)

let smallest_prime_factor n =
  if is_prime n || n < 2 then invalid_arg "Must be not a prime and greater than 2"
  else
  let rec iter i =
    if n mod i = 0 then i
    else iter (i + 1)
  in iter 2
