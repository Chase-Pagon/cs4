(*A.1.1
An integer of value 10*)
(*A.1.2
A float with the value of 10.0*)
(*A.1.3
An integer with the value of 12*)
(*A.1.4
An error occurs because we are trying to add floats without using the floating point operator +.*)
(*A.1.5
An error occurs because we are trying to add ints using the floating point operator +. when we should be using just +*)
(*A.1.6
Same reason as A.1.4 we cannot add floats with an integer, both have to be integers if we are using the + operation*)
(*A.1.7
Same logic as A.1.6 we cannot add floats with an integer, both have to be floats in this case since we are using the +. operation*)
(*A.1.8
A float with the value 7.2*)
(*A.1.9
An integer with the value 5*)
(*A.1.10
An integer with the value of 7, note that this is because the negative distributes through the parenthese making the -1 effectively a +1*)
(*A.1.11
A val a with a type integer with a value of 3*)
(*A.1.12
A val b with a type integer with a value of 4*)
(*A.1.13
A boolean with the value false as 3 does not equal 4*)
(*A.1.14
A boolean with the value true*)
(*A.1.15
A boolean with the value false. It is different because it is a test of physical equality of the two arrays. Essentially this means that a physical modification to one of the arrays will affect the other.
They are identical in memory*)
(*A.1.16
A list of a tuple with 3 integers with the a tuple of value of 1, 2, and 3*)
(*A.1.17
The same result as A.1.16. The ocaml interprets the commas to mean a tuple, so it interpreted it as a list with a single element that is a 3-tuple.*)
(*A.1.18
An integer with a value of 4 (b) because 4 > 3 and 4 < 12*)
(*A.1.19
Ocaml doesn't have the 'and' operator*)
(*A.1.20
An integer of value 6. 4 > 3 so its then 2 + 4 = 6*)
(*A.1.21
An integer of value 4 as anything after the else is only calculated if the else case is triggered. *)
(*A.1.22
An integer value of 6 as 4 + 2 = 6. By putting parenthese around the if statement the + 2 isn't included in the else statement and thus calculated after the fact.*)
(*A.1.23
It gives an error because both branches of the conditional must have the same type. If there is no else branch then the type assumed is the type 'unit' and the expression then have a type 'int'
but it expects a type of 'unit'*)

(*A.2*)
let rec sum_of_squares_of_two_largest a b c =
  if a > b && a > c
    then if b > c then a*a + b*b
    else a*a + c*c
  else if b > c
    then if a > c then a*a + b*b
    else b*b + c*c
  else
    if b > a then c*c + b*b
    else a*a + c*c

(*A.3
The following function takes in two integers and returns an integer. What the function does it checks if b is greater than 0 and if so it just adds the two integers,
  but if b < 0 then it does a psuedo absolute value by intstead minusing the negative b thus adding a the absolute value of b.*)

(*B.1
Ben will observe an infinite loop for an interpreter that uses applicative-order evaluation. This is because it will first try to evaluate each of the variables being passed into test.
So 0 will evalutate to 0 but calling the function p () will lead to an infinite loop because it just keeps calling itself. The other case though of a normal-order evaluation it will first
go into the function test and evaluate that 0 = 0 and return 0, thus never triggering the else statement which would cause an infinite loop.*)

(*B.2
The issue is that it will cause an infinite loop. Since the new_if is a function itself that means that the expressions inside will be evaluated before the entire method is evaluated.
Thus, since it calls sqrt_iter as part of one of the expressions it will continue to call the sqrt_iter function before it can evaluate the new_if function resulting in an infinite loop.*)

(*B.3.1
Add_a is a recursive process. This can best be explained by the idea that it does not start increasing b until it has finished calling add_a. The function add_a is continued to be called
until a = 0 then once a is equals 0 it starts to evaluate all of the inc functions that have queued so effectively it is doing inc( 0 inc ( 0 b)) since it only takes two iterations for a to
equal 0. Add_b is an iterative process. This is can best be explained by the idea it never stacks up other functions to be completed after a root case is triggered. It calls itself each time if
a != 0 and passed back itself two values and no other expressions to be evaluated later. It would look something like add_b 2 5 -> add_b 1 6 -> add_b 0 7 -> 7. It nevers needs to go back after
it reachs the root case, it just returns the value, thus it emulates an iterative process. *)

(*B.3.2*)
(*

let rec add_a a b =
  if a = 0
    then b
    else inc (add_a (dec a) b)

Desugar this to:

let rec add_a
  fun a b ->
    if a = 0
      then b
      else inc (add_a (dec a) b)

Bind the name "add_a" to the value:

    fun a b ->
    if a = 0
      then b
      else inc (add_a (dec a) b)

Evaluate(add_a 2 5)
  apply (fun a b -> if a = 0) to 2, 5
  substitute 2 for a, 5 for b in (if a = 0) 
    -> if 2 = 0 then 5 else inc(add_a (dec 2) 5)
  evaluate (if 2 = 0 then 5 else inc(add_a (dec 2) 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        evaluate 2 -> 2
        evaluate 0 -> 0
        = -> primitive function =
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate inc(add_a (dec 2) 5)
        evaluate (dec 2)
          evaluate 2 -> 2
          dec -> primitive function dec
          apply dec to 2 -> 1
        apply (fun a b -> if a = 0) to 1, 5
        substitute 1 for a, 5 for b in (if a = 0) 
          -> if 1 = 0 then 5 else inc(add_a (dec 1) 5)
        evaluate (if 1 = 0 then 5 else inc(add_a (dec 1) 5))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
              evaluate 1 -> 1
              evaluate 0 -> 0
              = -> primitive function =
              apply = to 1, 0 -> false
            first argument of if is false, so evaluate the third operand:
              evaluate inc(add_a (dec 1) 5)
            evaluate (dec 1)
              evaluate 1 -> 1
              dec -> primitive function dec
              apply dec to 1 -> 0
            apply (fun a b -> if a = 0) to 0, 5
            substitute 0 for a, 5 for b in (if a = 0) 
              -> if 0 = 0 then 5 else inc(add_a (dec 0) 5)
            evaluate (if 0 = 0 then 5 else inc(add_a (dec 0) 5))
              if is a special form, so evaluate the first operand:
            evaluate (0 = 0)
              evaluate 0 -> 0
              evaluate 0 -> 0
              = -> primitive function =
              apply = to 0, 0 -> true
            first argument of if is true, so evaluate the second operand:
              evaluate inc (inc (5))
                evaluate 5 -> 5
                inc -> primitive function inc
                apply inc to 5 -> 6
                evaluate inc (6)
                  evaluate 6 -> 6
                  inc -> primitive function inc
                  apply inc to 6 -> 7
                  result: 7


*)

(*B.3.3*)
(*
let rec add_b a b =
  if a = 0
     then b
     else add_b (dec a) (inc b)

Desugar this to:

let rec add_b =
  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Bind the name "add_b" to the value:

  fun a b ->
    if a = 0
       then b
       else add_b (dec a) (inc b)

Evaluate (add_b 2 5)
  apply (fun a b -> if >>>a = 0) to 2, 5
  substitute 2 for a, 5 for b in (if >>>a = 0)
    -> if 2 = 0 then 5 else add_b (dec 2) (inc 5)
  evaluate (if 2 = 0 then 5 else add_b (dec 2) (inc 5))
    if is a special form, so evaluate the first operand:
      evaluate (2 = 0)
        evaluate 2 -> 2
        evaluate 0 -> 0
        = -> primitive function =
        apply = to 2, 0 -> false
    first argument of if is false, so evaluate the third operand:
      evaluate (add_b (dec 2) (inc 5))
        evaluate (dec 2)
          evaluate 2 -> 2
          dec -> primitive function dec
          apply dec to 2 -> 1
        evaluate (inc 5)
          evaluate 5 -> 5
          inc -> primitive function inc
          apply inc to 5 -> 6
        apply (fun a b -> if >>> a = 0) to 1, 6
        substitute 1 for a, 6 for b in (if >>> a =0)
          -> if 1 = 0 then 6 else add_b (dec 1) (inc 6)
        evaluate (if 1 = 0 then 6 else add_b (dec 1) (inc 6))
          if is a special form, so evaluate the first operand:
            evaluate (1 = 0)
              evaluate 1 -> 1
              evaluate 0 -> 0
              = -> primitive function =
              apply = to 1, 0 -> false
          first argument of if is false, so evaluate the third operand:
            evaluate (add_b (dec 1) (inc 6))
              evaluate (dec 1)
                evaluate 1 -> 1
                dec -> primitive function dec
                apply dec to 1 -> 0
              evaluate (inc 6)
                evaluate 6 -> 6
                inc -> primitive function inc
                apply inc to 6 -> 7
              apply (fun a b -> if >>>a = 0) to 0, 7
              substitute 0 for a, 7 for b in (if >>>a = 0)
                -> if 0 = 0 then 7 else add_b (dec 0) (inc 7)
              evaluate (if 0 = 0 then 7 else add_b (dec 0) (inc 7))
                if is a special form, so evaluate the first operand:
                  evaluate (0 = 0)
                    evaluate 0 -> 0
                    evaluate 0 -> 0
                    = -> primitive function =
                    apply = to 0, 0 -> true
                first argument of if is true, so evaluate the second operand:
                  result: 7
*)

(*C.1.1*)
let rec factorial n =
  if n = 0 then 1 else n * factorial (n - 1)

let e_term n = 
  1. /. float_of_int (factorial n)

(*C.1.2*)
let rec e_approximation n = 
  if n = 0 then e_term 0
  else e_term n +. e_approximation (n-1)

(*C.1.3
e_appoximation 20 = float = 2.71828182845904553
exp 1.0 = float = 2.71828182845904509
They are identical up to the last two digits*)

(*C.1.4
e_approximation 100 = float = infinity
I believe that it says infinity because it runs out of bits to express the float around e_term 64 so it defaults to 0 and 1 / 0 is infinity thus it returns infinity
for e_term 64 and any number + infinity = infinity*)

(*C.2*)
let rec is_even n = if n = 0 then true else is_odd (n-1)
and is_odd n = if n = 0 then false else is_even (n-1)

(*C.3*)

let rec f_rec n = 
  if n < 3 then n else f_rec (n-1) + 2 * f_rec(n-2) + 3 * f_rec(n-3)

let rec f_num a b c n =
  if n < 3 then a else f_num (a + 2 * b + 3 * c) a b (n-1)
let f_iter n =
  if n < 3 then n else f_num 2 1 0 n



(*C.4*)

let rec pascal_coefficient r c =
  match r, c with
  | (r, c) when c > r || r < 1 || c < 1 -> failwith "Invalid input"
  | (r, c) when c = r -> 1
  | (_,1) -> 1
  | _ -> pascal_coefficient (r-1) (c) + pascal_coefficient (r-1) (c-1)
