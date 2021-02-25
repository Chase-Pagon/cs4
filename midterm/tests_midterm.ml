open OUnit2
open Midterm

(* 
 * Utilities. 
 *)

let assert_false msg x = assert_bool msg (not x)
let assert_true msg x = assert_bool msg x
let assert_not_equal msg x y = assert_bool msg (not (x = y))

let assert_raises_failure msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Failure _ -> true
       | _ -> false)

let assert_raises_invalid_arg msg x =
  assert_bool msg
    (try
       begin
          ignore (x ());
          false
       end
     with
       | Invalid_argument _ -> true
       | _ -> false)

(* Print a list of integers. *)
let print_int_list lst =
  let rec aux lst =
    match lst with
      | [] -> ""
      | h :: t -> ";" ^ string_of_int h ^ aux t
    in
  let s = 
    match lst with
      | [] -> "[]"
      | h :: t -> "[" ^ string_of_int h ^ aux t ^ "]"
  in
    Printf.printf "%s\n" s

(*
 * Sample trees for part B.
 *)

let tree0 = Leaf

let tree1 = Node2 (Leaf, 4, Leaf)

let tree2 = Node3 (Leaf, 4, Leaf, 6, Leaf)

let tree3 = 
  Node2 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 6, Leaf))

let tree4 = 
  Node2 (Node2 (Leaf, 3, Leaf), 4, Node3 (Leaf, 5, Leaf, 6, Leaf))

let tree5 = 
  Node3 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6, Node2 (Leaf, 7, Leaf))

let tree6 =
  Node3 (Node2 (Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6,
    Node3 (Leaf, 7, Leaf, 9, Leaf))

let tree7 =
  Node3 (Node3 (Leaf, 1, Leaf, 3, Leaf), 4, Node2 (Leaf, 5, Leaf), 6,
    Node3 (Leaf, 7, Leaf, 9, Leaf))

let tree7a =
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf), 60,
    Node3 (Leaf, 70, Leaf, 90, Leaf))

let tree7b = 
  Node3 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40,
    Node3 (Leaf, 50, Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf))

let tree7c = 
  Node2
    (Node2 (Node3 (Leaf, 10, Leaf, 30, Leaf), 40, Node2 (Leaf, 50, Leaf)), 51,
       Node2 (Node2 (Leaf, 52, Leaf), 60, Node3 (Leaf, 70, Leaf, 90, Leaf)))

let tree8 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node2 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 9, Leaf)))

let tree9 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node2 (Leaf, 7, Leaf), 9,
      Node2 (Leaf, 10, Leaf)))

let tree10 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf), 9,
      Node2 (Leaf, 10, Leaf)))

let tree11 =
  Node2 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node3 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf), 9,
      Node3 (Leaf, 10, Leaf, 11, Leaf)))

let tree12 =
  Node3 (Node2 (Node2 (Leaf, 1, Leaf), 2, Node2 (Leaf, 3, Leaf)), 4,
    Node2 (Node2 (Leaf, 5, Leaf), 6, Node3 (Leaf, 7, Leaf, 8, Leaf)), 9,
      Node2 (Node2 (Leaf, 10, Leaf), 11, Node2 (Leaf, 12, Leaf)))

let test_tree_insert i t =
  match insert_helper i t with
    | Ok t' -> t'
    | Split (t1, j, t2) -> Node2 (t1, j, t2)

(*
 * The tests.
 *)

let all_tests = "all" >:::
[ 

  (* ---------------------------------------------------------------------- 
   * Part A.
   * ---------------------------------------------------------------------- *)

  "Problem A.1" >:: (fun _ ->
    assert_equal ~msg:"replicate 0 0"  (replicate 0 0) [];
    assert_equal ~msg:"replicate 5 0"  (replicate 5 0) [0;0;0;0;0];
    assert_equal ~msg:"replicate 0 5"  (replicate 0 5) [];
    assert_equal ~msg:"repln 0 []"  (repln 0 []) [];
    assert_equal ~msg:"repln 1 []"  (repln 1 []) [];
    assert_equal ~msg:"repln 1 [1;2;3]"  (repln 1 [1;2;3]) [1;2;3];
    assert_equal ~msg:"repln 1 [1;2;1]"  (repln 1 [1;2;1]) [1;2;1];
    assert_equal ~msg:"repln 3 [1;2;3]"  (repln 3 [1;2;3]) [1;1;1;2;2;2;3;3;3];
    assert_equal ~msg:"repln 3 [1;2;1]"  (repln 3 [1;2;1]) [1;1;1;2;2;2;1;1;1];
    assert_equal ~msg:"repln2 0 []"  (repln2 0 []) [];
    assert_equal ~msg:"repln2 1 []"  (repln2 1 []) [];
    assert_equal ~msg:"repln2 1 [1;2;3]"  (repln2 1 [1;2;3]) [1;2;3];
    assert_equal ~msg:"repln2 3 [1;2;3]"  (repln2 3 [1;2;3]) [1;2;3;1;2;3;1;2;3];
    assert_equal ~msg:"repln2 3 [1;2;1]"  (repln2 3 [1;2;1]) [1;2;1;1;2;1;1;2;1];
    assert_equal ~msg:"repln3 0 []"  (repln3 0 []) [];
    assert_equal ~msg:"repln3 1 []"  (repln3 1 []) [];
    assert_equal ~msg:"repln3 1 [1;2;3]"  (repln3 1 [1;2;3]) [1;2;3];
    assert_equal ~msg:"repln3 3 [1;2;3]"  (repln3 3 [1;2;3]) [1;2;3;1;2;3;1;2;3];
    assert_equal ~msg:"repln3 3 [1;2;1]"  (repln3 3 [1;2;1]) [1;2;1;1;2;1;1;2;1];
  );

  "Problem A.2" >:: (fun _ ->
    assert_equal ~msg:"group []" (group []) [];
    assert_equal ~msg:"group [1;2;3;4]" (group [1;2;3;4]) [[1];[2];[3];[4]];
    assert_equal ~msg:"group [1;1;2;2;2;3;3;4;5;5;5;5;5]"
      (group [1;1;2;2;2;3;3;4;5;5;5;5;5]) [[1;1];[2;2;2];[3;3];[4];[5;5;5;5;5]];
    assert_equal ~msg:"group [1;1;2;2;2;1;1;4;1;1;1;1;1]"
      (group [1;1;2;2;2;1;1;4;1;1;1;1;1]) [[1;1];[2;2;2];[1;1];[4];[1;1;1;1;1]];
  );

  "Problem A.3" >:: (fun _ ->
    assert_equal ~msg:"rle []" (rle []) [];
    assert_equal ~msg:"rle [1;2;3;4]" (rle [1;2;3;4]) [(1,1);(2,1);(3,1);(4,1)];
    assert_equal ~msg:"rle [1;1;2;2;2;3;3;4;5;5;5;5;5]"
      (rle [1;1;2;2;2;3;3;4;5;5;5;5;5]) [(1,2);(2,3);(3,2);(4,1);(5,5)];
    assert_equal ~msg:"rle [1;1;2;2;2;1;1;4;1;1;1;1;1]"
      (rle [1;1;2;2;2;1;1;4;1;1;1;1;1]) [(1,2);(2,3);(1,2);(4,1);(1,5)];

    assert_equal ~msg:"rle2 []" (rle2 []) [];
    assert_equal ~msg:"rle2 [1;2;3;4]" (rle2 [1;2;3;4]) [(1,1);(2,1);(3,1);(4,1)];
    assert_equal ~msg:"rle2 [1;1;2;2;2;3;3;4;5;5;5;5;5]"
      (rle2 [1;1;2;2;2;3;3;4;5;5;5;5;5]) [(1,2);(2,3);(3,2);(4,1);(5,5)];
    assert_equal ~msg:"rle2 [1;1;2;2;2;1;1;4;1;1;1;1;1]"
      (rle2 [1;1;2;2;2;1;1;4;1;1;1;1;1]) [(1,2);(2,3);(1,2);(4,1);(1,5)];

    assert_equal ~msg:"rld []" (rld []) [];
    assert_equal ~msg:"rld [(1,1);(2,1);(3,1);(4,1)]"
      (rld [(1,1);(2,1);(3,1);(4,1)]) [1;2;3;4];
    assert_equal ~msg:"rld [(1,2);(2,3);(3,2);(4,1);(5,5)]"
      (rld [(1,2);(2,3);(3,2);(4,1);(5,5)]) [1;1;2;2;2;3;3;4;5;5;5;5;5];
    assert_equal ~msg:"rld [(1,2);(2,3);(1,2);(4,1);(1,5)]"
      (rld [(1,2);(2,3);(1,2);(4,1);(1,5)]) [1;1;2;2;2;1;1;4;1;1;1;1;1];
  );

  "Problem A.4" >:: (fun _ ->
    assert_equal ~msg:"group2 []" (group2 []) [];
    assert_equal ~msg:"group2 [1;2;3;4]" (group2 [1;2;3;4]) [[1];[2];[3];[4]];
    assert_equal ~msg:"group2 [1;1;2;2;2;3;3;4;5;5;5;5;5]"
      (group2 [1;1;2;2;2;3;3;4;5;5;5;5;5]) [[1;1];[2;2;2];[3;3];[4];[5;5;5;5;5]];

    assert_equal ~msg:"rle3 []" (rle3 []) [];
    assert_equal ~msg:"rle3 [1;2;3;4]" (rle3 [1;2;3;4]) [(1,1);(2,1);(3,1);(4,1)];
    assert_equal ~msg:"rle3 [1;1;2;2;2;3;3;4;5;5;5;5;5]"
      (rle3 [1;1;2;2;2;3;3;4;5;5;5;5;5]) [(1,2);(2,3);(3,2);(4,1);(5,5)];
  );

  "Problem A.5" >:: (fun _ ->
    assert_raises_invalid_arg "remove_nth (-1) []" (fun () -> remove_nth (-1) []);
    assert_raises_failure "remove_nth 1 []" (fun () -> remove_nth 1 []);
    assert_equal ~msg:"remove_nth 0 [42]" (remove_nth 0 [42]) (42, []);
    assert_equal ~msg:"remove_nth 0 [42;21]" (remove_nth 0 [42;21]) (42, [21]);
    assert_equal ~msg:"remove_nth 1 [3;2]" (remove_nth 1 [3;2]) (2, [3]);
    assert_equal ~msg:"remove_nth 1 [3;2;1]" (remove_nth 1 [3;2;1]) (2, [3;1]);
    assert_equal ~msg:"remove_nth 2 [4;3;2]" (remove_nth 2 [4;3;2]) (2, [4;3]);
    assert_equal ~msg:"remove_nth 2 [4;3;2;1]" (remove_nth 2 [4;3;2;1]) (2, [4;3;1]);

    (* NOTE: These tests are wholly inadequate to test proper shuffling. *)
    let test_shuffle msg lst_in lst_sorted =
      assert_equal ~msg:msg (List.sort compare lst_in) lst_sorted
    in
      begin
        test_shuffle "shuffle []" (shuffle []) [];
        test_shuffle "shuffle [1]" (shuffle [1]) [1];
        test_shuffle "shuffle [3;2;1]" (shuffle [3;2;1]) [1;2;3];
        test_shuffle "shuffle [5;4;3;2;1;2;3;4;5]" 
          (shuffle [5;4;3;2;1;2;3;4;5]) [1;2;2;3;3;4;4;5;5];
      end
  );

  "Problem A.6" >:: (fun _ ->
    assert_raises_invalid_arg "gray_codes (-1)" (fun () -> gray_codes (-1));
    assert_raises_invalid_arg "gray_codes 0" (fun () -> gray_codes 0);
    assert_equal ~msg:"gray_codes 1" (gray_codes 1) [[0]; [1]];
    assert_equal ~msg:"gray_codes 2" (gray_codes 2) [[0;0]; [0;1]; [1;1]; [1;0]];
    assert_equal ~msg:"gray_codes 3" (gray_codes 3) 
      [[0;0;0]; [0;0;1]; [0;1;1]; [0;1;0];
       [1;1;0]; [1;1;1]; [1;0;1]; [1;0;0]];
    assert_equal ~msg:"gray_codes 4" (gray_codes 4) 
      [[0; 0; 0; 0]; [0; 0; 0; 1]; [0; 0; 1; 1]; [0; 0; 1; 0]; [0; 1; 1; 0];
       [0; 1; 1; 1]; [0; 1; 0; 1]; [0; 1; 0; 0]; [1; 1; 0; 0]; [1; 1; 0; 1];
       [1; 1; 1; 1]; [1; 1; 1; 0]; [1; 0; 1; 0]; [1; 0; 1; 1]; [1; 0; 0; 1];
       [1; 0; 0; 0]];
  );

  (* ---------------------------------------------------------------------- 
   * Part B.
   * ---------------------------------------------------------------------- *)

  "Problem B.1: tree_search" >:: (fun c ->
    assert_bool "test0" (not (tree_search 4 tree0));

    assert_bool "test1" (tree_search 4 tree1);

    assert_bool "test2" (tree_search 4 tree2);
    assert_bool "test2" (tree_search 6 tree2);

    assert_bool "test3" (tree_search 4 tree3);
    assert_bool "test3" (tree_search 6 tree3);
    assert_bool "test3" (tree_search 3 tree3);

    assert_bool "test4" (tree_search 4 tree4);
    assert_bool "test4" (tree_search 6 tree4);
    assert_bool "test4" (tree_search 3 tree4);
    assert_bool "test4" (tree_search 5 tree4);

    assert_bool "test5" (tree_search 4 tree5);
    assert_bool "test5" (tree_search 6 tree5);
    assert_bool "test5" (tree_search 3 tree5);
    assert_bool "test5" (tree_search 5 tree5);
    assert_bool "test5" (tree_search 7 tree5);

    assert_bool "test6" (tree_search 4 tree6);
    assert_bool "test6" (tree_search 6 tree6);
    assert_bool "test6" (tree_search 3 tree6);
    assert_bool "test6" (tree_search 5 tree6);
    assert_bool "test6" (tree_search 7 tree6);
    assert_bool "test6" (tree_search 9 tree6);

    assert_bool "test7" (tree_search 4 tree7);
    assert_bool "test7" (tree_search 6 tree7);
    assert_bool "test7" (tree_search 3 tree7);
    assert_bool "test7" (tree_search 5 tree7);
    assert_bool "test7" (tree_search 7 tree7);
    assert_bool "test7" (tree_search 9 tree7);
    assert_bool "test7" (tree_search 1 tree7);

    assert_bool "test8" (tree_search 4 tree8);
    assert_bool "test8" (tree_search 6 tree8);
    assert_bool "test8" (tree_search 3 tree8);
    assert_bool "test8" (tree_search 5 tree8);
    assert_bool "test8" (tree_search 7 tree8);
    assert_bool "test8" (tree_search 9 tree8);
    assert_bool "test8" (tree_search 1 tree8);
    assert_bool "test8" (tree_search 2 tree8);

    assert_bool "test9" (tree_search 4 tree9);
    assert_bool "test9" (tree_search 6 tree9);
    assert_bool "test9" (tree_search 3 tree9);
    assert_bool "test9" (tree_search 5 tree9);
    assert_bool "test9" (tree_search 7 tree9);
    assert_bool "test9" (tree_search 9 tree9);
    assert_bool "test9" (tree_search 1 tree9);
    assert_bool "test9" (tree_search 2 tree9);
    assert_bool "test9" (tree_search 10 tree9);

    assert_bool "test10" (tree_search 4 tree10);
    assert_bool "test10" (tree_search 6 tree10);
    assert_bool "test10" (tree_search 3 tree10);
    assert_bool "test10" (tree_search 5 tree10);
    assert_bool "test10" (tree_search 7 tree10);
    assert_bool "test10" (tree_search 9 tree10);
    assert_bool "test10" (tree_search 1 tree10);
    assert_bool "test10" (tree_search 2 tree10);
    assert_bool "test10" (tree_search 10 tree10);
    assert_bool "test10" (tree_search 8 tree10);
  );

  "Problem B.2: insert_helper" >:: (fun c ->
    assert_equal (test_tree_insert 4 tree0) tree1;
    assert_equal (test_tree_insert 6 tree1) tree2;
    assert_equal (test_tree_insert 3 tree2) tree3;
    assert_equal (test_tree_insert 5 tree3) tree4;
    assert_equal (test_tree_insert 7 tree4) tree5;
    assert_equal (test_tree_insert 9 tree5) tree6;
    assert_equal (test_tree_insert 1 tree6) tree7;
    assert_equal (test_tree_insert 2 tree7) tree8;
    assert_equal (test_tree_insert 10 tree8) tree9;
    assert_equal (test_tree_insert 8 tree9) tree10;
    assert_equal (test_tree_insert 11 tree10) tree11;
    assert_equal (test_tree_insert 12 tree11) tree12;
    assert_equal (test_tree_insert 52 tree7a) tree7b;
    assert_equal (test_tree_insert 51 tree7b) tree7c;
  );

]

let _ = run_test_tt_main all_tests

