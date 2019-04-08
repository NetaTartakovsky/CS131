(* test cases for hw1 *)

let my_subset_test0 = subset [1;2;2] [1;2;3]
let my_subset_test1 = subset [] [1;4;3]
let my_subset_test2 = not (subset [1;5;6] [1;9;3])

let my_equal_sets_test0 = equal_sets [4;7;7;8;9] [4;4;7;8;9]
let my_equal_sets_test1 = not (equal_sets [0;2;3] [4;1;3])

let my_set_union_test0 = equal_sets (set_union [] [4;4;5;2]) [2;4;5]
let my_set_union_test1 = equal_sets (set_union [0;0;6;7] [1;5;6]) [0;1;5;6;7]
let my_set_union_test2 = equal_sets (set_union [] []) []

let my_set_intersection_test0 =
  equal_sets (set_intersection [4;5;6] []) []
let my_set_intersection_test1 =
  equal_sets (set_intersection [3;4;6] [1;4;3]) [4;3]
let my_set_intersection_test2 =
  equal_sets (set_intersection [4;7;6;8] [8;6;7;4]) [4;6;7;8]

let my_set_diff_test0 = equal_sets (set_diff [2;4] [1;4;3;2]) []
let my_set_diff_test1 = equal_sets (set_diff [5;6;5;7;6] [5;6]) [7]
let my_set_diff_test2 = equal_sets (set_diff [1;2;3] []) [1;2;3]
let my_set_diff_test3 = equal_sets (set_diff [] [8;9;7]) []

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 2) 100 = 0
let my_computed_fixed_point_test1 =
  computed_fixed_point (=) (fun x -> x *. 10.) 2. = infinity

type convo_nonterminals = | Expr | Noun | Verb | Adj

let convo_rules =
[Expr, [T"("; N Expr; T")"];
Expr, [N Noun; N Verb];
Expr, [N Noun; N Adj];
Expr, [T ":)"];
Noun, [N Adj];
Noun, [T "CAT"];
Verb, [N Adj];
Verb, [T "RUN"; N Adj];
Adj, [T "BLUE"]]

let convo_grammar = Expr, convo_rules

let my_filter_reachable_test0 = filter_reachable convo_grammar = convo_grammar

let my_filter_reachable_test1 = filter_reachable (Expr, List.tl (List.tl convo_rules)) = (Expr, [Expr, [N Noun; N Adj]; Expr, [T ":)"]; Noun, [N Adj]; Noun, [T "CAT"]; Adj, [T "BLUE"]])

let my_filter_reachable_test2 = filter_reachable (Noun, convo_rules) = (Noun, [Noun, [N Adj]; Noun, [T "CAT"]; Adj, [T "BLUE"]])
