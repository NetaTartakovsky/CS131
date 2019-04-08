(* function returns true if a is a subset of b *)
let subset a b = List.for_all (fun x -> List.exists (fun y -> y=x) b) a;;

(* function returns true if sets a & b are equal *)
let equal_sets a b = subset a b && subset b a;;

(* function returns a list representing the union of sets a & b *)
let set_union a b = List.sort_uniq compare (List.concat [a; b]);;

(* function returns a list representing the intersection of sets a & b *)
let rec set_intersection a b = match a with
| [] -> []
| h::r -> if (subset [h] b) then (set_union [h] (set_intersection r b)) else (set_intersection r b);; 

(* function returns a list representing the difference between sets a & b *)
let rec set_diff a b = match a with
| [] -> []
| h::r -> if (subset [h] b) then (set_diff r b) else (set_union [h] (set_diff r b));;

(* function returns the computed fixed point for f with respect to x *)
let rec computed_fixed_point eq f x = let k = (f x) in if (eq k x) then x else (computed_fixed_point eq f k);;

(* function filter_reachable g that returns a copy of the grammar g with all unreachable rules removed *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal ;;

(* function searches through right hand side rules *)
let rec searchRHS rules = match rules with
| (N n)::r -> List.concat[[n]; searchRHS r]
| (T n)::r -> searchRHS r
| [] -> [] ;;

(* rules search function *)
let rec searchRules rules nodes = match rules with
  | (x, y)::r -> if (List.exists (fun z -> z=x) nodes) then List.concat [(List.concat[searchRHS y; nodes]); searchRules r nodes] else List.concat[searchRules r nodes; nodes]
  | [] -> [];;

let getNodes rules symbol = set_union (computed_fixed_point (equal_sets) (fun w -> searchRules rules w) [symbol]) [];;

let rec removeRules rules nodes = match rules with  
  | (x, y)::r -> if (List.exists (fun z -> z=x) nodes) then List.concat[[(x,y)]; removeRules r nodes] else removeRules r nodes
  | [] -> [];;

let filter_reachable g = match g with
  (symbol, rules) -> (symbol, removeRules rules (getNodes rules symbol));;
