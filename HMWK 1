(* Function #1 *)
let subset a b = List.for_all (fun x -> List.exists (fun y -> y=x) b) a;;

(* Function #2 *)
let equal_sets a b = subset a b && subset b a;;

(* Function #3 *)
let set_union a b = List.sort_uniq compare (List.concat [a; b]);;

(* Function #4 *)
let rec set_intersection a b = match a with
| [] -> []
| h::r -> if (subset [h] b) then (set_union [h] (set_intersection r b)) else (set_intersection r b);; 

(* Function #5 *)
let rec set_diff a b = match a with
| [] -> []
| h::r -> if (subset [h] b) then (set_diff r b) else (set_union [h] (set_diff r b));;

(* Function #6 *)
let rec computed_fixed_point eq f x = let k = (f x) in if (eq k x) then x else (computed_fixed_point eq f k);;

(* Function #7 *)
type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
  | T of 'terminal ;;

(* RHS function *)
let rec searchRHS rules = match rules with
| (N n)::r -> List.concat[[n]; searchRHS r]
| (T n)::r -> searchRHS r
| [] -> [] ;;

(* Rules search function *)
let rec searchRules rules nodes = match rules with
  | (x, y)::r -> if (List.exists (fun z -> z=x) nodes) then List.concat [(List.concat[searchRHS y; nodes]); searchRules r nodes] else List.concat[searchRules r nodes; nodes]
  | [] -> [];;

let getNodes rules symbol = set_union (computed_fixed_point (equal_sets) (fun w -> searchRules rules w) [symbol]) [];;

let rec removeRules rules nodes = match rules with  
  | (x, y)::r -> if (List.exists (fun z -> z=x) nodes) then List.concat[[(x,y)]; removeRules r nodes] else removeRules r nodes
  | [] -> [];;

let filter_reachable g = match g with
  (symbol, rules) -> (symbol, removeRules rules (getNodes rules symbol));;
