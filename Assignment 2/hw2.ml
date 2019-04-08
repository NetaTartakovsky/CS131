type ('nonterm, 'term) symbol =
| N of 'nonterm
| T of 'term

type ('nonterminal, 'terminal) parse_tree =
  | Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
  | Leaf of 'terminal

(* returns list of possible rhs for a given non terminal *)
let rec func rules phrase = match rules with
| (x, y)::r -> if (x = phrase) then List.concat [[y]; func r phrase] else func r phrase
| [] -> [];;

(* takes hw 1 grammar and returns hw 2 grammar *)
let convert_grammar gram1 = (fst gram1, func (snd gram1));;

(* mutually recursive solution that returns leaves of a tree *)
let rec parse_tree_leaves tree = match tree with
| Node (x, y) -> tree_list y
| Leaf w -> [w]
and
tree_list = function
| (Leaf z)::r -> List.concat [[z]; tree_list r]
| a::r -> List.concat [parse_tree_leaves a; tree_list r]
| [] -> [];;

(* goes through list of right hand side rules for nonterminal nodes *)
let rec match_rules rules rhs_rules accept frag = match rhs_rules with
| [] -> None
| first_rule::next_rules -> 
let head_match = (match_symbols rules first_rule) 
and tail_match = (match_rules rules next_rules) in
let or_match = head_match accept frag
in match or_match with
| None -> tail_match accept frag
| _ -> or_match
and
(* goes through symbols contained within right hand side rule *)
match_symbols rules rule accept frag = match rule with
| [] -> accept frag
| (T x)::r -> if (List.length frag = 0) then None else (if (List.hd(frag) = x) then (match_symbols rules r accept (List.tl(frag))) else None)
| (N y)::r -> match_rules rules (rules y) (match_symbols rules r accept) frag

(* utilizes helper functions to return a matcher for gram *)
let make_matcher gram = match gram with
| (start, rules) -> (fun accept frag -> match_rules rules (rules start) accept frag)
