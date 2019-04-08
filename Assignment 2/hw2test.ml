(* test case for make_matcher function *)

let accept_all string = Some string

type sentence_nonterms = | Expr | Noun | Adj | Verb

let sentence_gram = (Expr, function
			   | Expr -> [[N Noun; N Verb; N Noun]; [N Noun; N Verb]]
| Noun -> [[N Adj; N Noun]; [T"Mark"]; [T"Sally"]; [T"pizza"]; [T"skates"]]
| Adj -> [[T"yellow"]; [T"yummy"]]
| Verb -> [[T"ate"]; [T"wore"]])

let make_matcher_test = (make_matcher sentence_gram accept_all ["Sally"; "ate"; "yellow"; "yummy"; "yellow"; "yellow"; "yummy"; "yellow"; "pizza"; "skates"] = Some ["skates"])
