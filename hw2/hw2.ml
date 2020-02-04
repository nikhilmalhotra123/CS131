type ('nonterminal, 'terminal) symbol =
	| N of 'nonterminal
	| T of 'terminal
;;

type ('nonterminal, 'terminal) parse_tree =
	| Node of 'nonterminal * ('nonterminal, 'terminal) parse_tree list
	| Leaf of 'terminal
;;

let accept_empty_suffix = function
   | [] -> Some []
   | _ -> None
;;

let rec find_rules_by_symbol rules symbol =
        match rules with
		| [] -> []
		| hd :: tl -> match ((fst hd) = symbol) with
			| false -> find_rules_by_symbol tl symbol
			| true -> [(snd hd)]@find_rules_by_symbol tl symbol
;;

(* Converts a hw1 grammar to hw2 grammar *)
let convert_grammar gram1 =
	let rules = snd gram1 in
	(fst gram1), function
		| symbol -> find_rules_by_symbol rules symbol
;;

let rec parse_tree_leaves tree =
	match tree with
		| Leaf leaf -> [leaf]
		| Node (_ , children) ->
			let rec helper tail =
				match tail with
					| [] -> []
					| hd::tl -> (parse_tree_leaves hd)@(helper tl)
			in
			helper children
;;

let rec match_symbol rules parse_or_match accept frag = function
	| T s -> (
		match frag with
			| [] -> None
			| hd::tl ->
				match s = hd with
					| true -> accept tl
					| false -> None
		)
	| N s ->
		matcher rules parse_or_match (rules s) accept frag

and match_rule rules parse_or_match accept frag = function
	| [] -> accept frag
	| hd::tl -> match_symbol rules parse_or_match (fun rule -> (match_rule rules parse_or_match accept rule tl)) frag hd

and matcher rules parse_or_match = function
	| [] -> (fun accept frag -> None)
	| hd::tl ->
		( fun accept frag ->
			match (match_rule rules parse_or_match accept frag hd) with
				| None -> matcher rules parse_or_match tl accept frag
				| Some x -> (parse_or_match x hd)
		)
;;

let make_matcher gram =
	let (start_symbol, rules) = gram in
	matcher rules (fun x hd -> Some x) (rules start_symbol)
;;

let rec deserialize_symbol preorder = function
    | T s -> ((Leaf s), preorder)
    | N s -> (
							match preorder with
                | [] -> ((Node (s, [])), [])
                | hd::tl -> let (children, list_left) = (deserialize_rule tl hd) in
                          ((Node (s, children)), list_left)
							)

and deserialize_rule preorder = function
	  | [] -> ([], preorder)
	  | hd::tl -> let (child, list_left) = deserialize_symbol preorder hd in
	            let (child_rest, new_list_left) = (deserialize_rule list_left tl) in
	            (child::child_rest, new_list_left)

and deserialize_tree start preorder =
  match deserialize_rule preorder [N start] with
    | hd::_, _-> Some (hd)
    | _ -> None
;;

let make_parser gram =
	let (start_symbol, rules) = gram in
	(fun frag -> match make_matcher gram accept_empty_suffix frag with
		| Some [] -> (match matcher rules (fun x hd -> Some (hd::x)) (rules start_symbol) accept_empty_suffix frag with
										| Some x -> deserialize_tree start_symbol x
										| _ -> None
									)
		| _ -> None

		)
;;
