type ('nonterminal, 'terminal) symbol =
  | N of 'nonterminal
    | T of 'terminal
;;

(* Check if a is a subset of b *)
let rec subset a b =
	match a with
		| [] -> true
		| hd :: tl -> List.mem hd b && subset tl b
;;

(* Check if 2 sets represented by a and b are equal *)
let equal_sets a b =
	subset a b && subset b a
;;

(* Return the intersection of 2 sets *)
let rec set_intersection a b =
	match a with
		| [] -> []
		| hd :: tl -> match List.mem hd b with
			| true -> hd :: set_intersection tl b
			| false -> set_intersection tl b
;;

(* Returns values of a that are not in b *)
let rec set_diff a b =
	match a with
		| [] -> []
		| hd :: tl -> match not (List.mem hd b) with
			| false -> set_diff tl b
			| true -> hd :: set_diff tl b
;;


(* Return the union of 2 sets *)
let set_union a b =
	a @ (set_diff b a)
;;

(* Returns the computed fixed point of f with respect to x *)
let rec computed_fixed_point eq f x =
	match (eq x (f x)) with
		| true -> x
		| false -> computed_fixed_point eq f (f x)
;;

let rec find_rules_by_symbol symbol rules = 
	match rules with
		| [] -> []
		| hd :: tl -> match ((fst hd) = symbol) with
			| false -> find_rules_by_symbol symbol tl
			| true -> [hd]@find_rules_by_symbol symbol tl
;;

let rec expand_rule r rules =
	match r with
		| [] -> []
		| hd :: tl -> match hd with
			| T _ -> expand_rule tl rules
			| N s -> set_union (find_rules_by_symbol s rules ) (expand_rule tl rules)
;;

let rec expand_set set rules = 
	match set with 
		| [] -> []
		| hd :: tl -> let tmp = (snd hd) in 
		set_union (set_union [hd] (expand_rule tmp rules)) (expand_set tl rules)
;;

(* Returns grammar with all unreachable rules removed *)
let filter_reachable g =
	let start_symbol = fst g in
	let rules = snd g in
	let start_set = find_rules_by_symbol start_symbol rules in
	let expand_set_helper s  =  expand_set s rules in
	let reachable_set = computed_fixed_point (equal_sets) expand_set_helper start_set in
	let tmp = set_intersection rules reachable_set in
	start_symbol, tmp
;;
	
