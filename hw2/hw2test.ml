let accept_empty_suffix = function
   | _::_ -> None
   | x -> Some x

type my_gram_nonterminals =
  | Sen | NP | Phrase | Conj | VP | Noun | Verb

let my_gram =
  (Sen,
   function
     | Sen ->
         [[N NP; N VP;];
          [N Phrase; N Conj; N Phrase;]]
     | Phrase ->
     	   [[N NP;];
         [N VP];]
     | NP ->
	     [[N Noun];
	      [N Noun; N Conj; N Phrase];]
     | Conj ->
        [[T"and"]; [T"or"];]
     | VP ->
        [[N Verb; N NP;];
  	     [N Verb; N Conj; N Verb; N NP;];]
     | Noun ->
        [[T"Jack"]; [T"Jill"]; [T"John"];]
     | Verb ->
        [[T"likes"]; [T"loves"];]
)

let frag = ["Jack"; "and"; "loves"; "or"; "likes"; "Jill"; "likes"; "Jill"; "or"; "John";]

let make_matcher_test =
  ((make_matcher my_gram accept_empty_suffix frag) = Some [])

let make_parser_test =
  match make_parser my_gram frag with
    | Some tree -> parse_tree_leaves tree = frag
    | _ -> false
