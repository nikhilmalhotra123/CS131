let my_subset_test0 = subset [2;4] [4;1;2;3]

let my_equal_sets_test0 = equal_sets [2;3;4] [4;4;3;2]

let my_set_union_test0 = equal_sets (set_union [1;4] [1;2;3]) [1;2;3;4]

let my_set_intersection_test0 =
  equal_sets (set_intersection [1;3;4] [1;2;3]) [1;]

let my_set_diff_test0 = equal_sets (set_diff [5;4;2] [5;4;3;1]) [2]

let my_computed_fixed_point_test0 =
  computed_fixed_point (=) (fun x -> x / 1) 20 = 20

type awksub_nonterminals =
  | Expr | Lvalue | Incrop | Binop | Num | Thing | What

let awksub_rules =
     [Expr, [T"("; N Expr; T")"];
     Expr, [N Num];
     Expr, [N Expr; N Binop; N Thing];
     Expr, [N Incrop; N Lvalue];
     Expr, [N Thing; N What];
     Lvalue, [T"$"; N Expr];
     Incrop, [T"++"];
     Incrop, [T"--"];
     Binop, [T"+"];
     Binop, [T"-"];
     Thing, [T"thing"];
     Thing, [T"THING"];
     What, [T"what"];
     What, [T"WHAT"];
     Num, [T"0"];
     Num, [T"1"];
     Num, [T"2"];
     Num, [T"3"];
     Num, [T"4"];
     Num, [T"5"];
     Num, [T"6"];
     Num, [T"7"];
     Num, [T"8"];
     Num, [T"9"]]
 
let awksub_grammar = Expr, awksub_rules

let my_awksub_test0 = filter_reachable awksub_grammar = awksub_grammar
