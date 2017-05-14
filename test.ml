(*
let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s"))) in
let big_tree = buildTree formule in
printBdd(reduceTreeTobdd big_tree);
*)

let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s"))) in
let reduced_tree = reduceTree (buildTree formule) in
let bdd = reduceTreeTobdd reduced_tree in
let valuation = emptyValuation() in
assert(evaluateBdd valuation bdd);
setValue valuation "p" true;
assert(not (evaluateBdd valuation bdd));
setValue valuation "q" true;
assert(evaluateBdd valuation bdd);
setValue valuation "q" false;
setValue valuation "r" true;
assert(not (evaluateBdd valuation bdd));
setValue valuation "s" true;
assert(evaluateBdd valuation bdd);
(* printBdd(bdd); *)

let formule = And(Var("x"), Not(Var("x"))) in
assert(estValide (noBdd (createBdd formule)));

let formule = Imp(Var("a"), Var("a")) in
assert(estValide (createBdd formule));

(*
let big_tree = buildTree formule;
let big_bdd = reduceTreeTobdd big_tree;
let reduced_bdd = reduceTreeTobdd reduced_tree;
*)
