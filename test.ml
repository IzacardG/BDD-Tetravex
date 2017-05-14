let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s"))) in
let bdd = createBdd formule in
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

let formule = And(Var("x"), Not(Var("x"))) in
assert(estValide (noBdd (createBdd formule)));

let formule = Imp(Var("a"), Var("a")) in
assert(estValide (createBdd formule));

print_string "Petit BDD\n";
let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s"))) in
printBdd(createBdd formule);

print_string "\nGros BDD\n";
let big_tree = buildTree formule in
printBdd (reduceTreeTobdd big_tree);
