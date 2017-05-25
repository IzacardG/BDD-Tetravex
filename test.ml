let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s"))) in
let bdd = BDD.create formule in
let valuation = Valuation.empty() in
assert(BDD.evaluate valuation bdd);
Valuation.setValue valuation "p" true;
assert(not (BDD.evaluate valuation bdd));
Valuation.setValue valuation "q" true;
assert(BDD.evaluate valuation bdd);
Valuation.setValue valuation "q" false;
Valuation.setValue valuation "r" true;
assert(not (BDD.evaluate valuation bdd));
Valuation.setValue valuation "s" true;
assert(BDD.evaluate valuation bdd);

let formule = And(Var("x"), Not(Var("x"))) in
assert(BDD.isValid (BDD.notBDD (BDD.create formule)));

let formule = Imp(Var("a"), Var("a")) in
assert(BDD.isValid (BDD.create formule));

print_string "Petit BDD\n";
let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s"))) in
BDD.print (BDD.create formule);

print_string "\nGros BDD\n";
let big_tree = BDT.build formule in
BDT.toString big_tree;
