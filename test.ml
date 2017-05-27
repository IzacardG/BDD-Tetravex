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

let rec print_list = function 
    [] -> ()
  | e::l -> print_string e ; print_string " " ; print_list l
;;

let d0 = new Tetravex.domino 1 1 1 1 0 in
let d1 = new Tetravex.domino 5 4 8 9 1 in
let d2 = new Tetravex.domino 1 9 9 5 2 in
let d3 = new Tetravex.domino 4 1 7 6 3 in
let d4 = new Tetravex.domino 9 6 6 6 4 in
let d5 = new Tetravex.domino 9 8 5 1 5 in
let d6 = new Tetravex.domino 8 4 6 3 6 in
let d7 = new Tetravex.domino 1 0 1 9 7 in
let d8 = new Tetravex.domino 6 4 9 0 8 in
let d9 = new Tetravex.domino 4 2 0 8 9 in
let d10 = new Tetravex.domino 1 3 3 4 10 in
let d11 = new Tetravex.domino 3 5 8 2 11 in
let d12 = new Tetravex.domino 0 1 2 3 12 in
let d13 = new Tetravex.domino 4 2 3 5 13 in
let d14 = new Tetravex.domino 2 8 5 8 14 in
let d15 = new Tetravex.domino 5 9 8 1 15 in
let l = [d0;d1;d2;d3;d4;d5;d6;d7;d8;d9;d10;d11;d12;d13;d14;d15] in
let t = new Tetravex.tetravex 4 4 l in
let solu = t#solve ()
in  print_list solu ;;
