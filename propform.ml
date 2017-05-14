(* Foncteurs *)

module S = Set.Make( 
    struct
        type t = string
        let compare = Pervasives.compare
    end)

(* Types *)

type 'a formula =
    |Var of 'a
    |True
    |False
    |Not of 'a formula
    |And of 'a formula * 'a formula
    |Or of 'a formula * 'a formula
    |Imp of 'a formula * 'a formula
    |Equi of 'a formula * 'a formula

(* Gauche : true, droite : false *)
type bdd =
    |Leaf of bool
    |Node of int * string * bdd * bdd

type bdt =
    |Leaf of bool
    |Node of string * bdt * bdt

(* Valuations *)

let getValue valuation v =
    if (not (Hashtbl.mem valuation v)) then
        false
    else
        Hashtbl.find valuation v
;;

let setValue valuation v x =
    Hashtbl.add valuation v x
;;

let emptyValuation () =
    Hashtbl.create 10
;;

let rec eval valuation = function
    |True          -> true
    |False         -> false
    |Var v         -> getValue valuation v
    |Not e         -> not (eval valuation e)
    |And (e1, e2)  -> (eval valuation e1) && (eval valuation e2)
    |Or (e1, e2)   -> (eval valuation e1) || (eval valuation e2)
    |Imp  (e1, e2) -> (eval valuation e2) || not (eval valuation e1)
    |Equi (e1, e2) -> (eval valuation (Imp(e1,e2))) && (eval valuation (Imp(e2, e1)))
;;

let rec formuleToString = function
    | True        -> "true"
    | False       -> "false"
    | Var e       -> e
    | Not e       -> "(! " ^ (formuleToString e) ^ ")"
    | And (e1, e2)   -> "(" ^ (formuleToString e1) ^ " && " ^ (formuleToString e2) ^ ")"
    | Or (e1,  e2)    -> "(" ^ (formuleToString e1) ^ " || " ^ (formuleToString e2) ^ ")"
    | Imp (e1, e2)  -> "(" ^ (formuleToString e1) ^ " -> " ^ (formuleToString e2) ^ ")"
    | Equi (e1, e2)  ->  "(" ^ (formuleToString e1) ^ " <-> " ^ (formuleToString e2) ^ ")"
;;

let rec setVar = function
    | True ->  S.empty
    | False -> S.empty
    | Var s -> S.singleton s
    | Not e1 -> setVar e1
    | And (e1, e2) -> S.union (setVar e1) (setVar e2)
    | Or (e1, e2) -> S.union (setVar e1) (setVar e2)
    | Imp (e1, e2) -> S.union (setVar e1) (setVar e2)
    | Equi (e1, e2) -> S.union (setVar e1) (setVar e2)
;;

let buildTree formule =
    let rec aux valuation = function
        | [] -> Leaf(eval valuation formule)
        | t::q ->
            begin
                setValue valuation t true;
                let a = aux valuation q in
                setValue valuation t false;
                let b = aux valuation q in
                Node(t, a, b)
            end
    in
    aux (emptyValuation ()) (S.elements (setVar formule))
;;

let isLeaf = function
    | Leaf(_) -> true
    | _ -> false
;;

let rec reduceTree tree =
    match tree with
    | Leaf(_) -> tree
    | Node(_, Leaf(b1), Leaf(b2)) -> if b1 == b2 then Leaf(b1) else tree
    | Node(v, t1, t2) ->
        begin
            let a = reduceTree t1 in
            let b = reduceTree t2 in
            if (isLeaf a && isLeaf b) then
                reduceTree (Node(v, a, b))
            else
                Node(v, a, b)
        end
;;

let treeToString tree = 
    "a"
;;

let reduceTreeToBDD tree =
    let t = Hashtbl.create 10 in
    let i = ref 0 in
    let vrai : bdd = Leaf(true) in
    let faux : bdd = Leaf(false) in
    let rec norm x : bdd =
        match x with
        | Leaf(b) -> if b then vrai else faux
        | Node(var, a, b) ->
            begin
                ignore(print_int (Hashtbl.length t));
                ignore(print_int (List.length (Hashtbl.find_all t "a")));
                let n : bdd = Node(!i, var, norm a, norm b) in
                let s = "a" in
                if Hashtbl.mem t s then
                    Hashtbl.find t s
                else
                    begin
                        Hashtbl.add t s n;
                        i := !i + 1;
                        n
                    end
            end
    in
    norm tree
;;

(* Tests *)

let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s"))) in
let big_tree = buildTree formule in
reduceTreeToBDD big_tree;

(*
let big_tree = buildTree formule;
let big_bdd = reduceTreeToBDD big_tree;
let reduced_bdd = reduceTreeToBDD reduced_tree;
*)
