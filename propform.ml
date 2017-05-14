(* Foncteurs *)

module S = Set.Make( 
  struct
    let compare = Pervasives.compare
    type t = string
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

(* Gauche : faux, droit vrai *)
type bdd =
    |Leaf of bool
    |Node of string * bdd * bdd

type bdt =
    |Leaf of bool
    |Node of string * bdt * bdt

(* Valuations *)

let getValue valuation v =
    if (not (Hashtbl.mem valuation v)) then
        failwith "Variable not assigned"
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

let rec to_string = function
    | True        -> "true"
    | False       -> "false"
    | Var e       -> e
    | Not e       -> "(! " ^ (to_string e) ^ ")"
    | And (e1, e2)   -> "(" ^ (to_string e1) ^ " && " ^ (to_string e2) ^ ")"
    | Or (e1,  e2)    -> "(" ^ (to_string e1) ^ " || " ^ (to_string e2) ^ ")"
    | Imp (e1, e2)  -> "(" ^ (to_string e1) ^ " -> " ^ (to_string e2) ^ ")"
    | Equi (e1, e2)  ->  "(" ^ (to_string e1) ^ " <-> " ^ (to_string e2) ^ ")"
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
                setValue valuation t false;
                let a = aux valuation q in
                setValue valuation t true;
                let b = aux valuation q in
                Node(t, a, b)
            end
    in
    aux (emptyValuation ()) (S.elements (setVar formule))
;;

let formule = Or(Imp(Var("p"), Var("q")), And(Var("r"), Var("s")));
