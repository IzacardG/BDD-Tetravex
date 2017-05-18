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
;;

(* Gauche : true, droite : false *)

type bdt =
    |Leaf of bool
    |Node of string * bdt * bdt
;;

type bdd =
    |DLeaf of bool
    |DNode of int * string * bdd * bdd
;;

(* Modules utiles *)

module S = Set.Make( 
    struct
        type t = string
        let compare = Pervasives.compare
    end)

module IntSet = Set.Make( 
    struct
        type t = int
        let compare = Pervasives.compare
    end)

(* Modules *)

module Valuation =
    struct
        let getValue valuation v =
            if (not (Hashtbl.mem valuation v)) then
                false
            else
                Hashtbl.find valuation v
        ;;

        let setValue valuation v x =
            Hashtbl.add valuation v x
        ;;

        let empty () =
            Hashtbl.create 10
        ;;
    end
;;

module Formule =
    struct

        let rec toString = function
            | True        -> "true"
            | False       -> "false"
            | Var e       -> e
            | Not e       -> "(! " ^ (toString e) ^ ")"
            | And (e1, e2)   -> "(" ^ (toString e1) ^ " && " ^ (toString e2) ^ ")"
            | Or (e1,  e2)    -> "(" ^ (toString e1) ^ " || " ^ (toString e2) ^ ")"
            | Imp (e1, e2)  -> "(" ^ (toString e1) ^ " -> " ^ (toString e2) ^ ")"
            | Equi (e1, e2)  ->  "(" ^ (toString e1) ^ " <-> " ^ (toString e2) ^ ")"
        ;;

        let setVar formule =
            let rec aux = function
            | True ->  S.empty
            | False -> S.empty
            | Var s -> S.singleton s
            | Not e1 -> aux e1
            | And (e1, e2) -> S.union (aux e1) (aux e2)
            | Or (e1, e2) -> S.union (aux e1) (aux e2)
            | Imp (e1, e2) -> S.union (aux e1) (aux e2)
            | Equi (e1, e2) -> S.union (aux e1) (aux e2)
            in S.elements (aux formule)
        ;;

        let rec eval valuation = function
            |True          -> true
            |False         -> false
            |Var v         -> Valuation.getValue valuation v
            |Not e         -> not (eval valuation e)
            |And (e1, e2)  -> (eval valuation e1) && (eval valuation e2)
            |Or (e1, e2)   -> (eval valuation e1) || (eval valuation e2)
            |Imp  (e1, e2) -> (eval valuation e2) || not (eval valuation e1)
            |Equi (e1, e2) -> (eval valuation (Imp(e1,e2))) && (eval valuation (Imp(e2, e1)))
        ;;

    end
;;

module BDT =
    struct

        let build formule =
            let rec aux valuation = function
                | [] -> Leaf(Formule.eval valuation formule)
                | t::q ->
                    begin
                        Valuation.setValue valuation t true;
                        let a = aux valuation q in
                        Valuation.setValue valuation t false;
                        let b = aux valuation q in
                        Node(t, a, b)
                    end
            in
            aux (Valuation.empty ()) (Formule.setVar formule)
        ;;

        let isLeaf = function
            | Leaf(_) -> true
            | _ -> false
        ;;

        let rec reduce tree =
            match tree with
            | Leaf(_) -> tree
            | Node(_, Leaf(b1), Leaf(b2)) -> if b1 == b2 then Leaf(b1) else tree
            | Node(v, t1, t2) ->
                begin
                    let a = reduce t1 in
                    let b = reduce t2 in
                    if (isLeaf a && isLeaf b) then
                        reduce (Node(v, a, b))
                    else
                        Node(v, a, b)
                end
        ;;

        let rec toString tree = 
          match tree with
          |Leaf(true) -> "L(T)"
          |Leaf(false) -> "L(F)"
          |Node(x,l,r) -> "N(" ^ x ^ "," ^ (toString l) ^ "," ^ (toString r) ^ ")"
        ;;
    end
;;

module BDD =
    struct

        let createFromBDT tree =
            let t = Hashtbl.create 10 in
            let i = ref 0 in
            let vrai = DLeaf(true) in
            let faux = DLeaf(false) in
            let rec norm x =
                match x with
                | Leaf(b) -> if b then vrai else faux
                | Node(var, a, b) ->
                    begin
                        let n = DNode(!i, var, norm a, norm b) in
                        let s = BDT.toString x in
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

        let print bdd =
            let getValue = function
                | DLeaf(b) -> if b then "@t" else "@f"
                | DNode(id, _, _, _) -> string_of_int id
            in
            let set = ref IntSet.empty in
            let rec aux (node : bdd) set =
                match node with
                | DLeaf(_) -> ()
                | DNode(id, var, a, b) ->
                    begin
                        if not (IntSet.mem id (!set)) then
                            begin
                                set := IntSet.add id (!set);
                                let s = (string_of_int id) ^ " " ^ var ^ " " ^ (getValue a) ^ " " ^ (getValue b) in
                                print_string (s ^ "\n");
                                aux a set;
                                aux b set;
                            end
                    end
            in
            aux bdd set
        ;;

        let rec evaluate valuation = function
            | DLeaf(b) -> b
            | DNode(_, var, t, f) ->
                if Valuation.getValue valuation var then
                    evaluate valuation t
                else
                    evaluate valuation f
        ;;

        let rec no = function
            | DLeaf(b) -> DLeaf(not b)
            | DNode(id, var, a, b) -> DNode(id, var, no a, no b)
        ;;

        let rec isSatisfiable = function
            | DLeaf(b) -> b
            | DNode(_, _, a, b) -> isSatisfiable a || isSatisfiable b
        ;;

        let rec isValid = function
            | DLeaf(b) -> b
            | DNode(_, _, a, b) -> isValid a && isValid b
        ;;

        let create formule =
            createFromBDT (BDT.reduce (BDT.build formule))
        ;;

    end
;;
