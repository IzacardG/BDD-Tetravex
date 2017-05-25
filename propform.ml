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

type 'a bdt =
    |Leaf of bool
    |Node of 'a * 'a bdt * 'a bdt
;;

type 'a bdd =
    |DLeaf of bool
    |DNode of int * 'a * 'a bdd * 'a bdd
;;

type 'a valuation = ('a, bool) Hashtbl.t;;

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
        let getValue (valuation: 'a valuation) v =
            if (not (Hashtbl.mem valuation v)) then
                false
            else
                Hashtbl.find valuation v
        ;;

        let setValue (valuation: 'a valuation) v x =
            Hashtbl.add valuation v x
        ;;

        let empty () : 'a valuation =
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

        let rec eval (valuation: 'a valuation) = function
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
            let i = ref 0 in
            let rec aux k valuation = function
                | [] -> k (Leaf(Formule.eval valuation formule))
                | t::q ->
                    begin
                        (* print_string ((string_of_int !i) ^ ","); *)
                        i := !i + 1;
                        let k0 a b =
                            k (Node(t, a, b))
                        in
                        let k1 a =
                            Valuation.setValue valuation t false;
                            aux (k0 a) valuation q
                        in
                        Valuation.setValue valuation t true;
                        aux k1 valuation q
                    end
            in
            aux (fun x -> x) (Valuation.empty ()) (Formule.setVar formule)
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
      let bdd_equal = function
        |DLeaf(true), DLeaf(true) -> true
        |DLeaf(false), DLeaf(false) -> true
        |DLeaf(false), DLeaf(true) -> false
        |DLeaf(true), DLeaf(false) -> false
        |DLeaf(_), _ -> false
        |_, DLeaf(_)-> false
        |DNode(x1,_,_,_), DNode(x2, _, _,_) -> x1 = x2
      ;;
        
        let simpleFromBDT tree =
            let rec norm x =
                match x with
                | Leaf(b) -> DLeaf(b)
                | Node(var, a, b) -> DNode(0, var, norm a, norm b) 
            in
            norm tree
        ;;

        let rec toString bdd = 
          match bdd with
          |DLeaf(true) -> "L(T)"
          |DLeaf(false) -> "L(F)"
          |DNode(_, x,l,r) -> "N(" ^ x ^ "," ^ (toString l) ^ "," ^ (toString r) ^ ")"
        ;;

        let optimize bdd =
            let t = Hashtbl.create 10 in
            let i = ref 0 in
            let rec norm x =
                match x with
                | DLeaf(_) -> x
                | DNode(_, var, a, b) ->
                    begin
                        let n = DNode(!i, var, norm a, norm b) in
                        let s = toString x in
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
            norm bdd
        ;;

        let createFromBDT tree = optimize (simpleFromBDT tree);;

        let print bdd =
            let getValue = function
                | DLeaf(b) -> if b then "@t" else "@f"
                | DNode(id, _, _, _) -> string_of_int id
            in
            let set = ref IntSet.empty in
            let rec aux node set =
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

        let optimized_combine f t1 t2 =
            let t = Hashtbl.create 10 in
            let i = ref 0 in
            let rec aux f t1 t2 =
                let n =
                match (t1, t2) with
                | (DLeaf(b1), DLeaf(b2)) -> DLeaf(f b1 b2)
                | (DLeaf(b1), DNode(_, var, a, b)) -> DNode(0, var, aux f t1 a, aux f t1 b)
                | (DNode(_, var, a, b), DLeaf(b2)) -> DNode(0, var, aux f a t2, aux f b t2)
                | (DNode(_, var1, a1, b1), DNode(_, var2, a2, b2)) ->
                    if var1 = var2 then
                        DNode(0, var1, aux f a1 a2, aux f b1 b2)
                    else if var1 < var2 then
                        DNode(0, var1, aux f a1 t2, aux f b1 t2)
                    else
                        DNode(0, var2, aux f t1 a2, aux f t1 b2)
                in
                begin
                    let s = toString n in
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
            aux f t1 t2
        ;;

        (* Suppose que les BDD sont triés de la même façon au niveau des variables *)
        let rec combine f t1 t2 =
            match (t1, t2) with
            | (DLeaf(b1), DLeaf(b2)) -> DLeaf(f b1 b2)
            | (DLeaf(b1), DNode(_, var, a, b)) -> DNode(0, var, combine f t1 a, combine f t1 b)
            | (DNode(_, var, a, b), DLeaf(b2)) -> DNode(0, var, combine f a t2, combine f b t2)
            | (DNode(_, var1, a1, b1), DNode(_, var2, a2, b2)) ->
                if var1 = var2 then
                    DNode(0, var1, combine f a1 a2, combine f b1 b2)
                else if var1 < var2 then
                    DNode(0, var1, combine f a1 t2, combine f b1 t2)
                else
                    DNode(0, var2, combine f t1 a2, combine f t1 b2)
        ;;

        let rec isCombined f = function
            | DLeaf(b) -> b
            | DNode(_, _, a, b) -> f (isCombined f a) (isCombined f b)
        ;;
        
        let andBDD = combine (fun x y -> x && y);;
        let orBDD = combine (fun x y -> x || y);;
        let imp = combine (fun x y -> (not x) || y);;
        let equi = combine (fun x y -> (x && y) || ((not x) && (not y)));;

        let isSatisfiable a = isCombined (fun x y -> x || y) a;;
        let isValid a = isCombined (fun x y -> x && y) a;;

        let rec satisfact = function
            | DLeaf(b) -> (b, [])
            | DNode(_, var, a, b) -> let (b1, l1) = satisfact a in
            if b1 then (true, (var, true)::l1)
            else let (b2, l2) = satisfact b in (b2, (var, false)::l2)
        ;;

        let rec createBis = function
            | True        -> DLeaf(true)
            | False       -> DLeaf(false)
            | Var (e)      -> DNode(0, e, DLeaf(true), DLeaf(false))
            | Not(e)       -> no (createBis e)
            | And (e1, e2)   -> andBDD (createBis e1) (createBis e2)
            | Or (e1,  e2)    -> orBDD (createBis e1) (createBis e2)
            | Imp (e1, e2)  -> imp (createBis e1) (createBis e2)
            | Equi (e1, e2)  -> equi (createBis e1) (createBis e2)
        ;;

        let create formule =
            createFromBDT (BDT.reduce (BDT.build formule))
        ;;

    end
;;

module Tetravex =
    struct

        class domino (h: int) (b: int) (g: int) (d: int) (i: int) =
            object
                method id = i
                method haut = h
                method bas = b
                method gauche = g
                method droite = d
            end
        ;;

        class tetravex (n: int) (p: int) (l: domino list) =
            object (self)

                val n = n
                val p = p
                val dominos = l

                method formulePlacement dom a b =
                    Var((string_of_int a) ^ "," ^ (string_of_int b) ^ ":" ^ (string_of_int dom#id))
                
                method impliqueDroite a b (dom: domino) (l: domino list) =
                    match l with
                    | [] -> False
                    | t::q ->
                        if t#id = dom#id then
                            self#impliqueDroite a b dom q
                        else if (t#gauche = dom#droite) then
                            let f = self#formulePlacement t (a + 1) b in
                            Or(f, self#impliqueDroite a b dom q)
                        else
                            self#impliqueDroite a b dom q

                method impliqueBas a b (dom: domino) (l: domino list) =
                    match l with
                    | [] -> False
                    | t::q ->
                        if t#id = dom#id then
                            self#impliqueBas a b dom q
                        else if (t#haut = dom#bas) then
                            let f = self#formulePlacement t a (b + 1) in
                            Or(f, self#impliqueBas a b dom q)
                        else
                            self#impliqueBas a b dom q

                method placerUnique dom a b =
                    let rec aux u v =
                        if u > n then
                            True
                        else if v > p then
                            aux (u + 1) 1
                        else if (u = a && v = b) then
                            aux u (v + 1)
                        else
                            And(Not(self#formulePlacement dom u v), aux u (v + 1))
                    in
                    aux 1 1

                method aucunAutre (dom: domino) a b =
                    let rec aux = function
                    | [] -> True
                    | t::q -> if t#id = dom#id then aux q
                        else
                            And(Not(self#formulePlacement t a b), aux q)
                    in
                    aux dominos

                method placerPotentiellement dom a b =
                    let f1 = if a + 1 <= n then self#impliqueDroite a b dom dominos else True in
                    let f2 = if b + 1 <= p then self#impliqueBas a b dom dominos else True in
                    let v = self#formulePlacement dom a b in
                    let unique = self#placerUnique dom a b in
                    let seul = self#aucunAutre dom a b in
                    Imp(v, And(And(And(f1, f2), unique), seul))

                method placerPotentiellementPartout dom =
                    let rec aux a b =
                        if a > n then
                            True
                        else if b > p then
                            aux (a + 1) 1
                        else
                            And(self#placerPotentiellement dom a b, aux a (b + 1))
                    in
                    aux 1 1

                method existence =
                    let rec exist a b = function
                        | [] -> False
                        | t::q -> Or(self#formulePlacement t a b, exist a b q)
                    in
                    let rec aux a b =
                        if a > n then
                            True
                        else if b > p then
                            aux (a + 1) 1
                        else
                            And(exist a b dominos, aux a (b +1))
                    in
                    aux 1 1

                method toutPlacer = function
                    | [] -> True
                    | t::q -> And(self#placerPotentiellementPartout t, self#toutPlacer q)

                method solve () =
                    let f1 = self#toutPlacer l in
                    let f2 = self#existence in
                    let f = And(f1, f2) in
                    let bdd = BDD.createBis f in
                    let (b, l) = BDD.satisfact bdd in
                    if b then List.map (fun (x, _) -> x) (List.filter (fun (_, x) -> x) l) else []
            end
        ;;

    end
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
let l = [d1;d2;d3;d4] in
let t = new Tetravex.tetravex 2 2 l in

List.iter (fun x -> print_string (x ^ ", ")) (t#solve ());;

