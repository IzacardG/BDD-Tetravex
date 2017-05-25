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
;;
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
            Hashtbl.create 10000
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
            | Var s ->S.singleton s
            | Not e1 -> aux e1
            | And (e1, e2) ->S.union (aux e1) (aux e2)
            | Or (e1, e2) ->S.union (aux e1) (aux e2)
            | Imp (e1, e2) ->S.union (aux e1) (aux e2)
            | Equi (e1, e2) ->S.union (aux e1) (aux e2)
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
          |Node(x,l,r) -> "N(" ^x ^ "," ^ (toString l) ^ "," ^ (toString r) ^ ")"
        ;;
    end
;;



type bdd = |True
           |False
           |ANode of int * string  * bdd * bdd;;


module BDD =
struct
  
  let getID = function
    |False -> 0
    |True -> 1
    |ANode(x,_,_,_) -> x;;
  
  let equalBDD bdd1 bdd2 = (getID bdd1) = (getID bdd2);;

  let bij x y =
    (x+y)*(x+y+1)/2  + x
    
  module H = struct
    type t = string*int*int
    let equal (v1, g1, d1) (v2, g2, d2) =
      v1 = v2 && g1 = g2 && d1 = d2
    let hash (v, g, d) =
      Hashtbl.hash (v,g,d)
  end
  
  module HBDD = Hashtbl.Make(H)
    
  let currID = ref 2
  let nodeSet  = HBDD.create 2047
    
    
  let makeNode v g d =
    let idg = getID g in
    let idd = getID d in
    if idg = idd
    then g
    else
      try
        HBDD.find nodeSet (v, idg, idd)
      with Not_found ->
        let newID = !currID
        in currID := newID + 1;
        let newNode = ANode(newID,v,g,d)
        in HBDD.add nodeSet (v, idg, idd) newNode;
        newNode

          
          
  module H2 = struct
    type t = int * int
    let equal (a1,b1) (a2,b2) =
      a1 = a2 && b1 = b2
    let hash (a,b) = Hashtbl.hash (a, b)
  end
    
  module HPairBDD = Hashtbl.Make(H2) 

    let notContainer =  Hashtbl.create 5000
      
    let rec notBDD bdd =
      match bdd with
      | False -> True
      | True -> False
      | ANode(id, v, g, d) ->
        try
          Hashtbl.find notContainer id
        with Not_found ->
          let newNode = makeNode v (notBDD g)  (notBDD d)
          in Hashtbl.add notContainer id newNode;
          newNode
            
    let andContainer = HPairBDD.create 5000
      
    let rec andBDD bdd1 bdd2 =
      match bdd1, bdd2 with
      |True, _ -> bdd2
      |False, _ -> False
      |_, True -> bdd1
      |_, False -> False
      |ANode(id1, v1, g1, d1), ANode(id2, v2, g2, d2) ->
         if id1 = id2
         then
           bdd1
         else
           let key = if id1 < id2 then (id1, id2) else (id2, id1)
           in try HPairBDD.find andContainer key
               with Not_found ->
                 let newNode =
                   if v1 = v2
                   then
                     makeNode v1 (andBDD g1 g2) (andBDD d1 d2)
                   else if v1 < v2
                   then
                     makeNode v1 (andBDD g1 bdd2) (andBDD d1 bdd2)
                   else
                     makeNode v2 (andBDD bdd1 g2) (andBDD bdd1 d2)
                 in HPairBDD.add andContainer key newNode;
                 newNode

    let orContainer = HPairBDD.create 5000
      
    let rec orBDD bdd1 bdd2 =
      match bdd1, bdd2 with
      |True, _ -> True
      |False, _ -> bdd2
      |_, True -> True
      |_, False -> bdd1
      |ANode(id1, v1, g1, d1), ANode(id2, v2, g2, d2) ->
         if id1 = id2
         then
           bdd1
         else
           let key = if id1 < id2 then (id1, id2) else (id2, id1)
           in try HPairBDD.find orContainer key 
               with Not_found ->
                 let newNode =
                   if v1 = v2
                   then
                     makeNode v1 (orBDD g1 g2) (orBDD d1 d2)
                   else if v1 < v2
                   then
                     makeNode v1 (orBDD g1 bdd2) (orBDD d1 bdd2)
                   else
                     makeNode v2 (orBDD bdd1 g2) (orBDD bdd1 d2)
                 in HPairBDD.add orContainer (id1, id2) newNode;
                 newNode
                   
    let implBDD bdd1 bdd2 =
      orBDD (notBDD bdd1) bdd2
        
    let equiBDD bdd1 bdd2 =
      orBDD (andBDD bdd1 bdd2) (andBDD (notBDD bdd1) (notBDD bdd2)) 
        
        
    let fromBDT tree =
      let rec help x =
        match x with
        |Leaf(true) -> True
        |Leaf(false) -> False
        |Node(var, a, b) ->
           let g = help a
           in let d = help b
              in makeNode var g d
      in help tree
        

    let rec evaluate valuation = function
      | True -> true
      | False -> false
      | ANode(id, var, t, f) ->
        if Valuation.getValue valuation var then
          evaluate valuation t
        else
          evaluate valuation f
            

    let rec isCombined f = function
      | True -> true
      | False -> false
      | ANode(_, _, a, b) -> f (isCombined f a) (isCombined f b)
                               

    let isSatisfiable = isCombined (fun x y -> x || y);;
    let isValid = isCombined (fun x y -> x && y);;

    let rec satisfact = function
      | True -> (true, [])
      | False -> (false, [])
      | ANode(_, var, a, b) -> let (b1, l1) = satisfact a in
        if b1 then (true, (var, true)::l1)
        else let (b2, l2) = satisfact b in (b2, (var, false)::l2)
                                           

    let create formule = fromBDT(BDT.reduce (BDT.build formule));;

    let print bdd =
      let getValue = function
        | True ->  "@t"
        | False -> "@f"
        | ANode(id, _, _, _) -> string_of_int id
      in
      let set = ref IntSet.empty in
      let rec aux (node : bdd) set =
        match node with
        | True -> ()
        |False -> ()
        | ANode(id, var, a, b) ->
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
        
            
    let rec createBDD (formule: string formula) = match formule with
      |True          -> True
      |False         -> False
      |Var v         -> makeNode v False True
      |Not e         -> notBDD (createBDD e)
      |And (e1, e2)  -> andBDD (createBDD  e1)  (createBDD e2)
      |Or (e1, e2)   -> orBDD (createBDD e1)  (createBDD e2)
      |Imp  (e1, e2) -> implBDD (createBDD e1) (createBDD e2)
      |Equi (e1, e2) -> equiBDD (createBDD e1) (createBDD e2)

end



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
        

        class tetravex (n: int) (p: int) (l: domino list) =
            object (self)

                val n = n
                val p = p
                val dominos = l

                method formulePlacement dom a b =
                    BDD.makeNode ((string_of_int b) ^ "," ^ (string_of_int a) ^ ":" ^ (string_of_int dom#id)) False True
                
                method impliqueDroite a b (dom: domino) (l: domino list) =
                    match l with
                    | [] -> False
                    | t::q ->
                        if t#id = dom#id then
                            self#impliqueDroite a b dom q
                        else if (t#gauche = dom#droite) then
                            let f = self#formulePlacement t (a + 1) b in
                            BDD.orBDD(f) (self#impliqueDroite a b dom q)
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
                            BDD.orBDD f (self#impliqueBas a b dom q)
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
                            BDD.andBDD (BDD.notBDD(self#formulePlacement dom u v))( aux u (v + 1))
                    in
                    aux 1 1

                method aucunAutre (dom: domino) a b =
                    let rec aux = function
                    | [] -> True
                    | t::q -> if t#id = dom#id then aux q
                        else
                            BDD.andBDD (BDD.notBDD(self#formulePlacement t a b)) (aux q)
                    in
                    aux dominos

                method placerPotentiellement dom a b =
                    let f1 = if a + 1 <= n then self#impliqueDroite a b dom dominos else True in
                    let f2 = if b + 1 <= p then self#impliqueBas a b dom dominos else True in
                    let v = self#formulePlacement dom a b in
                    let unique = self#placerUnique dom a b in
                    let seul = self#aucunAutre dom a b in
                    BDD.implBDD v (BDD.andBDD(BDD.andBDD(BDD.andBDD f1 f2) unique) seul)

                method placerPotentiellementPartout (dom:domino) =
                    let rec aux a b =
                        if a > n then
                            True
                        else if b > p then
                            aux (a + 1) 1
                        else
                            BDD.andBDD(self#placerPotentiellement dom a b) (aux a (b + 1))
                    in
                    aux 1 1

                method existence =
                    let rec exist a b = function
                        | [] -> False
                        | t::q -> BDD.orBDD(self#formulePlacement t a b)(exist a b q)
                    in
                    let rec aux a b =
                        if a > n then
                            True
                        else if b > p then
                            aux (a + 1) 1
                        else
                            BDD.andBDD(exist a b dominos) (aux a (b +1))
                    in
                    aux 1 1

                method toutPlacer = function
                    | [] -> True
                    | t::q -> BDD.andBDD (self#placerPotentiellementPartout t) (self#toutPlacer q)

                method solve () =
                    let f1 = self#toutPlacer l in
                    let f2 = self#existence in
                    let bdd = BDD.andBDD f1  f2 in
                    let (b, l) = BDD.satisfact bdd in
                    if b then List.map (fun (x, _) ->  x) (List.filter (fun (_, x) ->not x) l) else []
            end
end



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
