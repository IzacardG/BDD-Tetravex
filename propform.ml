type 'a formula =
  |Var of 'a
  |True
  |False
  |Not of 'a formula
  |And of 'a formula * 'a formula
  |Or of 'a formula * 'a formula
  |Imp of 'a formula * 'a formula
  |Equi of 'a formula * 'a formula

let rec value valuation v =
  match valuation with
  |[] -> failwith "variable not asigned"
  |(e,b)::tl -> if (e == v) then b
    else value tl v

let rec eval valuation = function
  |True          -> true
  |False         -> false
  |Var v         -> value valuation v
  |Not e         -> not (eval valuation e)
  |And (e1, e2)  -> (eval valuation e1) && (eval valuation e2)
  |Or (e1, e2)   -> (eval valuation e1) || (eval valuation e2)
  |Imp  (e1, e2) -> (eval valuation e2) || not (eval valuation e1)
  |Equi (e1, e2) -> (eval valuation (Imp(e1,e2))) && (eval valuation (Imp(e2, e1)))

let rec to_string = function
  | True        -> "true"
  | False       -> "false"
  | Var e       -> e
  | Not e       -> "(! " ^ (to_string e) ^ ")"
  | And (e1, e2)   -> "(" ^ (to_string e1) ^ " && " ^ (to_string e2) ^ ")"
  | Or (e1,  e2)    -> "(" ^ (to_string e1) ^ " || " ^ (to_string e2) ^ ")"
  | Imp (e1, e2)  -> "(" ^ (to_string e1) ^ " -> " ^ (to_string e2) ^ ")"
  | Equi (e1, e2)  ->  "(" ^ (to_string e1) ^ " <-> " ^ (to_string e2) ^ ")"

let rec setVar f =
  let rec aux f = match f with
  | True ->  []
  | False -> []
  | Var i -> [i]
  | Not e1 -> aux e1
  | And (e1, e2) -> (aux e1)@(aux e2)
  | Or (e1, e2) -> (aux e1)@(aux e2)
  | Imp (e1, e2) -> (aux e1)@(aux e2)
  | Equi (e1, e2) -> (aux e1)@(aux e2)
  and unique l =
        match l with
        |[] -> []
        |a::b -> if List.mem a b then unique b else a::unique b
    in unique(aux f)

type bdd =
  |Leaf of bool
  |Node of int * bdd * bdd

type bdt =
  |Leaf of bool
  |Node of int * bdt * bdt

let formula2bdt formule =
  let rec help setVar valuation =
    match setVar with
    |[] -> Leaf(eval valuation formule)
    |hd::tl ->  Node(hd, help tl ((hd, true):: valuation), help tl ((hd, false)::valuation))
  in  help (setVar formule) []

let reduce_bool = function
  |Leaf(p) -> Leaf(p)
  |Node(x, Leaf(l), Leaf(r)) -> if l = r then Leaf(l) else Node(x, Leaf(l), Leaf(r))
  |Node(x, l, r) -> Node(x, l, r)

let rec contains q e =
  match q  with
  | [] -> false
  | a::b -> if e=a then true else contains b e

let rec locate q node =
  
    
let build tree bdd nodeSet =
  match tree with
  | Leaf _ -> bdd, nodeSet
  | Node(x,l,r) -> if contains nodeSet tree
    then bdd, nodeSet
    else if contains nodeSet l
    then if contains nodeSet r
      then (Node(List.length bdd + 1, x, locate l bdd nodeSet, locate r bdd nodeSet)::bdd, Node(x,l,r)::nodeSet)
      else let (bddR, nodeSetR) = build r bdd nodeSet in
        (Node(List.length bdd + 1, x, locate l bdd nodeSet, locate r bdd nodeSet)::bddD, Node(x, l, r)::nodeSetR)
else if contains nodeSet r
then let (bddL, nodeSetL) = build l bdd nodeSet in
  Node(List.length bdd + 1, x, locate l bdd nodeSet, locate r bdd nodeSet)::bddL, 


