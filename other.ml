   
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
