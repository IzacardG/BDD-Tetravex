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
                    StringBDD.makeNode ((string_of_int b) ^ "," ^ (string_of_int a) ^ ":" ^ (string_of_int dom#id)) False True
                
                method impliqueDroite a b (dom: domino) (l: domino list) =
                    match l with
                    | [] -> False
                    | t::q ->
                        if t#id = dom#id then
                            self#impliqueDroite a b dom q
                        else if (t#gauche = dom#droite) then
                            let f = self#formulePlacement t (a + 1) b in
                            StringBDD.orBDD(f) (self#impliqueDroite a b dom q)
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
                            StringBDD.orBDD f (self#impliqueBas a b dom q)
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
                            StringBDD.andBDD (StringBDD.notBDD(self#formulePlacement dom u v))( aux u (v + 1))
                    in
                    aux 1 1

                method aucunAutre (dom: domino) a b =
                    let rec aux = function
                    | [] -> True
                    | t::q -> if t#id = dom#id then aux q
                        else
                            StringBDD.andBDD (StringBDD.notBDD(self#formulePlacement t a b)) (aux q)
                    in
                    aux dominos

                method placerPotentiellement dom a b =
                    let f1 = if a + 1 <= n then self#impliqueDroite a b dom dominos else True in
                    let f2 = if b + 1 <= p then self#impliqueBas a b dom dominos else True in
                    let v = self#formulePlacement dom a b in
                    let unique = self#placerUnique dom a b in
                    let seul = self#aucunAutre dom a b in
                    StringBDD.implBDD v (StringBDD.andBDD(StringBDD.andBDD(StringBDD.andBDD f1 f2) unique) seul)

                method placerPotentiellementPartout (dom:domino) =
                    let rec aux a b =
                        if a > n then
                            True
                        else if b > p then
                            aux (a + 1) 1
                        else
                            StringBDD.andBDD(self#placerPotentiellement dom a b) (aux a (b + 1))
                    in
                    aux 1 1

                method existence =
                    let rec exist a b = function
                        | [] -> False
                        | t::q -> StringBDD.orBDD(self#formulePlacement t a b)(exist a b q)
                    in
                    let rec aux a b =
                        if a > n then
                            True
                        else if b > p then
                            aux (a + 1) 1
                        else
                            StringBDD.andBDD(exist a b dominos) (aux a (b +1))
                    in
                    aux 1 1

                method toutPlacer = function
                    | [] -> True
                    | t::q -> StringBDD.andBDD (self#placerPotentiellementPartout t) (self#toutPlacer q)

                method solve () =
                    let f1 = self#toutPlacer l in
                    let f2 = self#existence in
                    let bdd = StringBDD.andBDD f1  f2 in
                    let (b, l) = StringBDD.satisfact bdd in
                    if b then List.map (fun (x, _) ->  x) (List.filter (fun (_, x) ->not x) l) else []
            end
end
