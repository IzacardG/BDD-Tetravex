type 'a formula =
    Var of 'a
  | True
  | False
  | Not of 'a formula
  | And of 'a formula * 'a formula
  | Or of 'a formula * 'a formula
  | Imp of 'a formula * 'a formula
  | Equi of 'a formula * 'a formula
type 'a bdt = Leaf of bool | Node of 'a * 'a bdt * 'a bdt
type 'a bdd = DLeaf of bool | DNode of int * 'a * 'a bdd * 'a bdd
type 'a valuation = ('a, bool) Hashtbl.t
module S :
  sig
    type elt = string
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
module IntSet :
  sig
    type elt = int
    type t
    val empty : t
    val is_empty : t -> bool
    val mem : elt -> t -> bool
    val add : elt -> t -> t
    val singleton : elt -> t
    val remove : elt -> t -> t
    val union : t -> t -> t
    val inter : t -> t -> t
    val diff : t -> t -> t
    val compare : t -> t -> int
    val equal : t -> t -> bool
    val subset : t -> t -> bool
    val iter : (elt -> unit) -> t -> unit
    val fold : (elt -> 'a -> 'a) -> t -> 'a -> 'a
    val for_all : (elt -> bool) -> t -> bool
    val exists : (elt -> bool) -> t -> bool
    val filter : (elt -> bool) -> t -> t
    val partition : (elt -> bool) -> t -> t * t
    val cardinal : t -> int
    val elements : t -> elt list
    val min_elt : t -> elt
    val max_elt : t -> elt
    val choose : t -> elt
    val split : elt -> t -> t * bool * t
    val find : elt -> t -> elt
    val of_list : elt list -> t
  end
module Valuation :
  sig
    val getValue : 'a valuation -> 'a -> bool
    val setValue : 'a valuation -> 'a -> bool -> unit
    val empty : unit -> 'a valuation
  end
module Formule :
  sig
    val toString : string formula -> string
    val setVar : S.elt formula -> S.elt list
    val eval : 'a valuation -> 'a formula -> bool
  end
module BDT :
  sig
    val build : S.elt formula -> S.elt bdt
    val isLeaf : 'a bdt -> bool
    val reduce : 'a bdt -> 'a bdt
    val toString : string bdt -> string
  end
module BDD :
  sig
    val simpleFromBDT : 'a bdt -> 'a bdd
    val toString : string bdd -> string
    val optimize : string bdd -> string bdd
    val createFromBDT : string bdt -> string bdd
    val print : string bdd -> unit
    val evaluate : 'a valuation -> 'a bdd -> bool
    val no : 'a bdd -> 'a bdd
    val optimized_combine :
      (bool -> bool -> bool) -> string bdd -> string bdd -> string bdd
    val combine : (bool -> bool -> bool) -> 'a bdd -> 'a bdd -> 'a bdd
    val isCombined : (bool -> bool -> bool) -> 'a bdd -> bool
    val andBDD : string bdd -> string bdd -> string bdd
    val orBDD : string bdd -> string bdd -> string bdd
    val imp : string bdd -> string bdd -> string bdd
    val equi : string bdd -> string bdd -> string bdd
    val isSatisfiable : 'a bdd -> bool
    val isValid : 'a bdd -> bool
    val satisfact : 'a bdd -> bool * ('a * bool) list
    val createBis : string formula -> string bdd
    val create : S.elt formula -> S.elt bdd
  end
module Tetravex :
  sig
    class domino :
      int ->
      int ->
      int ->
      int ->
      int ->
      object
        method bas : int
        method droite : int
        method gauche : int
        method haut : int
        method id : int
      end
    class tetravex :
      int ->
      int ->
      domino list ->
      object
        val dominos : domino list
        val n : int
        val p : int
        method aucunAutre : domino -> int -> int -> string formula
        method existence : string formula
        method formulePlacement : domino -> int -> int -> string formula
        method impliqueBas :
          int -> int -> domino -> domino list -> string formula
        method impliqueDroite :
          int -> int -> domino -> domino list -> string formula
        method placerPotentiellement : domino -> int -> int -> string formula
        method placerPotentiellementPartout : domino -> string formula
        method placerUnique : domino -> int -> int -> string formula
        method solve : unit -> string list
        method toutPlacer : domino list -> string formula
      end
  end
