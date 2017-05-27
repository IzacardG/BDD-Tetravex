module type Variable = sig
    type t
    val equal: t -> t -> bool
    val compare: t -> t -> int
    val hash: t -> int
    val toString: t -> string
end

module IntVar : Variable with type t = int = struct
    type t = int
    let equal x y = (x = y)
    let compare = Pervasives.compare
    let hash x = x
    let toString = string_of_int
end

module StringVar : Variable with type t = string = struct
    type t = string
    let equal x y = (x = y)
    let compare = Pervasives.compare
    let hash x = int_of_string x
    let toString x = x
end

type 'a formula =
    | Var of 'a
    | True
    | False
    | Not of 'a formula
    | And of 'a formula * 'a formula
    | Or of 'a formula * 'a formula
    | Imp of 'a formula * 'a formula
    | Equi of 'a formula * 'a formula

(* Gauche : true, droite : false *)

type 'a bdt =
    | Leaf of bool
    | Node of 'a * 'a bdt * 'a bdt

type 'a bdd =
    | True
    | False
    | ANode of int * 'a  * 'a bdd * 'a bdd

module IntSet = Set.Make(struct
    type t = int
    let compare = Pervasives.compare
end)
