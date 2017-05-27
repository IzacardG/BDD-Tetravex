module type Var = sig
    type t
    val equal: t -> t -> bool
    (* compare x y : positive if x > y, 0 if x = y, negative if x < y *)
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
