module type Variable = sig
  type t
  val equal: t -> t -> bool
  (* compare x y : positive if x > y, 0 if x = y, negative if x < y *)
  val compare: t -> t -> int
  val hash: t -> int
  val toString: t -> string
end



