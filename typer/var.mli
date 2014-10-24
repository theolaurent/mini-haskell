type t
val fresh : unit -> t

module Set : Set.S with type elt = t
