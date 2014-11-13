type t = int
val fresh : unit -> t
val to_string : t -> string
		       
module Set : Set.S with type elt = t
