type constructor = string

type 'a t = private
	      TConst of  constructor * ('a t) list
	    | TVar of Var.t
	    | TArrow of ('a t) * ('a t)
	    | TBot

type ty = [`Ty | `Skeleton] t
type skeleton = [`Skeleton] t
	
val constructor : constructor -> ([< `Ty | `Skeleton] as 'a) t list -> 'a t

val variable : Var.t -> ty
			  
val arrow : ([< `Ty | `Skeleton] as 'a) t -> 'a t -> 'a t

val bot : unit -> skeleton

val skeleton_of_ty : ty -> skeleton
			      
val subst : Var.t -> ([< `Ty | `Skeleton] as 'a) t -> 'a t -> 'a t
val occur : Var.t ->  [< `Ty | `Skeleton] t -> bool
val variables : [< `Ty | `Skeleton] t -> Var.Set.t
