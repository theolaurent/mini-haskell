type bound =
  | BRigid
  | BFlexible
      
type schema =
  | SBot
  | STy of Ty.ty
  | SForall of constr * schema
 and constr = Var.t * (bound * schema)

type prefix = constr list
			
val forall_map : prefix -> schema -> schema


val subst : Var.t -> Ty.ty -> schema -> schema

(* Replace all bounded variable by the schema they are bound to *)
val proj : schema -> Ty.skeleton

val is_free : Var.t -> schema -> bool
val free_variables : schema -> Var.Set.t

(* normal form of equivalent schemas are equal up to permutations of some bindings *)
val normal_form : schema -> schema
			      
val split : prefix -> Var.t list -> prefix * prefix
					       
