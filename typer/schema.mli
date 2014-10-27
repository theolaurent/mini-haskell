type bound =
  | BRigid
  | BFlexible

type schema_terminal =
  | STBot
  | STTy of Ty.ty
      
type schema = S of constr list * schema_terminal
 and constr = Var.t * (bound * schema)
      

type prefix = constr list
			
val bot : schema
val ty : Ty.ty -> schema
val forall : constr -> schema -> schema
val forall_map : prefix -> schema -> schema

val subst : Var.t -> Ty.ty -> schema -> schema

(* Replace all bounded variable by the schema they are bound to *)
val proj : schema -> Ty.skeleton

val is_free : Var.t -> schema -> bool
val free_variables : schema -> Var.Set.t

(* normal form of equivalent schemas are equal up to permutations of some bindings *)
val normal_form : schema -> schema

val constructed_form : schema -> schema
			      
val split : prefix -> Var.Set.t -> prefix * prefix
					       
