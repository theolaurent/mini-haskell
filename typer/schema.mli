type bound =
  | BRigid
  | BFlexible

type schema_terminal =
  | STBot
  | STTy of Ty.ty
      
type schema =
    {
      free_variables : Var.Set.t ;
      value : schema_value
    }
 and schema_value = S of constr list * schema_terminal
 and constr = Var.t * (bound * schema)
      

type prefix = constr list
			
val bot : schema
val ty : Ty.ty -> schema
val terminal : schema_terminal -> schema
val forall : constr -> schema -> schema
val forall_map : prefix -> schema -> schema

val subst : Var.t -> Ty.ty -> schema -> schema

(* Replace all bounded variable by the schema they are bound to *)
val proj : schema_value -> Ty.skeleton

val is_free : Var.t -> schema -> bool
val free_variables : schema -> Var.Set.t

(* normal form of equivalent schemas are equal up to permutations of some bindings *)
val normal_form : schema_value -> schema

val constructed_form : schema_value -> schema
			      
val rename : prefix -> schema_value -> prefix * Ty.ty
  
val split : prefix -> Var.Set.t -> prefix * prefix
					       
