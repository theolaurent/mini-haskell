(* Printers for types *)
let print_variable ff v =
  Format.fprintf ff "'%s" (Var.to_string v)

let rec print_type ff t =
  match t.Ty.value with
  | Ty.TVar v ->
     Format.fprintf ff "%a" print_variable v
  | Ty.TArrow (t1, t2) ->
     Format.fprintf ff "%a → %a" print_type t1 print_type t2
  | Ty.TConst (constr, tlist) ->
     Format.fprintf ff "%s%a" constr print_type_list tlist
  | Ty.TBot ->
     Format.fprintf ff "⊥"
and print_type_list ff l =
  if List.length l > 1
  then List.iter (Format.fprintf ff " (%a)" print_type) l
  else List.iter (Format.fprintf ff " %a" print_type) l

let print_schema_terminal ff = function
  | Schema.STTy ty -> print_type ff ty
  | Schema.STBot -> Format.fprintf ff "⊥"

let print_bound ff = function
  | Schema.BRigid -> Format.fprintf ff "="
  | Schema.BFlexible -> Format.fprintf ff "≥"


let rec print_schema ff sch =
  let Schema.S (l,term) = sch.Schema.value in
  Format.fprintf ff "(%a%a)"
		 print_bindings l
		 print_schema_terminal term
and print_binding ff (v, (b, s)) =
  Format.fprintf ff "∀%a %a %a"
		print_variable v
		print_bound b
		print_schema s
and print_bindings ff l =
  List.iter (Format.fprintf ff "%a, " print_binding) l
