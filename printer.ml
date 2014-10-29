
(* Printers for ast *)
let print_const ff = function
  | Ast.CUnit -> Format.fprintf ff "()"
  | Ast.CBool b -> Format.fprintf ff "%b" b
  | Ast.CInt i -> Format.fprintf ff "%d" i
  | Ast.CChar c -> Format.fprintf ff "'%s'" (Char.escaped c)
  | Ast.CPrim "empty" -> Format.fprintf ff "[]"
  | Ast.CPrim s -> Format.fprintf ff "%s" s

let rec print_ast ff = function
  | Ast.Const c -> print_const ff c
  | Ast.Var x -> Format.fprintf ff "%s" x
  | Ast.Constructor str -> Format.fprintf ff "%s" str
  | Ast.Abstr (x, body) ->
     Format.fprintf ff "(λ%s. %a)" x print_ast body
  | Ast.App (x, y) ->
     Format.fprintf ff "(%a %a)" print_ast x print_ast y
  | Ast.Let (l, e) ->
     Format.fprintf ff "let { %a} in %a" print_def_list l print_ast e
  and print_def_list ff l =
    List.iter (fun (x,b) -> Format.fprintf ff "%s = %a; " x print_ast b) l
		    
(* Printers for types *)
let print_variable ff v =
  Format.fprintf ff "'%s" (Var.to_string v)

let rec print_type ff = function
  | Ty.TVar v ->
     Format.fprintf ff "%a" print_variable v
  | Ty.TArrow (t1, t2) ->
     Format.fprintf ff "(%a → %a)" print_type t1 print_type t2
  | Ty.TConst (constr, tlist) ->
     Format.fprintf ff "(%s%a)" constr print_type_list tlist
  | Ty.TBot ->
     Format.fprintf ff "⊥"
and print_type_list ff l =
  List.iter (Format.fprintf ff " %a" print_type) l

let print_schema_terminal ff = function
  | Schema.STTy ty -> print_type ff ty
  | Schema.STBot -> Format.fprintf ff "⊥"

let print_bound ff = function
  | Schema.BRigid -> Format.fprintf ff "="
  | Schema.BFlexible -> Format.fprintf ff "≥"

				       
let rec print_schema ff (Schema.S (l,term)) =
  Format.fprintf ff "%a%a"
		 print_bindings l
		 print_schema_terminal term
and print_binding ff (v, (b, s)) =
  Format.fprintf ff "∀%a %a %a"
		print_variable v
		print_bound b
		print_schema s
and print_bindings ff l =
  List.iter (Format.fprintf ff "%a," print_binding) l
