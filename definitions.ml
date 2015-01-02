open Typer

let ty = Schema.ty
let forall = Schema.forall
let forall_map = Schema.forall_map
	   
let var = Var.fresh ()
let tvar = Ty.variable var
let var2 = Var.fresh ()
let tvar2 = Ty.variable var2

(* @ is used to have right associativity *)
let (@->) = Ty.arrow
let (??) v = (v, Schema.(BFlexible, bot))
let (!!) t = Ty.constructor t []
let list t = Ty.constructor "list" [t]
let io t = Ty.constructor "IO" [t]

let prim : Schema.schema Ast.Primitive.t =
  let binop n t r = 
    Ast.Primitive.add
      n (ty @@ !!t @-> !!t @-> !!r)
  in
  Ast.Primitive.empty
  |> binop "plus"  "int"  "int"
  |> binop "minus" "int"  "int"
  |> binop "mult"  "int"  "int"
  |> binop "lt"    "int"  "bool"
  |> binop "gt"    "int"  "bool"
  |> binop "leq"   "int"  "bool"
  |> binop "geq"   "int"  "bool"
  |> binop "eq"    "int"  "bool"
  |> binop "neq"   "int"  "bool"
  |> binop "and"   "bool" "bool"
  |> binop "or"    "bool" "bool"
  |> Ast.Primitive.add "empty"
		       (forall ??var (ty @@ list tvar))
  |> Ast.Primitive.add "cons"
		       (forall ??var (ty @@ tvar @-> list tvar @-> list tvar))


let env =
  [
    ("putChar", ty @@ !!"char" @-> io !!"unit") ;
    ("rem", ty @@ !!"int" @-> !!"int" @-> !!"int") ;
    ("div", ty @@ !!"int" @-> !!"int" @-> !!"int") ;
    ("error", forall ??var (Schema.ty @@ list !!"char" @-> tvar))
  ]
