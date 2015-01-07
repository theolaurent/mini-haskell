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
let list t = Ty.constructor "List" [t]
let io t = Ty.constructor "IO" [t]

let arithmetic = ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer"
let comparison = ty @@ !!"Integer" @-> !!"Integer" @-> !!"Bool"
let logical = ty @@ !!"Bool" @-> !!"Bool" @-> !!"Bool"
let cons = (forall ??var (ty @@ tvar @-> list tvar @-> list tvar))
			  
(*let prim : Schema.schema Ast.Primitive.t =
  let binop n t r = 
    Ast.Primitive.add
      n (ty @@ !!t @-> !!t @-> !!r)
  in
  Ast.Primitive.empty
  |> binop "plus"  "Integer"  "Integer"
  |> binop "minus" "Integer"  "Integer"
  |> binop "mult"  "Integer"  "Integer"
  |> binop "lt"    "Integer"  "Bool"
  |> binop "gt"    "Integer"  "Bool"
  |> binop "leq"   "Integer"  "Bool"
  |> binop "geq"   "Integer"  "Bool"
  |> binop "eq"    "Integer"  "Bool"
  |> binop "neq"   "Integer"  "Bool"
  |> binop "and"   "Bool" "Bool"
  |> binop "or"    "Bool" "Bool"
  |> Ast.Primitive.add "empty"
		       (forall ??var (ty @@ list tvar))
  |> Ast.Primitive.add "cons"
 *)
let env =
  [
    ("putChar", ty @@ !!"Char" @-> io !!"()") ;
    ("rem", ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer") ;
    ("div", ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer") ;
    ("error", forall ??var (Schema.ty @@ list !!"Char" @-> tvar))
  ]
