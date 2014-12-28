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

let prim : Schema.schema Ast.Primitive.t =
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
  |> Ast.Primitive.add "unary_minus"
		       (ty @@ !!"Integer" @-> !!"Integer")
  |> Ast.Primitive.add "empty"
		       (forall ??var (ty @@ list tvar))
  |> Ast.Primitive.add "cons"
		       (forall ??var (ty @@ tvar @-> list tvar @-> list tvar))
  |> Ast.Primitive.add "if"
		       (forall ??var (ty @@ !!"Bool" @-> tvar @-> tvar @-> tvar))

  (* do (do a b) c 'a -> ('b -> 'b) *)
  |> Ast.Primitive.add "do"
		       (forall_map
			  [??var ; ??var2]
			  (ty @@ io tvar @-> io tvar2 @-> io tvar2))
			  
  |> Ast.Primitive.add "return ()"
		       (ty @@ io !!"()")
		       
  |> Ast.Primitive.add "match"
		       (forall_map
			  [??var ; ??var2]
			  (ty @@
			     list tvar
			     @-> tvar2
			     @-> (tvar @-> list tvar @-> tvar2)
			     @-> tvar2))

let env =
  [
    ("putChar", ty @@ !!"Char" @-> io !!"()") ;
    ("rem", ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer") ;
    ("div", ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer") ;
    ("error", forall ??var (Schema.ty @@ list !!"Char" @-> tvar))
  ]
