open Ast
let plus a b = Ast.App (Ast.App (Ast.Const (Ast.CPrim "plus"), a), b)
let lt a b = Ast.App (Ast.App (Ast.Const (Ast.CPrim "lt"), a), b)
let bool_and a b = Ast.App (Ast.App (Ast.Const (Ast.CPrim "and"), a), b)
let ast_if c a b = Ast.App (Ast.App (Ast.App (Ast.Const (Ast.CPrim "if"), c), a), b)
let cint i = Ast.Const (Ast.CInt i)
let cbool b = Ast.Const (Ast.CBool b)
let abstr xlist expr = List.fold_right (fun x e -> Ast.Abstr (x,e)) xlist expr
let letin x e1 e2 = Ast.Let ([x,e1],e2)

let x = "x"
let y = "y"
let z = "z"
let a = "a"

module Primitive = Ast.Primitive

let prim : Schema.schema Ast.Primitive.t =
  let var = Var.fresh () in
  let binop n t r = 
    Ast.Primitive.add n (Schema.ty
		       (Ty.arrow (Ty.constructor t [])
				 (Ty.arrow (Ty.constructor t [])
		       			   (Ty.constructor r []))))
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
  |> Ast.Primitive.add "unary_minus"
		   (Schema.ty (Ty.arrow (Ty.constructor "int" []) (Ty.constructor "int" [])))
  |> Ast.Primitive.add "empty"
		   (Schema.forall (var, Schema.(BFlexible, bot))
				  (Schema.ty
				     (Ty.constructor "list" [Ty.variable var])))
  |> Ast.Primitive.add "cons"
		   (Schema.forall
		      (var, Schema.(BFlexible, bot))
		      (Schema.ty
			 (Ty.arrow (Ty.variable var)
				   (Ty.arrow (Ty.constructor "list" [Ty.variable var])
					     (Ty.constructor "list" [Ty.variable var])))))
  |> Ast.Primitive.add "if"
		   (Schema.forall
		      (var, Schema.(BFlexible,bot))
		      (Schema.ty
			 (Ty.arrow (Ty.constructor "bool" [])
				   (Ty.arrow (Ty.variable var)
					     (Ty.arrow (Ty.variable var)
						       (Ty.variable var))))))
  |> Ast.Primitive.add "putChar"
		   (Schema.ty
		      (Ty.arrow (Ty.constructor "char" [])
				(Ty.constructor "unit" [])))

		     



let t1 =
  let ast = plus (cint 5) (plus (cint 6) (cint 3)) in					  
  Inference.infer prim [] [] ast 

let t2 =
  let ast = lt (plus (cint 5) (cint 2)) (cint 2) in
  Inference.infer prim [] [] ast

let t3 =
  let ast = bool_and (cbool true) (cbool false) in
  Inference.infer prim [] [] ast
		  
		 
let t4 =
  let ast = bool_and (lt (plus (cint 5) (cint 3)) (cint 25)) (cbool true) in
  Inference.infer prim [] [] ast

let t5 =
  let ast = abstr [x] (Ast.Var x) in
  Inference.infer prim [] [] ast

let t6 =
  let ast = abstr [x] (plus (Ast.Var x) (Ast.Var x)) in
  Inference.infer prim [] [] ast
		  
let t7 =  	 
  let ast = abstr [x;y;z;a]
		  (bool_and (lt (plus (Ast.Var x) (Ast.Var y)) (Ast.Var z)) (Ast.Var a)) in
  Inference.infer prim [] [] ast
 
let t8 =
  let ast = Ast.App (Ast.Const (Ast.CPrim "if"), cbool true) in
  Inference.infer prim [] [] ast
		  
let t9 =
  let choose = abstr [x;y] (ast_if (cbool true) (Ast.Var x) (Ast.Var y)) in
  let id = abstr [x] (Ast.Var x) in
  let ast = Ast.App (choose, id) in
  Inference.infer prim [] [] ast
  
let t10 =
  let cond = ast_lt (Var "n") (Const (CInt 0)) in
  let rest = Var "n" in
  let resf = ast_app (Var "fac") [Var "n"] in
 
  let body = ast_app (Var "fac") [Const (CInt 1)] in
  let ast = ast_let ["fac", Abstr ("n", ast_if cond rest resf)] (body) in
  Inference.infer prim [] [] ast


let t11 =
  let body = ast_app (Var "error") [Const (CInt 1)] in
  let ast = ast_let ["error", Abstr ("x", Var "x")] body in
  Inference.infer_mutually_recursive_definitions prim [] [] ["main",ast]
