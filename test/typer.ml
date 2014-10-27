
let plus a b = Ast.App (Ast.App (Ast.Const (Ast.CPrim "+"), a), b)
let lt a b = Ast.App (Ast.App (Ast.Const (Ast.CPrim "<"), a), b)
let bool_and a b = Ast.App (Ast.App (Ast.Const (Ast.CPrim "&&"), a), b)
let cint i = Ast.Const (Ast.CInt i)
let cbool b = Ast.Const (Ast.CBool b)
let abstr xlist expr = List.fold_right (fun x e -> Ast.Abstr (x,e)) xlist expr
let letin x e1 e2 = Ast.Let (x,e1,e2)

let x = "x"
let y = "y"
let z = "z"
let a = "a"

let t1 =
  let ast = plus (cint 5) (plus (cint 6) (cint 3)) in					  
  Inference.infer Ast.prim [] [] ast 

let t2 =
  let ast = lt (plus (cint 5) (cint 2)) (cint 2) in
  Inference.infer Ast.prim [] [] ast

let t3 =
  let ast = bool_and (cbool true) (cbool false) in
  Inference.infer Ast.prim [] [] ast
		  
		 
let t4 =
  let ast = bool_and (lt (plus (cint 5) (cint 3)) (cint 25)) (cbool true) in
  Inference.infer Ast.prim [] [] ast

let t5 =
  let ast = abstr [x] (Ast.Var x) in
  Inference.infer Ast.prim [] [] ast

let t6 =
  let ast = abstr [x] (plus (Ast.Var x) (Ast.Var x)) in
  Inference.infer Ast.prim [] [] ast
		  
let t7 =  	 
  let ast = abstr [x;y;z;a]
		  (bool_and (lt (plus (Ast.Var x) (Ast.Var y)) (Ast.Var z)) (Ast.Var a)) in
  Inference.infer Ast.prim [] [] ast

