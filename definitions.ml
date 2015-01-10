open Typer

let ty = Schema.ty
let var x = Ty.variable x
let forall = Schema.forall
let forall_map = Schema.forall_map

(* @ is used to have right associativity *)
let (@->) = Ty.arrow
let (??) v = (v, Schema.(BFlexible, bot))
let (>=?) v b = (v, (Schema.BFlexible, b))
let (=?) v b = (v, (Schema.BRigid, b))
let (!!) t = Ty.constructor t []
let list t = Ty.constructor "List" [t]
let io t = Ty.constructor "IO" [t]


let arithmetic = ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer"
let comparison = ty @@ !!"Integer" @-> !!"Integer" @-> !!"Bool"
let logical = ty @@ !!"Bool" @-> !!"Bool" @-> !!"Bool"
let cons =
  let v = Var.fresh () in
  let tv = var v in
  forall ??v (ty @@ tv @-> list tv @-> list tv)

let env =
  [
    ("putChar", ty @@ !!"Char" @-> io !!"()") ;
    ("rem", ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer") ;
    ("div", ty @@ !!"Integer" @-> !!"Integer" @-> !!"Integer") ;
    ("error",
     let v = Var.fresh () in
     forall ??v (Schema.ty @@ list !!"Char" @-> var v))
  ]
