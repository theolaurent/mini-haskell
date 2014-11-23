type var = string
type constructor = string
	     
type const =
  | CUnit
  | CBool of bool
  | CInt of int
  | CChar of char
  | CPrim of string

type ast = (* on le mettra en prvate ou pas ? *) (* je vois pas trop l'interet du private, mais on peut *)
  | Const of const
  | Constructor of constructor
  | Var of var
  | Abstr of var * ast
  | App of ast * ast
  | Let of (var * ast) list * ast


module Primitive = Map.Make(String);;

type def = var * ast

let primitive str l =
  List.fold_left (fun res expr -> App (res, expr)) (Const (CPrim str)) l


let ast_if c x y = primitive "if" [ c ; x ; y ]
let ast_do a b = primitive "do" [ a ; b ]
let ast_cons hd tl = primitive "cons" [hd ; tl]
let ast_empty = primitive "empty" []
let ast_or x y = primitive "or" [x ; y]
let ast_and x y = primitive "and" [x ; y]
let ast_unary_minus n = primitive "unary_minus" [n]
let ast_plus x y =primitive "plus" [x ; y]
let ast_minus x y =primitive "minus" [x ; y]
let ast_mult x y =primitive "mult" [x ; y]
let ast_lt x y = primitive "lt" [x ; y]
let ast_leq x y = primitive "leq" [x ; y]
let ast_gt x y = primitive "gt" [x ; y]
let ast_geq x y = primitive "geq" [x ; y]
let ast_eq x y = primitive "eq" [x ; y]
let ast_neq x y = primitive "neq" [x ; y]

let ast_list l =
  (List.fold_right (fun x res -> primitive "cons" [ x ; res ])) l ast_empty

let ast_let l expr =
  (* List.fold_right (fun (x,y) e -> Let (x,y, e)) l expr *)
  Let (l, expr)

let ast_lambda l expr =
  List.fold_right (fun i res -> Abstr (i, res)) l expr

let ast_app f l = List.fold_left (fun res expr -> App (res, expr)) f l

let ast_case e x hd tl y =
  primitive "match" [e ; x ; Abstr (hd, Abstr (tl, y)) ]
