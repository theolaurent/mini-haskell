type var = string

type const =
  | CUnit
  | CBool of bool
  | CInt of int
  | CChar of char
  | CPrim of string

type ('a, 'b) annot = { annot : 'a ; data : 'b }

type 'a annoted_var = ('a, var) annot

type 'a gen_ast = ('a, 'a gen_ast_s) annot
and 'a gen_ast_s =
  | Const of const
  | Var of var
  | Abstr of 'a gen_def
  | App of 'a gen_ast * 'a gen_ast
  | Let of ('a gen_def) list * 'a gen_ast
  | Spec of 'a spec
and 'a spec =
  | If of 'a gen_ast * 'a gen_ast * 'a gen_ast
  | Case of 'a gen_ast * 'a gen_ast * 'a annoted_var * 'a annoted_var * 'a gen_ast
  | Do of 'a gen_ast list * 'a gen_ast
  | Return
and 'a gen_def = 'a annoted_var * 'a gen_ast

type pos = Pos of Lexing.position * Lexing.position
(* TODO : use this type everywhere we need a position e.g. the errors module *)

type ast = (pos * [ `Unty | `Annot of unit ]) gen_ast
type def = (pos * [ `Unty | `Annot of unit ]) gen_def
type typed_ast = (pos * [ `Ty of unit ]) gen_ast
type typed_def = (pos * [ `Ty of unit ]) gen_def

let annotate pos ?ty ast =
 let t = match ty with
 | None -> `Unty
 | _ -> failwith "ICE: type annotation are not supported yet"
 in { annot = (pos, t) ; data = ast }

module Primitive = Map.Make(String);;

(* TODO : make a diffenrece between the position of the primitive keyword
   and the positions of the whole primitive application *)
let primitive pos str l =
 List.fold_left (fun res expr ->
 annotate pos (App (res, expr)))
 (annotate pos (Const (CPrim str))) l

let ast_cons pos hd tl = primitive pos "cons" [hd ; tl]
let ast_empty pos = primitive pos "empty" []
let ast_or pos x y = primitive pos "or" [x ; y]
let ast_and pos x y = primitive pos "and" [x ; y]
let ast_plus pos x y = primitive pos "plus" [x ; y]
let ast_minus pos x y = primitive pos "minus" [x ; y]
let ast_unary_minus pos n = ast_minus pos (annotate pos (Const (CInt 0))) n
let ast_mult pos x y = primitive pos "mult" [x ; y]
let ast_lt pos x y = primitive pos "lt" [x ; y]
let ast_leq pos x y = primitive pos "leq" [x ; y]
let ast_gt pos x y = primitive pos "gt" [x ; y]
let ast_geq pos x y = primitive pos "geq" [x ; y]
let ast_eq pos x y = primitive pos "eq" [x ; y]
let ast_neq pos x y = primitive pos "neq" [x ; y]

let ast_list pos l =
  (List.fold_right (fun x res -> primitive pos "cons" [ x ; res ])) l (ast_empty pos)

let ast_let pos l expr =
  (* List.fold_right (fun (x,y) e -> Let (x,y, e)) l expr *)
  annotate pos (Let (l, expr))

let ast_lambda pos l expr =
  List.fold_right (fun i res -> annotate pos (Abstr (i, res))) l expr

let ast_app pos f l = List.fold_left (fun res expr -> annotate pos (App (res, expr))) f l
