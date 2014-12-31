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
  | Do of 'a gen_ast list
  | Return
and 'a gen_def = 'a annoted_var * 'a gen_ast

type pos = Pos of Lexing.position * Lexing.position
let pos s e = Pos (s, e)

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


 (* A gereric way to apply function to an annotated ast *)
let gen_traversal (f_ast:'a -> 'b gen_ast_s -> 'b) (ast:'a gen_ast) : 'b gen_ast =
  let rec loop_ast ast =
    let wrap_ast x = { data = x ; annot = f_ast ast.annot x } in
    let wrap_var x = { data = x ; annot = f_ast ast.annot (Var x) } in (* quite an ugly hack *)
    let do_spec s = match s with
      | If (e1, e2, e3) -> If (loop_ast e1, loop_ast e2, loop_ast e3)
      | Case (e, nilcase, { data = vh ; _ }, { data = vt ; _ }, conscase) ->
         Case (loop_ast e, loop_ast nilcase, wrap_var vh, wrap_var vt, loop_ast conscase)
      | Do le -> Do (List.map loop_ast le)
      | Return -> Return
    in
    match ast.data with
    | (Const _ as x)
    | (Var _ as x) -> wrap_ast x
    | Abstr ({ data = v ; _ }, body) -> wrap_ast (Abstr (wrap_var v, loop_ast body))
    | App (f, e) -> wrap_ast (App (loop_ast f, loop_ast e))
    | Let (binds, body) ->
       wrap_ast (Let (List.map (fun ({ data = v ; _ }, e) -> (wrap_var v, loop_ast e)) binds,
                      loop_ast body))
    | Spec s -> wrap_ast (Spec (do_spec s))
  in loop_ast ast


module VarSet = Set.Make (struct type t = var let compare = Pervasives.compare end)

let annot_free_vars ast =
  let f _ ast = match ast with
    | Const _ -> VarSet.empty
    | Var v -> VarSet.singleton v
    | Abstr ({ data = v ; _ }, body) -> VarSet.remove v body.annot
    | App (f, e) -> VarSet.union f.annot e.annot
    | Let (binds, body) -> (* Definitions are recursive by default *)
       let allfvars = List.fold_left (fun res (_, e) -> VarSet.union res e.annot) body.annot binds in
       let boundvars = List.fold_left (fun res (v, _) -> VarSet.union res (VarSet.singleton v.data)) VarSet.empty binds in
       VarSet.diff allfvars boundvars
    | Spec s -> begin match s with
                      | If (e1, e2, e3) -> VarSet.union (VarSet.union e1.annot e2.annot) e3.annot
                      | Case (e, nilcase, vh, vt, conscase) ->
                         VarSet.union (VarSet.union e.annot nilcase.annot)
                                      (VarSet.remove vh.data (VarSet.remove vt.data conscase.annot))
                      | Do le -> List.fold_left (fun res e -> VarSet.union res e.annot) VarSet.empty le
                      | Return -> VarSet.empty
                end
  in gen_traversal f ast


module Primitive = Map.Make(String)

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


let ast_if pos cond btrue bfalse =
  annotate pos (Spec (If (cond, btrue, bfalse)))

let ast_case pos list cempty hd tl cnempty =
  annotate pos (Spec (Case (list, cempty, hd, tl, cnempty)))

let ast_do pos instrs =
  annotate pos (Spec (Do instrs))

let ast_return pos =
  annotate pos (Spec (Return))
