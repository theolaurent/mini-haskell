type var = string

type const =
  | CUnit
  | CBool of bool
  | CInt of int
  | CChar of char
  | CEmpty
(*  | CPrim of string *)

type arithmetic_binop =
  | Add
  | Sub
  | Mul

type comparison_binop =
  | LessEqual    | LessThan        (* <= ; <  *)
  | GreaterEqual | GreaterThan     (* >= ; >  *)
  | Equal        | NotEqual        (* == ; /= *)

type logical_binop =
  | And
  | Or
	   
type binop =
  | Arithmetic of arithmetic_binop
  | Comparison of comparison_binop
  | Logical of logical_binop
  | Cons

type ('a, 'b) annot = { annot : 'a ; data : 'b }

type 'a annoted_var = ('a, var) annot
				

type 'a gen_expr = ('a, 'a gen_expr_s) annot
and 'a gen_expr_s =
  | Const of const
  | Var of var
  | Abstr of 'a gen_def
  | App of 'a gen_expr * 'a gen_expr
  | Let of ('a gen_def) list * 'a gen_expr
  | Spec of 'a spec
  | Binop of binop * 'a gen_expr * 'a gen_expr

and 'a spec =
  | If of 'a gen_expr * 'a gen_expr * 'a gen_expr
  | Case of 'a gen_expr * 'a gen_expr * 'a annoted_var * 'a annoted_var * 'a gen_expr
  | Do of 'a gen_expr list
  | Return
and 'a gen_def = 'a annoted_var * 'a gen_expr

type pos = Pos of Lexing.position * Lexing.position
let pos s e = Pos (s, e)

(* TODO : use this type everywhere we need a position e.g. the errors module *)

type expr = (pos * [ `Unty | `Annot of Typer.Schema.schema ]) gen_expr
type def = (pos * [ `Unty | `Annot of Typer.Schema.schema ]) gen_def
type typed_expr = (pos * [ `Ty of unit ]) gen_expr
type typed_def = (pos * [ `Ty of unit ]) gen_def

let annotate pos ?ty expr =
  let t = match ty with
    | None -> `Unty
    | Some t -> `Annot t
  in { annot = (pos, t) ; data = expr }
       
module Primitive = Map.Make(String);;
  

 (* A gereric way to apply function to an annotated expr *)
let gen_traversal (f_expr:'a -> 'b gen_expr_s -> 'b) (expr:'a gen_expr) : 'b gen_expr =
  let rec loop_expr expr =
    let wrap_expr x = { data = x ; annot = f_expr expr.annot x } in
    let wrap_var x = { data = x ; annot = f_expr expr.annot (Var x) } in (* quite an ugly hack *)
    let do_spec s = match s with
      | If (e1, e2, e3) -> If (loop_expr e1, loop_expr e2, loop_expr e3)
      | Case (e, nilcase, { data = vh ; _ }, { data = vt ; _ }, conscase) ->
         Case (loop_expr e, loop_expr nilcase, wrap_var vh, wrap_var vt, loop_expr conscase)
      | Do le -> Do (List.map loop_expr le)
      | Return -> Return
    in
    match expr.data with
    | (Const _ as x)
    | (Var _ as x) -> wrap_expr x
    | Abstr ({ data = v ; _ }, body) -> wrap_expr (Abstr (wrap_var v, loop_expr body))
    | App (f, e) -> wrap_expr (App (loop_expr f, loop_expr e))
    | Let (binds, body) ->
       wrap_expr (Let (List.map (fun ({ data = v ; _ }, e) -> (wrap_var v, loop_expr e)) binds,
                      loop_expr body))
    | Spec s -> wrap_expr (Spec (do_spec s))
    | Binop (op, x, y) -> wrap_expr (Binop (op, loop_expr x, loop_expr y))
  in loop_expr expr


module VarSet = Set.Make (struct type t = var let compare = Pervasives.compare end)

let annot_free_vars expr =
  let f _ expr = match expr with
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
    | Binop (_, x, y) -> VarSet.union x.annot y.annot
  in gen_traversal f expr



(* TODO : make a diffenrece between the position of the primitive keyword
   and the positions of the whole primitive application *)
let binop op pos x y : expr =
  annotate pos (Binop (op, x, y))
(* List.fold_left (fun res expr ->
 annotate pos (App (res, expr)))
 (annotate pos (Const (CPrim str))) l
 *)
let expr_empty pos = annotate pos (Const CEmpty)
let expr_cons  = binop Cons
let expr_or    = binop (Logical Or)
let expr_and   = binop (Logical And)
let expr_plus  = binop (Arithmetic Add)
let expr_minus = binop (Arithmetic Sub)
let expr_unary_minus pos n = expr_minus pos (annotate pos (Const (CInt 0))) n
let expr_mult  = binop (Arithmetic Mul)
let expr_lt    = binop (Comparison LessThan)
let expr_leq   = binop (Comparison LessEqual)
let expr_gt    = binop (Comparison GreaterThan)
let expr_geq   = binop (Comparison GreaterEqual)
let expr_eq    = binop (Comparison Equal)
let expr_neq   = binop (Comparison NotEqual)

let expr_list pos l =
  List.fold_right (expr_cons pos) l (expr_empty pos)

let expr_let pos l expr =
  (* List.fold_right (fun (x,y) e -> Let (x,y, e)) l expr *)
  annotate pos (Let (l, expr))

let expr_lambda pos l expr =
  List.fold_right (fun i res -> annotate pos (Abstr (i, res))) l expr

let expr_app pos f l = List.fold_left (fun res expr -> annotate pos (App (res, expr))) f l


let expr_if pos cond btrue bfalse =
  annotate pos (Spec (If (cond, btrue, bfalse)))

let expr_case pos list cempty hd tl cnempty =
  annotate pos (Spec (Case (list, cempty, hd, tl, cnempty)))

let expr_do pos instrs =
  annotate pos (Spec (Do instrs))

let expr_return pos =
  annotate pos (Spec (Return))
