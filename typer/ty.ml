type constructor = string
		
type 'a t =
  | TConst of  constructor * ('a t) list
  | TVar of Var.t
  | TArrow of ('a t) * ('a t)
  | TBot

type ty = [`Ty | `Skeleton] t
type skeleton = [`Skeleton] t

let constructor constr types =
  TConst (constr, types)

let variable v =
  TVar v

let arrow dom codom =
  TArrow (dom, codom)

let bot () =
  TBot

let skeleton_of_ty ty =
  (ty :> skeleton)
    

let rec subst var ty = function
  | TConst (constr, types) ->
     TConst (constr, List.map (subst var ty) types)
  | TVar v when v = var ->
     ty
  | TVar v ->
     TVar v
  | TArrow (domain, codomain) ->
     TArrow (subst var ty domain, subst var ty codomain)
  | TBot ->
     TBot

let rec occur var = function
  | TConst (_, types) ->
     List.exists (occur var) types
  | TVar v ->
     var = v
  | TArrow (domain, codomain) ->
     occur var domain || occur var codomain
  | TBot ->
     false

let rec variables = function
  | TConst (constr, types) ->
     List.fold_left
	Var.Set.union Var.Set.empty (List.map variables types)
  | TVar v -> Var.Set.singleton v
  | TArrow (a, b) ->
     Var.Set.union (variables a) (variables b)
  | TBot ->
     Var.Set.empty



(*
(* TODO: prevent continous recalculation of free vars *)
       
	     


(*
letec skeleton_of_ty = function
  | TConst (constr, types) ->
     SkConst (constr, List.map skeleton_of_ty types)
  | TVar v ->
     SkVar v
  | TArrow (domain, codomain) ->
     SkArrow (skeleton_of_ty domain, skeleton_of_ty codomain)
	    
let rec subst_skeleton var sk = function
  | SkBot ->
     SkBot
  | SkConst (constr, types) ->
     SkConst (constr, List.map (subst_skeleton var sk) types)
  | SkVar v when v = var ->
     sk
  | SkVar v ->
     SkVar v
  | SkArrow (domain, codomain) ->
     SkArrow (subst_skeleton var sk domain, subst_skeleton var sk codomain)

 *)

type ty =
  | TConst of tconst * ty list
  | TVar of Var.t
  | TArrow of ty * ty

type skeleton =
  | SkBot
  | SkConst of tconst * skeleton list
  | SkVar of Var.t
  | SkArrow of skeleton * skeleton
 *)

       
       
