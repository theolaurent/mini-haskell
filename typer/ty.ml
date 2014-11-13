type constructor = string

		     
type 'a t =
    {
      variables: Var.Set.t ;
      value: 'a u
    }
and 'a u =
  | TConst of  constructor * ('a t) list
  | TVar of Var.t
  | TArrow of ('a t) * ('a t)
  | TBot 

type ty = [`Ty | `Skeleton] t
type skeleton = [`Skeleton] t
type skeleton_value = [`Skeleton] u
			    
let constructor constr types =
  {
    variables = List.fold_left
		  (fun s t -> Var.Set.union s t.variables)
		  Var.Set.empty types ;
    value = TConst (constr, types)
  }

let variable v =
  {
    variables = Var.Set.singleton v;
    value = TVar v
  }

let arrow dom codom =
  {
    variables = Var.Set.union dom.variables codom.variables ;
    value = TArrow (dom, codom)
  }

let bot () =
  {
    variables = Var.Set.empty ;
    value = TBot
  }

let skeleton_of_ty ty =
  {
    variables = ty.variables ;
    value = (ty.value :> skeleton_value)
  }

let rec subst var ty t =
  if Var.Set.mem var t.variables
  then begin
      match t.value with
      | TConst (constr, types) ->
	 let types = List.map (subst var ty) types in
	 constructor constr types
      | TVar v when v = var ->
	 ty
      | TVar v ->
	 variable v
      | TArrow (domain, codomain) ->
	 arrow (subst var ty domain) (subst var ty codomain)
      | TBot ->
	 bot ()
    end
  else t
	 
let occur var t =
  Var.Set.mem var t.variables
  
let variables t =
  t.variables
(*
  function
  | TConst (constr, types) ->
     List.fold_left
	Var.Set.union Var.Set.empty (List.map variables types)
  | TVar v -> Var.Set.singleton v
  | TArrow (a, b) ->
     Var.Set.union (variables a) (variables b)
  | TBot ->
     Var.Set.empty
 *)


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
  | SkArrow of skeleton 
 *)
