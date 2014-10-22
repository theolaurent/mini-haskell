open Ast

module Var =
struct
  type t =  int
  let fresh =
    let counter = ref 0 in
    fun () -> (incr counter ; !counter)
end

type tconst = string

type ty =
  | TConst of tconst * ty list
  | TVar of Var.t
  | TArrow of ty * ty


type bound =
  | BRigid
  | BFlexible

type schema =
  | SBot
  | STy of ty
  | SForall of constr * schema (* TODO : simply use a list,
                                  there is no problem for bottom,
                                  as we can say "all alpha = bottom, alpha" *)
 and constr = Var.t * (bound * schema)


type env = (Var.t * schema) list

let split q2 q =
  List.partition (fun (x,_) -> List.exists (fun (y,_) -> x = y) q) q2


let forall_map q4 sch =
  List.fold_left (fun sch constr ->
		  SForall (constr,sch)
		 ) sch q4


let rec subst_ty var ty = function
  | TConst (constr, types) ->
     TConst (constr, List.map (subst_ty var ty) types)
  | TVar v when v = var ->
     ty
  | TVar v ->
     TVar var
  | TArrow (domain, codomain) ->
     TArrow (subst_ty var ty domain, subst_ty var ty codomain)
	  
let rec subst var ty = function
  | SBot ->
     SBot
  | STy ty' ->
     STy (subst_ty var ty ty')
  | SForall ((alpha, (bound, sigma)), sigma') ->
     SForall ((alpha, (bound, subst var ty sigma)), subst var ty sigma')

let rec occur var = function
  | TConst (_, types) ->
     List.exists (occur var) types
  | TVar v ->
     var = v
  | TArrow (domain, codomain) ->
     occur var domain || occur var codomain
	     
let rec is_free var = function
  | SBot -> false
  | STy ty -> occur var ty
  | SForall ((v, (bound, sigma)), sigma') ->
     is_free var sigma || (v <> var && is_free var sigma')
			    (* is v <> var check useful ? *)
	     
let rec normal_form = function
  | SBot -> SBot
  | STy t -> STy t
  | SForall ((alpha, (bound, sigma)), sigma') ->
     let nfsigma  = normal_form sigma  in
     let nfsigma' = normal_form sigma' in
     match (nfsigma, nfsigma') with
     | (_, STy (TVar v)) when v = alpha -> nfsigma
     | (STy tau, _) -> subst alpha tau nfsigma'
     | _ when not (is_free alpha nfsigma') -> nfsigma'
     | _ -> SForall ((alpha, (bound, nfsigma)), nfsigma')
    
exception unificationfailure

(* unification algorithm (monotypes) *)
let rec unify q t1 t2 = match (t1, t2) with
  | (tvar a1, tvar a2) when a1 = a2 -> q
  | (tconst (g1, l1), tconst (g2, l2))
    when g1 = g2
      && list.length l1 = list.length g2 ->
    list.fold_left (fun res (t1, t2) -> unify res t1 t2) q (list.combine l1 l2)
  | (tconst _, tconst _) -> raise unificationfailure
  | _ -> failwith "to be continued"

(* unification algorithm (polytypes) *)
let polyunify q s1 s2 = failwith "todo"

 (* type inference *)
let rec infer q env = function
  | Const _ -> (q, sty (tconst ("cst",[])))
  | Var x -> (q, list.assoc x env)
  | Abstr (x, a) ->
     let alpha = Var.fresh () in
     let q1 = (alpha, (BFlexible, SBot)) :: q in
     let (q2, sigma) = infer q1 ((x,STy (TVar alpha)) :: gamma) in
     let beta = Var.fresh () in
     let (q3, q4) = split q2 q in
     q3, forall_map q4 (SForall ((beta, BFlexible, sigma),
				 STy (TArrow (TVar alpha, TVar beta))))
  | App (a, b) ->
     let (q1, sigma_a) = infer q1 env a in
     let (q2, sigma_b) = infer q2 env b in
     let alpha_a = Var.fresh () in
     let alpha_b = Var.fresh () in
     let beta = Var.fresh () in
     let q3 = unify ((beta, BFlexible, SBot)
		     :: (alpha_b, BFlexible, sigma_b)
 		     :: (alpha_a, BFlexible, sigma_a)
		     :: q2)
		    alpha_a alpha_b
     in
     let (q4, q5) = split q3 q in
     q4, forall_map q5 (STy (TVar beta))
  | Let (x, a1, a2) ->
     let (q1, sigma1) = infer q env a1 in
     infer q1 ((x, sigma1) :: gamma) a2
