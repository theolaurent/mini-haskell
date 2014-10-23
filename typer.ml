open Ast

(* TODO: prevent continous recalculation of free vars *)
       
module Var =
struct
  type t =  int
  let fresh =
    let counter = ref 0 in
    fun () -> (incr counter ; !counter)
end

module VarSet =
  Set.Make (
      struct
	type t = Var.t
	let compare = compare
      end
    )

type tconst = string

type ty =
  | TConst of tconst * ty list
  | TVar of Var.t
  | TArrow of ty * ty

type skeleton =
  | SkBot
  | SkConst of tconst * skeleton list
  | SkVar of Var.t
  | SkArrow of skeleton * skeleton
		     
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
     TVar v
  | TArrow (domain, codomain) ->
     TArrow (subst_ty var ty domain, subst_ty var ty codomain)

let rec skeleton_of_ty = function
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


let rec proj = function
  | SBot ->
     SkBot
  | STy ty ->
     skeleton_of_ty ty
  | SForall ((alpha, (_, sigma)), sigma') ->
     subst_skeleton alpha (proj sigma) (proj sigma')
			       
let rec is_free var = function
  | SBot -> false
  | STy ty -> occur var ty
  | SForall ((v, (bound, sigma)), sigma') ->
     is_free var sigma || (v <> var && is_free var sigma')
(* is v <> var check useful ? *)

 
let rec variables = function
  | TConst (constr, types) ->
     List.fold_left
	VarSet.union VarSet.empty (List.map variables types)
  | TVar v -> VarSet.singleton v
  | TArrow (a, b) ->
     VarSet.union (variables a) (variables b)
     
let rec free_variables = function
  | SBot -> VarSet.empty
  | STy ty -> variables ty
  | SForall ((alpha, (bound, sigma)), sigma') ->
     VarSet.union (free_variables sigma) (VarSet.remove alpha (free_variables sigma'))
		       
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

let rec subst_extracted = function
  | [] -> (fun x -> x)
  | (alpha, (_, sigma)) :: q ->
     let nfsigma = normal_form sigma in
     match nfsigma with
     | STy ty -> (fun y -> subst alpha ty (subst_extracted q y))
     | _ -> subst_extracted q

			    
let star (i, j, k) bound =	
  if k <> 0
  then (i, j, k)
  else if j <> 0
  then
    if bound = BFlexible
    then (i, j-1, k+1)
    else (i, j,   k)
  else
    if bound = BRigid
    then (i-1, j+1, k)
    else (i,   j,   k)

module Polynom =
  Map.Make(
      struct
	type t = (int * int * int)
	let compare = compare
      end)
	   
let rec modify_polynom poly modif coefs a = function
  | SBot ->
       let v =
	 try Polynom.find coefs poly
	 with Not_found -> 0
       in
       let poly = Polynom.remove coefs poly in		  
       Polynom.add coefs (modif v) poly
  | STy ty ->
     poly
  | SForall ((alpha, (bound, sigma)), sigma') ->
     if not (is_free alpha sigma')
     then modify_polynom poly modif coefs a sigma'
     else if normal_form sigma = STy (TVar alpha)
     then modify_polynom poly modif coefs a sigma
     else begin
	 let poly = modify_polynom poly modif coefs a sigma' in
	 let (c1, c2, c3) = coefs in
	 let (a1, a2, a3) = star a bound in
	 modify_polynom poly modif (c1 + a1, c2 + a2, c3 + a3) (a1, a2, a3) sigma
       end

let is_monotype = function
  | STy _ -> true
  | _ -> false
	    
     
let rec abstraction_check q sigma1 sigma2 =
  let p1 = proj (forall_map q sigma1) in
  let p2 = proj (forall_map q sigma2) in
  if p1 <> p2
  then false
  else begin
      let ext_sub = subst_extracted q in
      let nf1 = normal_form sigma1 in
      let nf2 = normal_form sigma2 in
      if is_monotype (ext_sub nf1)
      then true
      else match ext_sub nf2 with
	   | STy (TVar v) ->
	      begin
		try
		  let (bound, sigma) = List.assoc v q in
		  if bound = BRigid
		  then abstraction_check q sigma1 sigma
		  else false
		with Not_found ->
		     false
	      end
	   | _ ->
	      let poly = modify_polynom Polynom.empty succ (0, 0, 0) (1, 0, 0) sigma1  in
	      let poly = modify_polynom poly          pred (0, 0, 0) (1, 0, 0) sigma2 in
	      Polynom.for_all (fun (cx, _, _) v -> v = 0 || cx = 0) poly
    end
			    
exception UnificationFailure

(* unification algorithm (monotypes) *)
let rec unify q t1 t2 = match (t1, t2) with
  | (TVar a1, TVar a2) when a1 = a2 -> q
  | (TConst (g1, l1), TConst (g2, l2))
    when g1 = g2
      && List.length l1 = List.length l2 ->
    List.fold_left (fun res (t1, t2) -> unify res t1 t2) q (List.combine l1 l2)
  | (TConst _, TConst _) -> raise UnificationFailure
  | _ -> failwith "to be continued"

(* unification algorithm (polytypes) *)
let polyunify q s1 s2 = failwith "todo"

 (* type inference *)
let rec infer q env = function
  | Const _ -> (q, STy (TConst ("cst",[])))
  | Var x -> (q, List.assoc x env)
  | Abstr (x, a) ->
     let alpha = Var.fresh () in
     let q1 = (alpha, (BFlexible, SBot)) :: q in
     let (q2, sigma) = infer q1 ((x,STy (TVar alpha)) :: env) a in
     let beta = Var.fresh () in
     let (q3, q4) = split q2 q in
     q3, forall_map q4 (SForall ((beta, (BFlexible, sigma)),
				 STy (TArrow (TVar alpha, TVar beta))))
  | App (a, b) ->
     let (q1, sigma_a) = infer q env a in
     let (q2, sigma_b) = infer q1 env b in
     let alpha_a = Var.fresh () in
     let alpha_b = Var.fresh () in
     let beta = Var.fresh () in
     let q3 = unify ((beta, (BFlexible, SBot))
		     :: (alpha_b, (BFlexible, sigma_b))
 		     :: (alpha_a, (BFlexible, sigma_a))
		     :: q2)
		    (TVar alpha_a) (TVar alpha_b)
     in
     let (q4, q5) = split q3 q in
     q4, forall_map q5 (STy (TVar beta))
  | Let (x, a1, a2) ->
     let (q1, sigma1) = infer q env a1 in
     infer q1 ((x, sigma1) :: env) a2
