open Ast
open Schema
open Unification

type env = (Var.t * schema) list

let infer_const prim = function
  | CChar _ -> ty (Ty.constructor "char" [])
  | CInt _  -> ty (Ty.constructor "int" [])
  | CBool _ -> ty (Ty.constructor "bool" [])
  | CPrim name -> ty (Primitive.find name prim)
			    
 (* type inference *)
let rec infer prim q env = function
  | Const c -> (q, infer_const prim c)
  | Constructor c -> (q, ty (Ty.constructor c []))
  | Var x -> (q, List.assoc x env)
  | Abstr (x, a) ->
     let alpha = Var.fresh () in
     let q1 = (alpha, (BFlexible, bot)) :: q in
     let (q2, sigma) = infer prim q1 ((x,ty (Ty.variable alpha)) :: env) a in
     let beta = Var.fresh () in
     let (q3, q4) = split q2 (fst (List.split q)) in
     q3, S (List.rev_append q4 [beta, (BFlexible, sigma)],
	    STTy (Ty.arrow (Ty.variable alpha) (Ty.variable beta)))
  | App (a, b) ->
     let (q1, sigma_a) = infer prim q env a in
     let (q2, sigma_b) = infer prim q1 env b in
     let alpha_a = Var.fresh () in
     let alpha_b = Var.fresh () in
     let beta = Var.fresh () in
     let q3 = unify ((beta, (BFlexible, bot))
		     :: (alpha_b, (BFlexible, sigma_b))
 		     :: (alpha_a, (BFlexible, sigma_a))
		     :: q2)
		    (Ty.variable alpha_a)
		    (Ty.arrow (Ty.variable alpha_b) (Ty.variable beta))
     in
     let (q4, q5) = split q3 (fst (List.split q)) in
     q4, S (List.rev q5, STTy (Ty.variable beta))
  | Let (x, a1, a2) ->
     let (q1, sigma1) = infer prim q env a1 in
     infer prim q1 ((x, sigma1) :: env) a2
