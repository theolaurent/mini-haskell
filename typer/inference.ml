open Ast
open Schema
open Unification

type env = (Var.t * schema) list
       
 (* type inference *)
let rec infer q env = function
  | Const _ -> (q, STy (Ty.constructor "cst" []))
  | Var x -> (q, List.assoc x env)
  | Abstr (x, a) ->
     let alpha = Var.fresh () in
     let q1 = (alpha, (BFlexible, SBot)) :: q in
     let (q2, sigma) = infer q1 ((x,STy (Ty.variable alpha)) :: env) a in
     let beta = Var.fresh () in
     let (q3, q4) = split q2 (fst (List.split q)) in
     q3, forall_map q4 (SForall ((beta, (BFlexible, sigma)),
				 STy (Ty.arrow (Ty.variable alpha)
					       (Ty.variable beta))))
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
		    (Ty.variable alpha_a) (Ty.variable alpha_b)
     in
     let (q4, q5) = split q3 (fst (List.split q)) in
     q4, forall_map q5 (STy (Ty.variable beta))
  | Let (x, a1, a2) ->
     let (q1, sigma1) = infer q env a1 in
     infer q1 ((x, sigma1) :: env) a2
