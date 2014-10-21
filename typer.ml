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


type env = (var * schema) list

let split q2 q =
  List.split (fun (x,_) -> List.filter (fun (y,_) -> x = y) q) q2


let forall_map q4 sch =
  List.fold_left (fun sch constr ->
		  SForall (constr,sch)
		 ) sch q4

exception UnificationFailure

(* unification algorithm (monotypes) *)
let rec unify q t1 t2 = match (t1, t2) with
  | (TVar a1, Tvar a2) when a1 = a2 -> q
  | (TConst (g1, l1), TConst (g2, l2))
    when g1 = g2
      && List.length l1 = List.length g2 ->
    List.fold_left (fun res (t1, t2) -> unify res t1 t2) q (List.combine l1 l2)
  | (TConst _, TConst _) -> raise UnificationFailure
  | _ -> failwith "TO BE CONTINUED"

(* unification algorithm (polytypes) *)
let polyunify q s1 s2 = failwith "TODO"

 (* type inference *)
 let rec infer q env = function
  | Const _ -> (q, STy (TConst ("cst",[])))
  | Var x -> (q, List.assoc x env)
  | Abstr (x, a) ->
     let alpha = Var.fresh () in
     let (q1, sigma) = (alpha, (BFlexible, SBot)) :: q in
     let q2 = infer q1 ((x,STy (TVar alpha)) :: gamma) in
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
