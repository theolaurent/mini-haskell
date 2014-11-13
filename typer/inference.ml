open Ast
open Schema
open Unification

module IdentSet = Set.Make(String)
module IdentMap = Set.Make(String)
module Graph = Graph.Make(String)
       
type env = (Var.t * schema) list

let set_of_list l =
  List.fold_left (fun e var -> Var.Set.add var e) Var.Set.empty l
		 
let infer_const prim = function
  | CUnit -> ty (Ty.constructor "unit" [])
  | CChar _ -> ty (Ty.constructor "char" [])
  | CInt _  -> ty (Ty.constructor "int" [])
  | CBool _ -> ty (Ty.constructor "bool" [])
  | CPrim name -> Primitive.find name prim

let msg_fail_app fn arg =
  Format.fprintf (Format.str_formatter) "While applying %a to %a..."
		 Printer.print_ast arg
		 Printer.print_ast fn ;
  Format.flush_str_formatter ()
  
module Make(Err:Errors.S) =

  struct
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
	 let (q3, q4) = split q2 (set_of_list (fst (List.split q))) in
	 (q3,
	  forall_map q4
		     (forall (beta, (BFlexible, sigma))    
			     (ty  (Ty.arrow (Ty.variable alpha) (Ty.variable beta)))))
      | App (a, b) ->
	 let (q1, sigma_a) = infer prim q env a in
	 let (q2, sigma_b) = infer prim q1 env b in
	 let alpha_a = Var.fresh () in
	 let alpha_b = Var.fresh () in
	 let beta = Var.fresh () in
	 let q3 =
	   try
	     unify ((beta, (BFlexible, bot))
		    :: (alpha_b, (BFlexible, sigma_b))
 		    :: (alpha_a, (BFlexible, sigma_a))
		    :: q2)
		   (Ty.variable alpha_a)
		   (Ty.arrow (Ty.variable alpha_b) (Ty.variable beta))
	   with Unification.Failure trace ->
	     raise (Unification.Failure (msg_fail_app a b :: trace))
	 in
	 let (q4, q5) = split q3 (set_of_list (fst (List.split q))) in
	 (q4, forall_map q5 (ty (Ty.variable beta)))
      | Let (l, expr) ->
	 (*    let alpha = Var.fresh () in
     let q1 = (alpha, (BFlexible, bot)) :: q in
     let (q2, sigma) = infer prim q1 ((x, ty (Ty.variable alpha)) :: env) a1 in
     let beta = Var.fresh () in
     let q3 = unify ((beta, (BFlexible, sigma)) :: q2) (Ty.variable alpha) (Ty.variable beta) in
     let (q4, q5) = split q3 (set_of_list (fst (List.split q))) in
     infer prim q4 ((x, S (List.rev q5, STTy (Ty.variable alpha))) :: env) a2 *)
	 let (q1, defs) = infer_potentially_mutually_recursive_definitions prim q env l in
	 infer prim q1 (defs @ env) expr
	       
    and infer_potentially_mutually_recursive_definitions prim q env l =
      let rec add_edges_to g bound x = function
	| Const _ | Constructor _ -> g
	| Var y ->
	   if List.exists (fun (z, _) -> z = y) env || IdentSet.mem y bound
	   then g
	   else Graph.add_edge y x g
	| Abstr (y, a) ->
	   add_edges_to g (IdentSet.add y bound) x a
	| App (a, b) ->
	   let g = add_edges_to g bound x a in
	   add_edges_to g bound x b
	| Let (l, b) ->
	   let bound = List.fold_left (fun bound (z, _) -> IdentSet.add z bound) bound l in
	   let g = List.fold_left (fun g (_, a) -> add_edges_to g bound x a) g l in
	   add_edges_to g bound x b
      in
      if List.length l = 1
      then infer_mutually_recursive_definitions prim q env l 
      else begin
	  let g =
	    List.fold_left
	      (fun g (x, d) ->
	       let g = Graph.add_vertex x g in
	       add_edges_to g (IdentSet.singleton x) x d)
	      Graph.empty l
	  in
	  let tl = Graph.topologically_sorted_components g in
	  
	  let ls = List.map (List.map (fun x -> (x, List.assoc x l))) tl in
	  List.fold_left
	    (fun (q, vars) l ->
	     let (q, nv) = infer_mutually_recursive_definitions prim q (vars @ env) l in
	     (q, nv @ vars)) (q, []) ls	
	end
	     
    and infer_mutually_recursive_definitions prim q env l =
      try
	let defVar = List.map (fun (x, _) -> (x, Var.fresh ())) l in
	let q1 = List.fold_left (fun q1 (_, a) -> (a, (BFlexible, bot)) :: q1) q defVar in
	let nEnv = List.fold_left (fun nEnv (x, a) -> (x, ty (Ty.variable a)) :: nEnv) env defVar in
	let (q2,sigmaList) =
	  List.fold_right (fun (_, d) (q2, sigmaList) ->
			   let (q2, sigma) = infer prim q2 nEnv d in
			   (q2, sigma :: sigmaList)
			  ) l (q1, [])
	in
	
	let bodyVar = List.map (fun sigma -> (Schema.normal_form sigma.value, Var.fresh ())) sigmaList in
	let q3 =
	  List.fold_left (fun q3 (sigma, beta) -> (beta, (BFlexible, sigma)) :: q3) q2 bodyVar
	in
	let q4 =
	  List.fold_left2 (fun q4 (_,alpha) (_, beta) ->
			   unify q4 (Ty.variable alpha) (Ty.variable beta)
			  ) q3  defVar bodyVar
	in
	let (q5, q6) = split q4 (set_of_list (fst (List.split q))) in
	(q5, (List.map (fun (x,alpha) -> (x, forall_map q6 (ty (Ty.variable alpha)))) defVar))
      with Unification.Failure trace ->
	List.iter (fun msg -> Err.report ("type error : " ^ msg) (Lexing.dummy_pos) (Lexing.dummy_pos)) trace ;
	let defVar = List.map (fun (x, _) ->
			       let v = Var.fresh () in
			       (x, forall (v, Schema.(BFlexible, bot)) (ty (Ty.variable v)))) l in
	(q, defVar)
  end
