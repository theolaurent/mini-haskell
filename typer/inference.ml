open Ast
open Schema
open Unification

module IdentSet = Set.Make(String)
module IdentMap = Set.Make(String)

module G =
  Graph.Persistent.Digraph.Concrete
    (struct
      type t = string
      let compare = compare
      let hash = Hashtbl.hash
      let equal = (=)
    end)
module Components = Graph.Components.Make(G)
       
type env = (Var.t * schema) list

let set_of_list l =
  List.fold_left (fun e var -> Var.Set.add var e) Var.Set.empty l
		 

				 
let variable_not_found x =
  Format.fprintf (Format.str_formatter) "Variable %s was not declared"
		 x ;
  Format.flush_str_formatter ()
		 
				 
let invalid_application fn arg =
  Format.fprintf (Format.str_formatter) "While applying %a to %a..."
		 Printer.print_ast arg
		 Printer.print_ast fn ;
  Format.flush_str_formatter ()

let invalid_mutually_recursive_definition defs =
  let def = List.fold_left (fun s (d, _) -> s ^ ", " ^ d.data) "" defs in
  Format.fprintf  (Format.str_formatter) "While typing definitions of %s..." def ;
  Format.flush_str_formatter ()


let invalid_condition cond =
  Format.fprintf (Format.str_formatter) "While evaluating condition %a..."
		 Printer.print_ast cond ;
  Format.flush_str_formatter ()

let invalid_branches b1 b2 =
  Format.fprintf (Format.str_formatter) "While unifying branches:\n\t%a,\n\t%a..."
		 Printer.print_ast b1
		 Printer.print_ast b2 ;
  Format.flush_str_formatter ()

let invalid_instruction instr =
  Format.fprintf (Format.str_formatter) "While typing instruction in do block:%a..."
		 Printer.print_ast instr ;
  Format.flush_str_formatter ()
			     
let infer_const prim = function
  | CUnit -> ty (Ty.constructor "unit" [])
  | CChar _ -> ty (Ty.constructor "char" [])
  | CInt _  -> ty (Ty.constructor "int" [])
  | CBool _ -> ty (Ty.constructor "bool" [])
  | CPrim name -> Primitive.find name prim

let universal_type q =
  let var = Var.fresh () in
  ((var, (BFlexible, bot)) :: q, ty (Ty.variable var))

				 
module Make(Err:Errors.S) =
  struct
    let error msgs spos epos =
      let msg = List.fold_left (fun s m -> s ^ "\n\t" ^ m) "" msgs in
      Err.report ("type error : " ^ msg) spos epos

	 
		 
    (* type inference *) 
    let rec infer prim q env expr =
      let (Pos (spos, epos), _) = expr.annot in
      match expr.data with
      | Const c -> (q, infer_const prim c)
      | Var x ->
	 begin
	   try
	     (q, List.assoc x env)
	   with Not_found ->
	     error [variable_not_found x] spos epos ;
	     universal_type q
	 end 
      | Abstr (x, a) ->
	 let alpha = Var.fresh () in
	 let q1 = (alpha, (BFlexible, bot)) :: q in
	 let (q2, sigma) =
	   infer prim q1 ((x.data ,ty (Ty.variable alpha)) :: env) a
	 in
	 let beta = Var.fresh () in
	 let (q3, q4) = split q2 (set_of_list (fst (List.split q))) in
	 (q3,
	  forall_map q4
		     (forall (beta, (BFlexible, sigma))    
			     (ty  (Ty.arrow (Ty.variable alpha) (Ty.variable beta)))))
      | App (a, b) ->
	 begin
	   let (q1, sigma_a) = infer prim q env a in
	   let (q2, sigma_b) = infer prim q1 env b in
	   let alpha_a = Var.fresh () in
	   let alpha_b = Var.fresh () in
	   let beta = Var.fresh () in
	   try
	     let q3 =
	       unify ((beta, (BFlexible, bot))
		      :: (alpha_b, (BFlexible, sigma_b))
 		      :: (alpha_a, (BFlexible, sigma_a))
		      :: q2)
		     (Ty.variable alpha_a)
		     (Ty.arrow (Ty.variable alpha_b) (Ty.variable beta))
	     in
	     let (q4, q5) = split q3 (set_of_list (fst (List.split q))) in
	     (q4, forall_map q5 (ty (Ty.variable beta)))
	   with Unification.Failure trace ->
	     error (invalid_application a b :: trace) spos epos ;
	     universal_type q
	 end 
      | Let (l, expr) ->
	 let (q1, defs) =
	   infer_potentially_mutually_recursive_definitions prim q env l spos epos
	 in
	 infer prim q1 (defs @ env) expr
      | Spec s ->
	 infer_spec prim q env spos epos s
	
    and infer_potentially_mutually_recursive_definitions prim q env l spos epos =     
      let rec add_edges_from g bound x expr =
	match expr.data with
	| Const _ -> g
	| Var y ->
	   if List.exists (fun (z, _) -> z = y) env || IdentSet.mem y bound
	   then g
	   else G.add_edge g x y
	| Abstr (y, a) ->
	   add_edges_from g (IdentSet.add y.data bound) x a
	| App (a, b) ->
	   let g = add_edges_from g bound x a in
	   add_edges_from g bound x b
	| Let (l, b) ->
	   let bound =
	     List.fold_left (fun bound (z, _) -> IdentSet.add z.data bound) bound l
	   in
	   let g = List.fold_left (fun g (_, a) -> add_edges_from g bound x a) g l in
	   add_edges_from g bound x b
	| Spec (If (a, b, c)) ->
	   let g = add_edges_from g bound x a in
	   let g = add_edges_from g bound x b in
	   add_edges_from g bound x c
	| Spec (Case (a, b, y, z, c)) ->
	   let g = add_edges_from g bound x a in
	   let g = add_edges_from g bound x b in
	   let bound =
	     bound
	     |> IdentSet.add y.data
	     |> IdentSet.add z.data
	   in
	   add_edges_from g bound x c
	| Spec (Do l) ->
	   List.fold_left (fun g a -> add_edges_from g bound x a) g l
	| Spec (Return) ->
	   g
      in
      if List.length l = 1
      then infer_mutually_recursive_definitions prim q env l spos epos
      else begin
	  let g =
	    List.fold_left
	      (fun g (x, d) ->
	       let g = G.add_vertex g x.data in
	       add_edges_from g (IdentSet.singleton x.data) x.data d)
	      G.empty l
	  in
	  let tl = Components.scc_list g in
	  let ls =
	    List.map
	      (List.map (fun x -> List.find (fun (y, _) -> x = y.data) l))
	      tl
	  in
	  List.fold_left
	    (fun (q, vars) l ->
	     let (q, nv) =
	       infer_mutually_recursive_definitions prim q (vars @ env) l spos epos
	     in
	     (q, nv @ vars)) (q, []) ls	
	end
	     
    and infer_mutually_recursive_definitions prim q env l spos epos =
      try
	let defVar = List.map (fun (x, _) -> (x, Var.fresh ())) l in
	let q1 =
	  List.fold_left (fun q1 (_, a) -> (a, (BFlexible, bot)) :: q1) q defVar
	in
	let nEnv =
	  List.fold_left (fun nEnv (x, a) -> (x.data, ty (Ty.variable a)) :: nEnv) env defVar
	in
	let (q2,sigmaList) =
	  List.fold_right (fun (_, d) (q2, sigmaList) ->
			   let (q2, sigma) = infer prim q2 nEnv d in
			   (q2, sigma :: sigmaList)
			  ) l (q1, [])
	in
	
	let bodyVar =
	  List.map (fun sigma -> (normal_form sigma.value, Var.fresh ())) sigmaList
	in
	let q3 =
	  List.fold_left (fun q3 (sigma, beta) -> (beta, (BFlexible, sigma)) :: q3) q2 bodyVar
	in
	let q4 =
	  List.fold_left2 (fun q4 (_,alpha) (_, beta) ->
			   unify q4 (Ty.variable alpha) (Ty.variable beta)
			  ) q3  defVar bodyVar
	in
	let (q5, q6) = split q4 (set_of_list (fst (List.split q))) in
	(q5, (List.map (fun (x,alpha) -> (x.data, forall_map q6 (ty (Ty.variable alpha)))) defVar))
      with Unification.Failure trace ->
	error (invalid_mutually_recursive_definition l :: trace) spos epos;
	let defVar = List.map
		       (fun (x, _) ->
			let v = Var.fresh () in
			(x.data, forall (v, (BFlexible, bot)) (ty (Ty.variable v)))) l
	in
	(q, defVar)
	  
    and infer_spec prim q env spos epos = function
      | If (cond, b1, b2) ->
	 begin
	   let (q1, sigmaCond) = infer prim q env cond in
	   let alpha = Var.fresh () in
	   let q2 =
	     try
	       unify ((alpha, (BFlexible, sigmaCond)) :: q1)
		     (Ty.variable alpha)
		     (Ty.constructor "bool" [])
	     with Unification.Failure trace ->
	       error (invalid_condition cond :: trace) spos epos ;
	       q
	   in
	   let (q3, sigma1) = infer prim q2 env b1 in
	   let (q4, sigma2) = infer prim q3 env b2 in
	   
	   let beta1 = Var.fresh () in
	   let beta2 = Var.fresh () in
	   
	   try   
	     let q5 =
	       unify ((beta2, (BFlexible, sigma2))
		    :: (beta1, (BFlexible, sigma1))
		    :: q4)
		     (Ty.variable beta1)
		     (Ty.variable beta2)
	     in
	     let (q6, q7) = split q5 (set_of_list (fst (List.split q))) in
	     (q6, forall_map q7 (ty (Ty.variable beta1)))
	   with Unification.Failure trace ->
	     error (invalid_branches b1 b2 :: trace) spos epos ;
	     universal_type q
	 end
      | Case (list, bempty, hd, tl, bnempty) ->
	 begin
	   let listType = Var.fresh () in
	   let (q1, sigmaList) = infer prim q env list in
	   let alpha = Var.fresh () in
	   let q2 =
	     try
	       unify ((alpha, (BFlexible, sigmaList))
		      :: (listType, (BFlexible, bot))
		      :: q1)
		     (Ty.variable alpha)
		     (Ty.constructor "list" [Ty.variable listType])
	     with Unification.Failure trace ->
	       error (invalid_condition list :: trace) spos epos ;
	       (listType, (BFlexible, bot)) :: q
	   in
	   
	   let (q3, sigma1) = infer prim q2 env bempty in	   
	   let (q4, sigma2) =
	     let env =
	       (hd.data, ty (Ty.variable listType))
	       :: (tl.data, ty (Ty.constructor "list" [Ty.variable listType]))
	       :: env
	     in
	     infer prim q3 env bnempty
	   in
	   
	   let beta1 = Var.fresh () in
	   let beta2 = Var.fresh () in
	   
	   try   
	     let q5 =
	       unify ((beta2, (BFlexible, sigma2))
		    :: (beta1, (BFlexible, sigma1))
		    :: q4)
		     (Ty.variable beta1)
		     (Ty.variable beta2)
	     in
	     let (q6, q7) = split q5 (set_of_list (fst (List.split q))) in
	     (q6, forall_map q7 (ty (Ty.variable beta1)))
	   with Unification.Failure trace ->
	     error (invalid_branches bempty bnempty :: trace) spos epos ;
	     universal_type q
	 end
      | Do (instrs) ->
	 let q1 =
	   List.fold_left (
	       fun q1 expr ->
	       let (q2, sigma) = infer prim q1 env expr in
	       let v = Var.fresh () in
	       try
		 unify ((v, (BFlexible, sigma)) :: q2)
		       (Ty.variable v)
		       (Ty.constructor "IO" [Ty.constructor "unit" []])
	       with Unification.Failure trace ->
		    let (Pos (spos, epos), _) = expr.annot in
		    error (invalid_instruction expr :: trace) spos epos ;
		    q2
	     ) q instrs
	 in
	 (q1, ty (Ty.constructor "IO" [Ty.constructor "unit" []]))
      | Return ->
	 (q, ty (Ty.constructor "IO" [Ty.constructor "unit" []]))
  end
