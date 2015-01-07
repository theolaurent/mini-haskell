open Ast
open Typer
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

let invalid_binop expr =
  Format.fprintf (Format.str_formatter) "While type %a"
		 Printer.print_ast expr ;
  Format.flush_str_formatter ()
			     
let invalid_mutually_recursive_definition defs =
  let def =
    List.fold_left (fun s (d, _) -> s ^ ", " ^ d.data)
      (fst (List.hd defs)).data (List.tl defs)
  in
  Format.fprintf  (Format.str_formatter) "While typing definition(s) of %s..." def ;
  Format.flush_str_formatter ()


let invalid_condition cond =
  Format.fprintf (Format.str_formatter) "While evaluating condition %a..."
    Printer.print_ast cond ;
  Format.flush_str_formatter ()

let invalid_branches b1 b2 =
  Format.fprintf (Format.str_formatter) "While unifying branches:\n\t\t\t%a,\n\t\t\t%a..."
    Printer.print_ast b1
    Printer.print_ast b2 ;
  Format.flush_str_formatter ()

let invalid_instruction instr =
  Format.fprintf (Format.str_formatter) "While typing instruction in do block:%a..."
    Printer.print_ast instr ;
  Format.flush_str_formatter ()

let infer_const = function
  | CUnit -> ty (Ty.constructor "()" [])
  | CChar _ -> ty (Ty.constructor "Char" [])
  | CInt _  -> ty (Ty.constructor "Integer" [])
  | CBool _ -> ty (Ty.constructor "Bool" [])
  | CEmpty ->
     let v = Var.fresh () in
     forall (v, (BFlexible, bot)) (ty (Ty.constructor "List" [Ty.variable v]))
(*  | Cname -> Primitive.find name *)

				 
let universal_type q =
  let var = Var.fresh () in
  ((var, (BFlexible, bot)) :: q, ty (Ty.variable var))


module Make(Err:Errors.S) =
struct
  let error msgs spos epos =
    let msg =
      List.fold_left (fun s m -> s ^ "\n\t\t" ^ m)
        (List.hd msgs) (List.tl msgs)
    in
    Err.report ("type error : " ^ msg) spos epos

  let infer_binop = function
    | Arithmetic _ -> Definitions.arithmetic
    | Comparison _ -> Definitions.comparison
    | Logical    _ -> Definitions.logical
    | Cons         -> Definitions.cons


  (* type inference *) 
  let rec infer q env expr =
    let (Pos (spos, epos), annot) = expr.annot in
    let (q, sch) =
      match expr.data with
      | Const c -> (q, infer_const c)
      | Var x ->
        begin
	  try
            (q, List.assoc x env)
          with Not_found ->
	    error [variable_not_found x] spos epos ;
	    universal_type q
        end 
      | Abstr (x, a) -> begin
        match snd x.annot with
        | `Unty ->
          let alpha = Var.fresh () in
          let q1 = (alpha, (BFlexible, bot)) :: q in
          let (q2, sigma) =
	    infer q1 ((x.data ,ty (Ty.variable alpha)) :: env) a
          in
          let beta = Var.fresh () in
          let (q3, q4) = split q2 (set_of_list (fst (List.split q))) in
          (q3,
           forall_map q4
	     (forall (beta, (BFlexible, sigma))    
	 (ty  (Ty.arrow (Ty.variable alpha) (Ty.variable beta)))))
        | `Annot s ->
          let alpha = Var.fresh () in
          let (q1, sigma) =
	    infer q ((x.data, s) :: env) a
          in
          let q2 = (alpha, (BRigid, s)) :: q1 in
          let beta = Var.fresh () in
          let (q3, q4) = split q2 (set_of_list (fst (List.split q))) in
          (q3,
           forall_map q4
	     (forall (beta, (BFlexible, sigma))    
	 (ty  (Ty.arrow (Ty.variable alpha) (Ty.variable beta)))))
         
        end
      | App (a, b) ->
        begin
	  let (q1, sigma_a) = infer q env a in
	  let (q2, sigma_b) = infer q1 env b in
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
	  infer_potentially_mutually_recursive_definitions q env l spos epos
        in
        infer q1 (defs @ env) expr
      | Spec s ->
        infer_spec q env spos epos s
      | Binop (b, x, y) ->
	 begin
	   let schema_op = infer_binop b in
	   let (q1, schema_x) = infer q  env x in
	   let (q2, schema_y) = infer q1 env y in
	   let alpha_op = Var.fresh () in
	   let alpha_x = Var.fresh () in
	   let alpha_y = Var.fresh () in
	   let beta = Var.fresh () in
	   try
	     let q3 =
	       unify ((beta, (BFlexible, bot))
		      :: (alpha_y, (BFlexible, schema_y))
		      :: (alpha_x, (BFlexible, schema_x))
		      :: (alpha_op, (BFlexible, schema_op))
		      :: q2)
		     (Ty.variable alpha_op)
		     (Ty.arrow (Ty.variable alpha_x)
			       (Ty.arrow (Ty.variable alpha_y) (Ty.variable beta)))
	     in
	     let (q4, q5) = split q3 (set_of_list (fst (List.split q))) in
	     (q4, forall_map q5 (ty (Ty.variable beta)))
	   with Unification.Failure trace ->
	     error (invalid_binop expr :: trace) spos epos ;
	     universal_type q
	 end
    in
    match annot with
    | `Unty -> (q, sch)
    | `Annot s ->
      let alpha = Var.fresh () in
      let beta = Var.fresh () in
      let q' =
	unify ((beta, (BFlexible, sch)) :: (alpha, (BRigid, s)) :: q) (Ty.variable alpha) (Ty.variable beta) in
      (q', s)
      (*      (ignore  polyunify q sch s ; (q, s)) *)
    
    and infer_potentially_mutually_recursive_definitions q env l spos epos =     
      let rec add_edges_from bound x expr (env, g) =
        match expr.data with
        | Const _ -> (env, g)
        | Var y ->
	   if IdentSet.mem y bound
	   then (env, g)
	   else if List.exists (fun (z, _) -> z.data = y) l
	   then (env, G.add_edge g x y)
	   else if List.exists (fun (z, _) -> z = y) env
	   then (env, g)
	   else (* Ident is not *)
	     begin
	       let (Pos (spos, epos)) = fst expr.annot in
	       let msg = "Unbound identifier " ^ y in
	       Err.report msg spos epos ;
	       let v = Var.fresh () in
	       ((y, (forall (v, (BFlexible, bot)) (ty (Ty.variable v)))) :: env, g)
	     end
        | Abstr (y, a) ->
	  add_edges_from (IdentSet.add y.data bound) x a (env, g)
        | App (a, b) ->
	   (env, g)
	   |> add_edges_from bound x a
	   |> add_edges_from bound x b
        | Let (l, b) ->
	  let bound =
	    List.fold_left (fun bound (z, _) -> IdentSet.add z.data bound) bound l
	  in
	  List.fold_left
	    (fun (env, g) (_, a) -> add_edges_from bound x a (env, g)) (env, g) l
	  |> add_edges_from bound x b
        | Spec (If (a, b, c)) ->
	   (env, g)
	   |> add_edges_from bound x a
	   |> add_edges_from bound x b
	   |> add_edges_from bound x c
        | Spec (Case (a, b, y, z, c)) ->
	  let bound2 =
	    bound
	    |> IdentSet.add y.data
	    |> IdentSet.add z.data
	  in
	   (env, g)
	   |> add_edges_from bound x a
	   |> add_edges_from bound x b
	   |> add_edges_from bound2 x c
        | Spec (Do l) ->
	   List.fold_left
	     (fun (env, g) a -> add_edges_from bound x a (env, g)) (env, g) l
        | Spec (Return) ->
	   (env, g)
	| Binop (_, a, b) ->
	   (env, g)
	   |> add_edges_from bound x a
	   |> add_edges_from bound x b
      in
      if List.length l = 1
      then infer_mutually_recursive_definitions q env l spos epos
      else begin
        let (env, g) =
	  List.fold_left
	    (fun (env, g) (x, d) ->
	       let g = G.add_vertex g x.data in
	       add_edges_from (IdentSet.singleton x.data) x.data d (env, g))
	    (env, G.empty) l
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
	       infer_mutually_recursive_definitions q (vars @ env) l spos epos
	     in
	     (q, nv @ vars)) (q, []) ls	
      end

    and infer_mutually_recursive_definitions q env l spos epos =
      try
        let defVar = List.map (fun (x, _) -> (x, Var.fresh ())) l in
        let q1 = List.fold_left (fun q1 (x, a) ->
            let sch =
              match snd x.annot with
              | `Unty -> bot
              | `Annot sch -> sch
            in
            (a, (BFlexible, sch)) :: q1) q defVar
        in
        let nEnv =
	  List.fold_left (fun nEnv (x, a) -> (x.data, ty (Ty.variable a)) :: nEnv) env defVar
        in
        let (q2,sigmaList) =
	  List.fold_right (fun (_, d) (q2, sigmaList) ->
	      let (q2, sigma) = infer q2 nEnv d in
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

    and infer_spec q env spos epos = function
      | If (cond, b1, b2) ->
        begin
	  let (q1, sigmaCond) = infer q env cond in
          let alpha = Var.fresh () in
          let q2 =
            try
	      unify ((alpha, (BFlexible, sigmaCond)) :: q1)
	        (Ty.variable alpha)
	        (Ty.constructor "Bool" [])
	    with Unification.Failure trace ->
	      error (invalid_condition cond :: trace) spos epos ;
	      q
	  in
	  let (q3, sigma1) = infer q2 env b1 in
	  let (q4, sigma2) = infer q3 env b2 in

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
	  let (q1, sigmaList) = infer q env list in
	  let alpha = Var.fresh () in
	  let q2 =
	    try
	      unify ((alpha, (BFlexible, sigmaList))
		     :: (listType, (BFlexible, bot))
		     :: q1)
	        (Ty.variable alpha)
	        (Ty.constructor "List" [Ty.variable listType])
	    with Unification.Failure trace ->
	      error (invalid_condition list :: trace) spos epos ;
	      (listType, (BFlexible, bot)) :: q
	  in

	  let (q3, sigma1) = infer q2 env bempty in	   
	  let (q4, sigma2) =
	    let env =
	      (hd.data, ty (Ty.variable listType))
	      :: (tl.data, ty (Ty.constructor "List" [Ty.variable listType]))
	      :: env
	    in
	    infer q3 env bnempty
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
	      let (q2, sigma) = infer q1 env expr in
	      let v = Var.fresh () in
	      try
	        unify ((v, (BFlexible, sigma)) :: q2)
		  (Ty.variable v)
		  (Ty.constructor "IO" [Ty.constructor "()" []])
	      with Unification.Failure trace ->
	        let (Pos (spos, epos), _) = expr.annot in
	        error (invalid_instruction expr :: trace) spos epos ;
	        q2
	  ) q instrs
        in
        (q1, ty (Ty.constructor "IO" [Ty.constructor "()" []]))
      | Return ->
        (q, ty (Ty.constructor "IO" [Ty.constructor "()" []]))
end
