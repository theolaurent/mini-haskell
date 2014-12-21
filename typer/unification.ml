open Schema

exception Failure of string list
exception UpdateFailure



(* Associated substitution of a prefix *)
let rec subst_extracted = function
  | [] -> (fun x -> x)
  | (alpha, (_, sigma)) :: q ->
     let nfsigma = normal_form sigma.value in
     match nfsigma.value with
     | S ([], STTy ty) -> (fun y -> subst_extracted q (subst alpha ty y))
     | _ -> subst_extracted q


module AbstrCheck =
struct
  (* Use polynoms with of 3 indeterminates to count types of bound in a schema :
     X <-> consecutive leading flexible bound
     Y <-> consecutive following rigid bound
     Z <-> bound following the first flex. bound following a rig. bound
     Powers count the depth of the bound ie X^2 is a leading flexible bound appearing in the schema of a leading flexible bound eg (alpha >= (.. >= ..)(beta = ..)..).., XY is a rigid bound appearing in the schema of a leading flexible bound

Polynoms are implemented by mapping a triplet (i,j,k) to the coefficient of (X^i Y^j Z^k).

The goal is to check that leading abstractions have not been modified between schema by checking that coefficient of monomial divisible by X are all zero.
   *)

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
    | S ([], STBot) ->
       let v =
	 try Polynom.find coefs poly
	 with Not_found -> 0
       in
       let poly = Polynom.remove coefs poly in
       Polynom.add coefs (modif v) poly
    | S ([], STTy _) ->
       poly
    | S ((alpha, (bound, sigma)) :: pref, t) ->
       let sigma' = forall_map (List.rev pref) (Schema.terminal t) in
       if not (is_free alpha sigma')
       then modify_polynom poly modif coefs a sigma'.value
       else if normal_form sigma.value = ty (Ty.variable alpha)
       then modify_polynom poly modif coefs a sigma.value
     else begin
	 let poly = modify_polynom poly modif coefs a sigma'.value in
	 let (c1, c2, c3) = coefs in
	 let (a1, a2, a3) = star a bound in
	 modify_polynom poly modif (c1 + a1, c2 + a2, c3 + a3) (a1, a2, a3) sigma.value
       end

  let is_monotype = function
    | S ([], STTy _) -> true
    | _ -> false


  let rec abstraction_check q sigma1 sigma2 =
    let p1 = proj (forall_map q sigma1).value in
    let p2 = proj (forall_map q sigma2).value in
    if p1 <> p2
    then false
    else begin
	let ext_sub = subst_extracted q in
	let nf1 = normal_form sigma1.value in
	let nf2 = normal_form sigma2.value in
	if is_monotype (ext_sub nf1).value
	then true
      else match (ext_sub nf2).value with
	   | S ([], STTy {Ty.value = Ty.TVar v ; _}) ->
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
	      let poly = modify_polynom Polynom.empty succ (0, 0, 0) (1, 0, 0) sigma1.value in
	      let poly = modify_polynom poly          pred (0, 0, 0) (1, 0, 0) sigma2.value in
	      Polynom.for_all (fun (cx, _, _) v -> v = 0 || cx = 0) poly
      end
end

let abstraction_check = AbstrCheck.abstraction_check


let update q (alpha, (bound, sigma)) =
  let ftv = free_variables sigma in
  let (q1, q2) = split q ftv in
  let rec find_alpha = function
    | [] -> assert false
    | (beta, (bound', sigma')) :: q2_a ->
       if beta = alpha
       then begin
	   if bound' = BRigid && not (abstraction_check q sigma' sigma)
	   then raise UpdateFailure
	   else (alpha, (bound, sigma)) :: (q2_a @ q1)
	 end
       else (beta, (bound', sigma')) :: (find_alpha q2_a)
  in
  find_alpha q2

(* Merge two variables bounded to the same schema into one binding to the schema and one between the variables *)
let merge q alpha alpha' =
  let rec find constr alpha'  = function
    | [] -> assert false
    | (beta, (bound', sigma')) :: q0 ->
       let (alpha, (bound, sigma)) = constr in
       if beta = alpha' && sigma = sigma'
       then
	 let bound'' =
	   if bound = BFlexible && bound' = BFlexible
	   then BFlexible
	   else BRigid
	 in
	 (alpha', (BRigid, ty (Ty.variable alpha)))
	 :: (alpha, (bound'', sigma)) :: q0
       else (beta, (bound', sigma')) :: (find constr alpha' q0)
  in
  let rec find2 = function
    | [] -> assert false
    | (beta, (bound, sigma)) :: q1 ->
	     if beta = alpha
	     then find (alpha, (bound, sigma)) alpha' q1
	     else if beta = alpha'
	     then find (alpha', (bound, sigma)) alpha q1
	     else (beta, (bound, sigma)) :: (find2 q1)

  in
  find2 q


let is_useful alpha q sigma =
  let rec search = function
    | [] -> assert false
    | (beta,_) :: q2 ->
       if beta = alpha
       then q2
       else search q2
  in
  let q2 = search q in
  is_free alpha (forall_map q2 sigma)

let set_of_list l =
  List.fold_left (fun e var -> Var.Set.add var e) Var.Set.empty l

let fail t1 t2 =
  Format.fprintf (Format.str_formatter) "Failed to unify %a with %a" Schema_printer.print_type t1 Schema_printer.print_type t2 ;
  raise (Failure [Format.flush_str_formatter ()])

(* unification algorithm (monotypes) *)
let rec unify q t1 t2 = match (t1.Ty.value, t2.Ty.value) with
  | (Ty.TVar a1, Ty.TVar a2) when a1 = a2 -> q
  | (Ty.TConst (g1, l1), Ty.TConst (g2, l2))
    when g1 = g2
      && List.length l1 = List.length l2 ->
    List.fold_left (fun res (t1, t2) -> unify res t1 t2) q (List.combine l1 l2)
  | (Ty.TConst _, Ty.TConst _) ->  fail t1 t2 
  | (Ty.TVar a1, Ty.TVar a2) ->
      let (b1, s1) =
	List.assoc a1 q
      in
      let n1 = normal_form s1.value in

     let (b2, s2) = List.assoc a2 q in
     let n2 = normal_form s2.value in
     begin
       match (n1.value, n2.value) with
       | (S ([], STTy {Ty.value = Ty.TVar a1 ; _}), _) -> unify q t2 (Ty.variable a1)
       | (_, S ([], STTy {Ty.value = Ty.TVar a2 ; _})) -> unify q t1 (Ty.variable a2)
       | _ ->
	  begin
	    if is_useful a1 q s2 || is_useful a2 q s1
	    then fail t1 t2 ;
	    let (q', s3) = polyunify q s1 s2 in
	    let q' = update q' (a1, (b1, s3)) in
	    let q' = update q' (a2, (b2, s3)) in
	    merge q' a1 a2
	  end
     end
  | (Ty.TVar a1, _) ->
     let (_, s1) = List.assoc a1 q in
     let n1 = normal_form s1.value in

     begin
       match n1.value with
       | S ([], STTy {Ty.value = Ty.TVar a1 ; _}) -> unify q t2 (Ty.variable a1)
       | _ ->
	  begin
	    if is_useful a1 q (ty t2)
	    then fail t1 t2 ;
 	    let (q', _) = polyunify q s1 (ty t2) in
	    update q' (a1, (BRigid, ty t2))
	  end
     end
  | (_, Ty.TVar _) -> unify q t2 t1
  | (Ty.TArrow (t11, t12), Ty.TArrow (t21, t22)) ->
     unify (unify q t11 t21) t12 t22
  | _ -> fail t1 t2 

(* unification algorithm (polytypes) *)
and polyunify q s1 s2 =
  let s1 = Schema.constructed_form s1.value in
  let s2 = Schema.constructed_form s2.value in
  match (s1.value, s2.value) with
  | (S (_, STBot), _) -> (q, s2)
  | (_, S (_, STBot)) -> (q, s1)
  | (S (_, STTy _), S (_, STTy _)) ->
     let (q_, t1) = Schema.rename q s1.value in
     let (q_, t2) = Schema.rename q_ s2.value in

     (*     let q_ = List.rev_append p2 (List.rev_append p1 q) in *)
     let q0 = unify q_ t1 t2 in
     let (q3, q4) = split q0 (set_of_list (fst (List.split q))) in
     (q3, forall_map q4 (ty t1))
