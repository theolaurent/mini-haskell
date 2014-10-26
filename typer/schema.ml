type bound =
  | BRigid
  | BFlexible
      
type schema =
  | SBot
  | SForall of constr list * Ty.ty (* TODO : simply use a list,
                                  there is no problem for bottom,
                                  as we can say "all alpha = bottom, alpha" *)
 and constr = Var.t * (bound * schema)

			
type prefix = constr list
			
let forall binding = function
  | SBot -> failwith "No binding over bot"
  | SForall (l, ty) ->
     SForall (binding :: l, ty)

let forall_map pref = function
  | SBot -> failwith "No binding over bot"
  | SForall (l, ty) ->
     SForall (List.rev_append pref l, ty)
	     

let rec subst var ty = function
  | SBot ->
     SBot
  | SForall (pref, ty') ->
     SForall (
	 List.map (fun (a, (b, s)) -> (a, (b, subst var ty s))) pref,
	 Ty.subst var ty ty'
       )

let rec proj = function
  | SBot ->
     Ty.bot ()
  | SForall ([], ty) ->
     Ty.skeleton_of_ty ty
  | SForall ((alpha, (_, sigma)) :: pref, ty) ->
     Ty.subst alpha (proj sigma) (proj (SForall (pref, ty)))

let rec is_free var = function
  | SBot -> false
  | SForall ([], ty) -> Ty.occur var ty
  | SForall ((v, (bound, sigma)) :: pref, ty) ->
     let sigma' = SForall (pref, ty) in
     (v <> var && is_free var sigma') ||
       (is_free v sigma' && is_free var sigma)


let rec free_variables = function
  | SBot -> Var.Set.empty
  | SForall ([], ty) -> Ty.variables ty	   
  | SForall ((alpha, (bound, sigma)) :: pref, ty) ->
     let sigma' = SForall (pref, ty) in
     let ftvs' = free_variables sigma' in
     if Var.Set.mem alpha ftvs'
     then Var.Set.union (free_variables sigma) (Var.Set.remove alpha (ftvs'))
     else ftvs'
		       

let rec normal_form = function
  | SBot -> SBot
  | SForall ([], t) -> SForall([], t)
  | SForall ((alpha, (bound, sigma)) :: pref, ty) ->
     let sigma' = SForall (pref, ty) in
     let nfsigma  = normal_form sigma  in
     let nfsigma' = normal_form sigma' in
     match (nfsigma, nfsigma') with
     | (_, SForall ([], (Ty.TVar v))) when v = alpha -> nfsigma
     | (SForall ([], tau), _) -> subst alpha tau nfsigma'
     | _ when not (is_free alpha nfsigma') -> nfsigma'
     | _ -> forall (alpha, (bound, nfsigma)) nfsigma'

let rec constructed_form = function
  | SBot -> SBot
  | SForall ([], t) -> SForall([], t)
  | SForall ((alpha, (b, sigma)) :: pref, ty) ->
     let sigma' = SForall (pref, ty) in
     let ns' = normal_form sigma' in
     match ns' with
     | SForall([], Ty.TVar beta) when alpha = beta -> constructed_form sigma
     | _ -> forall (alpha, (b, sigma))  (constructed_form sigma')
		    
(* Split q into (q inter vars) and (q \ vars) *)
let split q vars =
  List.partition (fun (x,_) -> List.exists (fun y -> x = y) vars) q

		 
