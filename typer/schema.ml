type bound =
  | BRigid
  | BFlexible
      
type schema =
  | SBot
  | STy of Ty.ty
  | SForall of constr * schema (* TODO : simply use a list,
                                  there is no problem for bottom,
                                  as we can say "all alpha = bottom, alpha" *)
 and constr = Var.t * (bound * schema)

			

type env = (Var.t * schema) list

let forall_map q sch =
  List.fold_left (fun sch constr ->
		  SForall (constr,sch)
		 ) sch q

let rec subst var ty = function
  | SBot ->
     SBot
  | STy ty' ->
     STy (Ty.subst var ty ty')
  | SForall ((alpha, (bound, sigma)), sigma') ->
     SForall ((alpha, (bound, subst var ty sigma)), subst var ty sigma')

let rec proj = function
  | SBot ->
     Ty.bot ()
  | STy ty ->
     Ty.skeleton_of_ty ty
  | SForall ((alpha, (_, sigma)), sigma') ->
     Ty.subst alpha (proj sigma) (proj sigma')

let rec is_free var = function
  | SBot -> false
  | STy ty -> Ty.occur var ty
  | SForall ((v, (bound, sigma)), sigma') ->
     is_free var sigma || (v <> var && is_free var sigma')
(* is v <> var check useful ? *)


let rec free_variables = function
  | SBot -> Var.Set.empty
  | STy ty -> Ty.variables ty
  | SForall ((alpha, (bound, sigma)), sigma') ->
     Var.Set.union (free_variables sigma) (Var.Set.remove alpha (free_variables sigma'))
		       

let rec normal_form = function
  | SBot -> SBot
  | STy t -> STy t
  | SForall ((alpha, (bound, sigma)), sigma') ->
     let nfsigma  = normal_form sigma  in
     let nfsigma' = normal_form sigma' in
     match (nfsigma, nfsigma') with
     | (_, STy (Ty.TVar v)) when v = alpha -> nfsigma
     | (STy tau, _) -> subst alpha tau nfsigma'
     | _ when not (is_free alpha nfsigma') -> nfsigma'
     | _ -> SForall ((alpha, (bound, nfsigma)), nfsigma')
