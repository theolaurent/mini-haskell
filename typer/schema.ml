type bound =
  | BRigid
  | BFlexible

type schema_terminal =
  | STBot
  | STTy of Ty.ty
      
type schema =
    {
      free_variables : Var.Set.t ;
      value : schema_value
    }
 and schema_value = S of constr list * schema_terminal
 and constr = Var.t * (bound * schema)

			
type prefix = constr list

let bot =
  {
    free_variables = Var.Set.empty ;
    value = S ([], STBot)
  }

let ty t =
  { 
    free_variables = t.Ty.variables ;
    value = S ([], STTy t)
  }

let terminal = function
  | STBot -> bot
  | STTy t -> ty t
	    
let forall (alpha, (b, sigma)) sigma' =
  let free_variables =
    if Var.Set.mem alpha sigma'.free_variables
    then Var.Set.union
	   sigma.free_variables
	   (Var.Set.remove alpha sigma'.free_variables)
    else sigma'.free_variables
  in
  let value =
    let S (l, term) = sigma'.value in
    S ((alpha, (b, sigma)) :: l, term)
  in
  { free_variables ; value }


(* Use fold_left : first of pref depends on following bindings *)
let forall_map pref sigma =
  List.fold_left (fun sigma binding -> forall binding sigma) sigma pref
  
  (* S (List.rev_append pref l, term) *)
	     
let term_subst var ty = function
  | STBot -> STBot
  | STTy ty' -> STTy (Ty.subst var ty ty')
  
let rec subst var ty sigma =
  if Var.Set.mem var sigma.free_variables
  then
    {
      free_variables =
	Var.Set.(union ty.Ty.variables (remove var sigma.free_variables)) ;
      value =
	let S (pref, term) = sigma.value in
	let pref = List.map (fun (a, (b, s)) -> (a, (b, subst var ty s))) pref in
	let term = term_subst var ty term in
	S (pref, term)		  
    }
  else begin
      {
	free_variables = sigma.free_variables ;
	value =
	  let S (pref, term) = sigma.value in
	  let pref = List.map (fun (a, (b, s)) -> (a, (b, subst var ty s))) pref in
	  S (pref, term)
      }
    end


  (*  S (List.map (fun (a, (b, s)) -> (a, (b, subst var ty s))) l,
     term_subst var ty term) *)

let term_proj = function
  | STBot -> Ty.bot ()
  | STTy ty -> Ty.skeleton_of_ty ty

let rec proj = function
  | S ([], term) ->
     term_proj term
  | S ((alpha, (_, sigma)) :: pref, term) ->
     Ty.subst alpha (proj sigma.value) (proj (S (pref, term)))
   	      
let is_free var sigma =
  Var.Set.mem var sigma.free_variables
  (*function
  | S ([], STBot) -> false
  | S ([], STTy ty) -> Ty.occur var ty
  | S ((v, (bound, sigma)) :: pref, term) ->
     let sigma' = S (pref, term) in
     (v <> var && is_free var sigma') ||
       (is_free v sigma' && is_free var sigma)
   *)

let free_variables sigma =
  sigma.free_variables
  (*function
  | S ([], STBot) -> Var.Set.empty
  | S ([], STTy ty) -> Ty.variables ty	   
  | S ((alpha, (bound, sigma)) :: pref, term) ->
     let sigma' = S (pref, term) in
     let ftvs' = free_variables sigma' in
     if Var.Set.mem alpha ftvs'
     then Var.Set.union (free_variables sigma) (Var.Set.remove alpha (ftvs'))
     else ftvs'
   *)		       

let rec normal_form = function
  | S ([], term) -> terminal term
    | S ((alpha, (bound, sigma)) :: pref, term) ->
       let sigma' = S (pref, term) in
       let nfsigma  = normal_form sigma.value  in
       let nfsigma' = normal_form sigma' in
       match (nfsigma.value, nfsigma'.value) with
       | (_, S ([], STTy {Ty.value = Ty.TVar v ; _ })) when v = alpha -> nfsigma
       | (S ([], STTy tau), _) -> subst alpha tau nfsigma'
       | _ when not (is_free alpha nfsigma') -> nfsigma'
       | _ -> forall (alpha, (bound, nfsigma)) nfsigma'

let rec constructed_form = function
  | S ([], t) -> terminal t
  | S ((alpha, (b, sigma)) :: pref, term) ->
     let sigma' = S (pref, term) in
     let ns' = normal_form sigma' in
     match ns'.value with
     | S ([], STTy {Ty.value = Ty.TVar beta ; _}) when alpha = beta ->
	constructed_form sigma.value
     | _ -> forall (alpha, (b, sigma)) (constructed_form sigma')

let rec rename q s =
  match s with
  | S ([], STBot) -> assert false
  | S ([], STTy t) -> (q, t)
  | S ((alpha, (b, sigma)) :: pref, term) ->
     let (q, s) =
       if List.exists (fun (beta, _) -> alpha = beta) q
       then begin
	   let new_alpha = Var.fresh () in
	   let q = (new_alpha, (b, sigma)) :: q in
	   let s =
	     subst alpha (Ty.variable new_alpha)
		   (forall_map (List.rev pref) (terminal term))
	   in
	   (q, s.value)
	 end
       else ((alpha, (b, sigma)) :: q, S (pref, term))
     in
     rename q s
       
(* Split q  *)
let split q vars =
  let rec impl vars = function
    | [] -> ([],[])
    | ((a, (_, s)) as x) :: q ->
       if Var.Set.mem a vars
       then
	 let vars = Var.Set.union (Var.Set.remove a vars) (free_variables s) in 
	 let (q1, q2) = impl vars q in
	 (x :: q1, q2)
       else
	 let (q1, q2) = impl vars q in
	 (q1, x :: q2)
  in impl vars q
  

		 
