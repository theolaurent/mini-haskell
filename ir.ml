open Ast


(* Warning : no more stack in he ir, was useless ! *)

(* invariant : the value returned by an expression of mini-haskell will always be in v0 *)

(* TODO : a nice interface, cf mips.ml *)

type variable =
  | GlobalVar of string (* on data segment *)
  | LocalVar of int     (* on stack, index starting at fp *)
  | ClosureVar of int   (* on closure, index from $a1 *)
  | ArgVar              (* argument, in $a0 *)


module VarMap = Map.Make (struct type t = var let compare = compare end)


type alloc =
  | AImm
  | ACons
  | AClos of int (* env length *)
  | AFroz

type label = L of int
type env = value list
and value =
  | Imm of int
  | Cons
  | Clos of label * variable list
  (* The variable list states where to find the variables when copying them to the environement of the closure *)
  | Froz of label * variable list

type ir =
  | Force (* force the current value i.e. the content of v0 *)
  | Label of label
  | Alloc of alloc
  | Value of value
  | Branch of label
  | BranchTrue of label
  | BranchFalse of label
  | CallFun (* no need of number arg, always one *)
  | StartCall
  | ReturnCall
  | ReturnForce
  | Push (* push v0 *)
  | Pop of int (* deallocate an given number of variables on the stack *)
  (* is it really usefull ? there will always be a Return after that (?) *)
  | Fetch of variable
  | Store of variable (* change the content of a variable to the current value ; will be usefull for recursive definitions, cf let bindings *)
  | UnPrim of unprim
  | BinPrim of binprim
  | ApplyCons
  | ApplyUncons (* push hd ; push tl *)
and unprim = [ `error | `putChar ]
and binprim = [ `div | `rem | `add | `sub | `mul | `lt
              | `leq | `gt | `geq | `eq | `neq ]



type free_vars_expr = var list gen_expr

(* TODO : don't use lists for sequences ! *)

let next_label =
  let n = ref (-1) in
  (fun () -> incr n ; L !n)

let index l x =
  let rec loop l i = match l with
  | [] -> raise Not_found
  | h :: t -> if h = x then i else loop t (i + 1)
  in loop l 0

let calc_free_vars_ast ast(*:typed_ast*) : free_vars_expr = (* TODO : the transformation from lists to set has become usefull ! *)
  let f set _ = VarSet.fold (fun x res -> x :: res) set []
  in gen_traversal f (annot_free_vars ast)

let clos_of_binop bin =
  let func1 = next_label () in
  let func2 = next_label () in
  let cont1  = next_label () in
  let cont2 = next_label () in
  [ Branch cont1 ; Label func1 ;
    StartCall ; Branch cont2 ; Label func2 ;
    StartCall ; Fetch (ClosureVar 0) ; Force ; Push ;
    Fetch ArgVar ; Force ; BinPrim bin ; ReturnCall ;
    Label cont2 ; Alloc (AClos 1) ; Value (Clos (func2, [ArgVar])) ; ReturnCall ;
    Label cont1 ; Alloc (AClos 0) ; Value (Clos (func1, [])) ]

let clos_of_unop un =
  let func = next_label () in
  let cont = next_label () in
  [ Branch cont ; Label func ;
    StartCall ; Fetch ArgVar ; Force ; UnPrim un ; ReturnCall ;
    Label cont ; Alloc (AClos 0) ; Value (Clos (func, [])) ]

let primitives =
  clos_of_binop `div @ [Store (GlobalVar "div")]
  @ clos_of_binop `rem @ [Store (GlobalVar "rem")]
  @ clos_of_unop `error @ [Store (GlobalVar "error")]
  @ clos_of_unop `putChar @ [Store (GlobalVar "putChar")]


let filter_out_globals env free_vars =
  List.filter (fun s -> VarMap.mem s env) free_vars

let closure_env _ free_vars =
  List.fold_left (fun (e, i) s -> (VarMap.add s (ClosureVar i) e, succ i))
		 (VarMap.empty, 0) free_vars
  |> fst

let const_to_ir c =
  match c with
  | CUnit -> [ Value (Imm 0) ]
  | CBool b -> [ Value (Imm (if b then 1 else 0)) ]
  | CInt i -> [ Value (Imm i) ]
  | CChar c -> [ Value (Imm (int_of_char c)) ]
  | CEmpty -> [ Value (Imm 0) ]
        (* be carefull with partial applications *)
       (* do not forget to force *)
       (* TODO : change primitives from string to a variant type in the whole compiler ; *)
(*  | CPrim _ -> (Printf.printf "Unexpected primtive value" ; exit 2) *)

let arithmetic_to_ir a =
  match a with
  | Add -> [BinPrim `add]
  | Sub -> [BinPrim `sub]
  | Mul -> [BinPrim `mul]

let comparison_to_ir c =
  match c with
  | LessThan     -> [BinPrim `lt]
  | LessEqual    -> [BinPrim `leq]
  | GreaterThan  -> [BinPrim `gt]
  | GreaterEqual -> [BinPrim `geq]
  | Equal        -> [BinPrim `eq]
  | NotEqual     -> [BinPrim `neq]

let logical_branch lcont conn =
  match conn with
  | And -> [BranchFalse lcont]
  | Or  -> [BranchTrue  lcont]

let rec binop_to_ir (env : variable VarMap.t) locals (b, e1, e2) =
  match b with
  | Arithmetic a ->
     expr_to_ir env locals e1
     @ [ Force ; Push ]
     @ expr_to_ir env (locals + 1) e2
     @ [ Force ]
     @ arithmetic_to_ir a
  | Comparison c ->
     expr_to_ir env locals e1
     @ [ Force ; Push ]
     @ expr_to_ir env (locals + 1) e2
     @ [ Force ]
     @ comparison_to_ir c
  | Logical conn ->
     let lcont = next_label () in
     (expr_to_ir env locals e1)
     @ [ Force ]
     @ logical_branch lcont conn
     @ (expr_to_ir env locals e2)
     @ [ Label lcont ]
  | Ast.Cons ->
     (froze env e1)
     @ [ Push ] (* push *)
     @ (froze env e2)
     @ [ Push ; Alloc (ACons) ; Value (Cons) ]



and expr_to_ir (env : variable VarMap.t) locals (ast:free_vars_expr) = match ast.data with
  | Const c ->
     begin
       [ Alloc (AImm) ] @ (const_to_ir c)
     end
  | Var v -> [ Fetch (try VarMap.find v env with Not_found -> GlobalVar v) ] (* the evaluation is not forced *)
  | Abstr ({ data = v ; _ }, body) ->
     let free_vars = filter_out_globals env ast.annot in
     let new_env =
       closure_env env free_vars
       |> VarMap.add v ArgVar
     in
     let lfunc = next_label () in
     let lcont = next_label () in
     let clos_env =
       List.map (fun s -> VarMap.find s env) free_vars
     in
     [ Branch lcont ; Label lfunc ; StartCall ]
     @ (expr_to_ir new_env 0 body)
     @ [ ReturnCall ; Label lcont ; Alloc (AClos (List.length clos_env)) ; Value (Clos (lfunc, clos_env)) ]
  | App (f, e) ->
     (froze env e)
     @ [ Push ]
     @ (expr_to_ir env locals f)
     @ [ Force ; CallFun ]
  | Let (binds, body) ->
     let n = List.length binds in
     let new_env =
       List.fold_left (fun (map, i) ({data = v ; _}, _) -> (VarMap.add v (LocalVar i) map, succ i)) (env, locals) binds
       |> fst
     in
     ( binds
       |> List.map (fun (_, e) -> (alloc_frozen new_env e) @ [ Push ]) (* TODO : check the indices *)
       |> List.flatten )
     @ ( binds
	 |> List.mapi (fun i (_, e) -> [Fetch (LocalVar (locals + i))] @ (store_frozen new_env e))
	 |> List.flatten)
     @ (expr_to_ir new_env (locals + n) body)
     @ [ Pop n ]
  | Spec s -> begin match s with
                    | If (cond, ontrue, onfalse) ->
                       let lfalse = next_label () in
                       let lcont = next_label () in
                       (expr_to_ir env locals cond)
                       @ [ Force ; BranchFalse lfalse ]
                       @ (expr_to_ir env locals ontrue)
                       @ [ Branch lcont ; Label lfalse ]
                       @ (expr_to_ir env locals onfalse)
                       @ [ Label lcont ]
                    | Case (l, bempty, hd, tl, bnempty) ->
		       let lempty = next_label () in
		       let lcont = next_label () in
		       (expr_to_ir env locals l)
		       @ [ Force ; BranchFalse lempty ] (* note: false and empty have the same representation *)
		       @ [ ApplyUncons ]
		       @ ( let env = env
				     |> VarMap.add hd.data (LocalVar locals)
				     |> VarMap.add tl.data (LocalVar (locals + 1))
			   in expr_to_ir env (locals + 2) bnempty )
		       @ [ Pop 2 ; Branch lcont ; Label lempty ]
		       @ (expr_to_ir env locals bempty)
		       @ [ Label lcont ]
		    | Do l  ->
		       List.fold_left (fun ir expr -> ir @ expr_to_ir env locals expr) [] l
                    | Return -> [Alloc (AImm) ; Value (Imm 0)]
              end
  | Binop (bop, e1, e2) ->
     binop_to_ir env locals (bop, e1, e2)

and alloc_frozen env e =
  match e.data with
  | Const _ ->
     [Alloc (AImm)]
  | Abstr _ ->
     let l = List.length @@ filter_out_globals env e.annot in
     [Alloc (AClos l)]
  | _ ->
     [Alloc (AFroz)]
and store_frozen env e =
  match e.data with
  | Const c ->
     const_to_ir c
  | Abstr ({ data = v ; _ }, body) ->
     let free_vars = filter_out_globals env e.annot in
     let new_env =
       closure_env env free_vars
       |> VarMap.add v ArgVar
     in
     let lfunc = next_label () in
     let lcont = next_label () in
     let clos_env =
       List.map (fun s -> VarMap.find s env) free_vars
     in
     [ Branch lcont ; Label lfunc ; StartCall ]
     @ (expr_to_ir new_env 0 body)
     @ [ ReturnCall ; Label lcont ; Value (Clos (lfunc, clos_env)) ]
  | _ ->
     let free_vars = filter_out_globals env e.annot in
     let new_env = closure_env env free_vars in
     let lfroz = next_label () in
     let lcont = next_label () in
     let froz_env = List.map (fun s -> VarMap.find s env) free_vars in (* get the indices of all free variables to build the env of the closure *)
     [ Branch lcont ; Label lfroz ; StartCall ]
     @ (expr_to_ir new_env 0 e) (* no need of a "return" ?? to see with the force function *)
     @ [ ReturnCall ; Label lcont ; Value (Froz (lfroz, froz_env)) ]
and froze env e =
  (alloc_frozen env e) @ (store_frozen env e)
