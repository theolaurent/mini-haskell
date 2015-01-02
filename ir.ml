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
		       
			 
type label = L of int
type env = value list
and value =
  | Imm of int
  | Cons of value * value
  | Clos of label * variable list
  (* The variable list states where to find the variables when copying them to the environement of the closure *)
  | Froz of label * variable list

type ir =
  | Force (* force the current value i.e. the content of v0 *)
  | Label of label
  | Value of value
  | Branch of label
  | BranchTrue of label
  | BranchFalse of label
  | CallFun (* no need of number arg, always one *)
  | ReturnCall
  | ReturnForce
  | Alloc of int (* allocate an given number of variables on the stack *)
  | DeAlloc of int (* deallocate an given number of variables on the stack *)
                   (* is it really usefull ? there will always be a Return after that (?) *)
  | Fetch of variable
  | Store of variable (* change the content of a variable to the current value ; will be usefull for recursive definitions, cf let bindings *)
  | BinPrim of string
  | ApplyCons
		 
type free_vars_ast = var list gen_ast

(* TODO : don't use lists for sequences ! *)

let next_label =
  let n = ref (-1) in
  (fun () -> incr n ; L !n)

let index l x =
  let rec loop l i = match l with
  | [] -> raise Not_found
  | h :: t -> if h = x then i else loop t (i + 1)
  in loop l 0

let calc_free_vars_ast (ast:typed_ast) : free_vars_ast = (* TODO : the transformation from lists to set has become usefull ! *)
  let f set _ = VarSet.fold (fun x res -> x :: res) set []
  in gen_traversal f (annot_free_vars ast)

		   
let rec expr_to_ir (env : variable VarMap.t) locals (ast:free_vars_ast) = match ast.data with
  | Const c -> begin match c with
                     | CUnit -> [ Value (Imm 0) ]
                     | CBool b -> [ Value (Imm (if b then 1 else 0)) ]
                     | CInt i -> [ Value (Imm i) ]
                     | CChar c -> [ Value (Imm (int_of_char c)) ]
                     | CPrim "empty" -> [ Value (Imm 0) ]
                          (* be carefull with partial applications *)
                          (* do not forget to force *)
					  (* TODO : change primitives from string to a variant type in the whole compiler ; *)
		     | CPrim _ -> (Printf.printf "Unexpected primtive value" ; exit 2)
               end
  | Var v -> [ Fetch (VarMap.find v env) ] (* the evaluation is not forced *)
  | Abstr ({ data = v ; _ }, body) ->
     let new_env =
       List.fold_left (fun (e, i) s -> (VarMap.add s (ClosureVar i) e, succ i)) (VarMap.empty, 0) ast.annot
       |> fst
       |> VarMap.add v ArgVar
     in
     let lfunc = next_label () in
     let lcont = next_label () in
     let clos_env = List.map (fun s -> VarMap.find s env) ast.annot in
     [ Branch lcont ; Label lfunc ]
     @ (expr_to_ir new_env 0 body)
     @ [ ReturnCall ; Label lcont ; Value (Clos (lfunc, clos_env)) ]
  | App ({ data = App ({ data = Const (CPrim "and") ; _ }, e1) ; _ }, e2) ->
     let lcont = next_label () in
     (expr_to_ir env locals e1)
     @ [ Force ; BranchFalse lcont ]
     @ (expr_to_ir env locals e2)
     @ [ Label lcont ]
  | App ({ data = App ({ data = Const (CPrim "or") ; _ }, e1) ; _ }, e2) ->
     let lcont = next_label () in
     (expr_to_ir env locals e1)
     @ [ Force ; BranchTrue lcont ]
     @ (expr_to_ir env locals e2)
     @ [ Label lcont ]
  | App ({ data = App ({ data = Const (CPrim "cons") ; _ }, e1) ; _ }, e2) ->
     (froze env e1)
     @ [ Alloc 1 ; Store (LocalVar locals) ] (* push *)
     @ (froze env e2)
     @ [ApplyCons] 
  | App ({ data = App ({ data = Const (CPrim s) ; _ }, e1) ; _ }, e2) ->
     (expr_to_ir env locals e1)
     @ [ Force ; Alloc 1 ; Store (LocalVar locals) ] (* push *)
     @ (expr_to_ir env (locals + 1) e2)
     @ [ Force ]
     @ [BinPrim s]
  | App (f, e) ->
     (froze env e)
     @ [Alloc 1 ; Store (LocalVar locals)] (* I mean push *)
     @ (expr_to_ir env locals f)
     @ [ CallFun ]
  | Let (binds, body) ->
     let n = List.length binds in
     let new_env =
       List.fold_left (fun (map, i) ({data = v}, _) -> (VarMap.add v (LocalVar i) map, succ i)) (env, locals) binds
       |> fst
     in
     [ Alloc n ]
     @ ( binds
         |> List.mapi (fun i (_, e) -> (froze new_env e) @ [ Store (LocalVar i) ]) (* TODO : check the indices *)
         |> List.flatten )
     @ (expr_to_ir new_env (locals + n) body)
     @ [ DeAlloc n ]
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
                    | Case _ -> failwith "TODO"
                    (* why not just expand case into if + lambda ? *)
                    | Do _ -> failwith "TODO"
                    | Return -> failwith "TODO"
              end
and froze env e =
  let new_env =
    List.fold_left (fun (e, i) s -> (VarMap.add s (ClosureVar i) e, succ i)) (VarMap.empty, 0) e.annot
    |> fst
  in
  let lfroz = next_label () in
  let lcont = next_label () in
  let froz_env = List.map (fun s -> VarMap.find s env) e.annot in (* get the indices of all free variables to build the env of the closure *)
  [ Branch lcont ; Label lfroz ]
  @ (expr_to_ir new_env 0 e) (* no need of a "return" ?? to see with the force function *)
  @ [ ReturnForce ; Label lcont ; Value (Froz (lfroz, froz_env)) ]
