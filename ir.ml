open Ast
       
(* Warning : no more stack in he ir, was useless ! *)

(* invariant : the value returned by an expression of mini-haskell will always be in v0 *)

(* TODO : a nice interface, cf mips.ml *)

(* TODO : handle recursive definitions ([t]: i will do it) *)
type label = L of int
type env = value list
and value =
  | Imm of int
  | Cons of value * value
  | Clos of label * int list
  (* int list is index of free vars of the body, the formal parameter being in front of the list,
     usefull to build the closure *)
  (* will use a register to handle current environement *)
  | Froz of label * int list
type ir =
  | Force (* force the current value i.e. the content of v0 *)
  | Label of label
  | Value of value
  | Branch of label
  | BranchFalse of label
  | CallFun
  (* no need of number arg, always one ; 
     TODO : wich register for what ? 
      * We need to push the argument on the stack (because evaluating the closure is an arbirtrary operation, so it may requires to call other functions and there is no way to only use registers). I think it should push v0 on the top of the stack.
      * The closure is the last thing evaluated before the call, so it should be in v0. Call fun will move it to a1.
  *)
  | ReturnCall
  | ReturnForce (* Not needed *)
  (* lexical bindings *)
  | Fetch of int
  | Store of int (* will be usefull for recursive definitions *)


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

let calc_free_vars_ast (ast:typed_ast) : free_vars_ast =
  let f set _ = VarSet.fold (fun x res -> x :: res) set []
  in gen_traversal f (annot_free_vars ast)


let rec expr_to_ir env (ast:free_vars_ast) = match ast.data with
  | Const c -> begin match c with
                     | CUnit -> [ Value (Imm 0) ]
                     | CBool b -> [ Value (Imm (if b then 1 else 0)) ]
                     | CInt i -> [ Value (Imm i) ]
                     | CChar c -> [ Value (Imm (int_of_char c)) ]
                     | CPrim p -> failwith "TODO" (* be carefull with partial applications *)
                                                  (* do not forget to force *)
               end
  | Var v -> [ Fetch (index env v) ] (* the evaluation is not forced *)
  | Abstr ({ data = v}, body) ->
     let new_env = v :: ast.annot in (* formal parameter must be at at first place *)
     let lfunc = next_label () in
     let lcont = next_label () in
     let clos_env = List.map (index env) new_env in (* TODO : fix the problem of the formal parameter, wich is not in env... *)
     [ Branch lcont ; Label lfunc ]
     @ (expr_to_ir new_env body)
     @ [ ReturnCall ; Label lcont ; Value (Clos (lfunc, clos_env)) ]
  | App (f, e) ->
     (froze env e) @ (expr_to_ir env f) @ [ CallFun ] (* TODO : and call prim ?? *)
  | Let _ -> failwith "TODO" (* do not forget to froze *)
  | Spec s -> begin match s with
                    | If (cond, ontrue, onfalse) ->
                       let lfalse = next_label () in
                       let lcont = next_label () in
                       (expr_to_ir env cond)
                       @ [ Force ; BranchFalse lfalse ]
                       @ (expr_to_ir env ontrue)
                       @ [ Branch lcont ; Label lfalse ]
                       @ (expr_to_ir env onfalse)
                       @ [ Label lcont ]
                    | Case _ -> failwith "TODO"
                    (* why not just expand case into if ? *)
		    (* No, it has to load variables *)
                    | Do _ -> failwith "TODO"
                    | Return -> failwith "TODO"
		    (* I think it should just load unit in v0 : 
                       something like Value (Imm 0) *)
              end
and froze env e =
  let new_env = e.annot in
  let lfroz = next_label () in
  let lcont = next_label () in
  let froz_env = List.map (index env) new_env in (* get the indices of all free variables to build the env of the closure *)
  [ Branch lcont ; Label lfroz ]
  @ (expr_to_ir new_env e) (* no need of a "return" ?? to see with the force function *)
  @ [ ReturnForce ; Label lcont ; Value (Froz (lfroz, froz_env)) ]
