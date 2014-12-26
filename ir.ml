open Ast

(* TODO : a nice interface, cf mips.ml *)


(* Inspired from UPMC'LI223 ; is it a good intermediate rep ?? *)
(* closures => not chained env ; closure = code + env *)
(* TODO : handle recursive definitions *)
type label = L of int
type env = value list
and value =
  | Imm of const   (* TODO : what about lists ?? *)
  | Clos of label * int list
  (* int list is index of free vars of the body, the formal parameter being in front of the list,
     usefull to build the closure *)
  (* will use a register to handle current environement *)
  | Froz of unit (* TODO *)
type ir =
  | Label of label
  | Push of value
  | Branch of label
  | BranchFalse of label
  | CallFun (* no need of number arg, always one *)
  | ReturnCall
  (* lexical bindings *)
  | Fetch of int


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
  | Const c -> [ Push (Imm c) ]
  | Var v -> [ Fetch (index env v) ]
  (* /\ TODO : what about primitives ? *)
  | Abstr ({ data = v}, body) ->
     let new_env = v :: ast.annot in (* formal parameter must be at a known place *)
     let lfunc = next_label () in
     let lcont = next_label () in
     let clos_env = List.map (index env) new_env in
     [ Branch lcont ; Label lfunc ]
     @ (expr_to_ir new_env body)
     @ [ ReturnCall ; Label lcont ; Push (Clos (lfunc, clos_env)) ]
  | App (f, e) ->
     (expr_to_ir env e) @ (expr_to_ir env f) @ [ CallFun ] (* TODO : et call prim ?? *)
  | Let _ -> failwith "TODO"
  | Spec s -> begin match s with
                    | If (cond, ontrue, onfalse) ->
                       let lfalse = next_label () in
                       let lcont = next_label () in
                       (expr_to_ir env cond)
                       @ [ BranchFalse lfalse ]
                       @ (expr_to_ir env ontrue)
                       @ [ Branch lcont ; Label lfalse ]
                       @ (expr_to_ir env onfalse)
                       @ [ Label lcont ]
                    | Case _ -> failwith "TODO"
                    (* why not just expand case into if ? *)
                    | Do _ -> failwith "TODO"
                    | Return -> failwith "TODO"
              end
