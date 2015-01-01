open Ast

       
(* Warning : no more stack in he ir, was useless ! *)

(* invariant : the value returned by an expression of mini-haskell will always be in v0 *)

(* TODO : a nice interface, cf mips.ml *)

(* TODO : handle recursive definitions ([t]: i will do it) *)
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
  (* int list is index of free vars of the body, the formal parameter being in front of the list,
     usefull to build the closure *)
  (* will use a register to handle current environement *)
  | Froz of label * variable list
type ir =
  | Force (* force the current value i.e. the content of v0 *)
  | Label of label
  | Value of value
  | Branch of label
  | BranchFalse of label
  | CallFun (* no need of number arg, always one ; TODO : wich register for what ? *)
  | ReturnCall
  | ReturnForce
  (* lexical bindings *)
  | Fetch of variable
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

   
let rec expr_to_ir (env : variable VarMap.t) (ast:free_vars_ast) = match ast.data with
  | Const c -> begin match c with
                     | CUnit -> [ Value (Imm 0) ]
                     | CBool b -> [ Value (Imm (if b then 1 else 0)) ]
                     | CInt i -> [ Value (Imm i) ]
                     | CChar c -> [ Value (Imm (int_of_char c)) ]
                     | CPrim p -> failwith "TODO" (* be carefull with partial applications *)
                                                  (* do not forget to force *)
               end
  | Var v -> [ Fetch (VarMap.find v env) ] (* the evaluation is not forced *)
  | Abstr ({ data = v ; _ }, body) ->
     let new_env =
       List.fold_left (fun (e, i) s -> (VarMap.add s (ClosureVar i) e, succ i)) (VarMap.empty, 0) ast.annot
       |> fst
       |> VarMap.add v ArgVar
     in
     (* formal parameter must be at at first place *)
     let lfunc = next_label () in
     let lcont = next_label () in
     let clos_env = List.map (fun s -> VarMap.find s env) ast.annot in (* TODO : fix the problem of the formal parameter, wich is not in env... *)
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
  @ (expr_to_ir new_env e) (* no need of a "return" ?? to see with the force function *)
  @ [ ReturnForce ; Label lcont ; Value (Froz (lfroz, froz_env)) ]
