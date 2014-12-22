open Ast

(* TODO : a nice interface, cf mips.ml *)


(* Inspired from UPMC'LI223 ; is it a good intermediate rep ?? *)
type value = unit (* TODO *)
type label = L of int
type addr = A of int
type prim =
  | Add
  | Sub
  | Mult
  | Div
type ir =
  | NoOp (* useless ? *)
  | Label of label
  | Push of value
  | Pop
  | Jump of label
  | JumpFalse of label
  | CallFun (* no need of number arg, always one *)
  | CallPrim of prim
  | Return
  (* lexical bindings *)
  | Fetch of addr
  | Store of addr
  (* Global definitions ; useless ? *)
  | GAlloc
  | GFetch of addr
  | Gstore of addr

(* TODO : don't use lists ! *)

let next_label =
  let n = ref (-1) in
  (fun () -> incr n ; L !n)

let compile_const c = match c with
  | CUnit -> failwith "TODO"
  | CBool b -> failwith "TODO"
  | CInt i -> failwith "TODO"
  | CChar c -> failwith "TODO"
  | CPrim p -> failwith "TODO"

let compile_var v = failwith "TODO"

let rec compile_expr ast = match ast.data with
  | Const c -> compile_const c
  | Var v -> compile_var v
  | Abstr _ -> failwith "TODO"
  | App _ -> failwith "TODO"
  | Let _ -> failwith "TODO"
  | Spec s -> compile_spec s
and compile_spec s = match s with
  | If (cond, ontrue, onfalse) ->
     let lfalse = next_label () in
     let lcont = next_label () in
     (compile_expr cond)
     @ [ JumpFalse lfalse ]
     @ (compile_expr ontrue)
     @ [ Jump lcont ; Label lfalse ]
     @ (compile_expr onfalse)
     @ [ Label lcont ]
  | Case _ -> failwith "TODO"
  (* why not just expand case into if ? *)
  | Do _ -> failwith "TODO"
  | Return -> failwith "TODO"
