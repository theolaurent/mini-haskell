
open Ast

module TVar : sig
  type t = private int
  val fresh : unit -> t
end = struct
  type t = int
  let last = ref 0
  let fresh () =
    let () = last := 1 + !last in
    !last
end

type tvar = TVar.t
type ty = (* no tuples :( *)
  | TVar of tvar
  | TInt
  | TBool
  | TChar
  | TList of ty
  | TFun of ty * ty
  (* TODO : see do and return *)
type ty_sch = tvar list * ty

module VarSet = Set.Make (struct type t = tvar
                                 let compare = Pervasives.compare end)
(* construct types *)
let ty t = ([], t)
let all x (l, t) = (x::l, t)

let rec fun_of l b = match l with
  | [] -> Utils.exhaust_pattern ()
  | [x] -> TFun (x, b)
  | h :: t -> TFun (h, fun_of t b)

let rec subst t1 x1 t = match t with
  | TVar v -> if v = x1 then t1 else t
  | TInt | TBool | TChar -> t
  | TList t2 -> TList (subst t1 x1 t2)
  | TFun (t2, t3) -> TFun (subst t1 x1 t2, subst t1 x1 t3)

(* typed ast *)
type 'a typed_ast = 'a * ty_sch ast

let set_type (ast : 'a ast) sty : 'a typed_ast = (fst ast, (sty, snd ast))
let get_type (ast : 'a typed_ast) : ty_sch = fst (snd ast)

let infer_equs env ast =
  let stack = Stack.create () in
  let rec aux env ast = match snd ast with
    | Int _ -> set_type ast (ty TInt)
    | Bool _ -> set_type ast (ty TBool)
    | Char _ -> set_type ast (ty TChar)
    | Op _ -> failwith "TODO: Op"
    | Id _ -> failwith "TODO: Id"
    | Lambda (lx, body) ->
       let fresh_vars = List.map (fun _ -> TVar (TVar.fresh ())) lx in
       let ty_body =
         get_type (aux ((List.combine (List.map ty fresh_vars) lx) @ env)
                       body) in
       set_type ast (ty (fun_of fresh_vars ty_body))
    | _ -> failwith "TODO: rest"
  in ()
