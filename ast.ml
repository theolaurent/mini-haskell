
module Id : sig
  type t = private string
  val of_str : string -> t
end = struct
  type t = string
  let of_str x = x
end

type id = Id.t

type op =
  | Plus
  | Minus
  | Mult
  | Leq
  | Geq
  | Le
  | Ge
  | Neq
  | Eq
  | And
  | Or
  | Cons

type 'a ast = 'a * 'a s_ast
and 'a s_ast =
  (* constants *)
  | Int of int
  | Bool of bool
  | Char of char
  | Op of op
  (* idents *)
  | Id of id
  (* expr *)
  | Lambda  of id list * 'a ast (* the id list is not supposed to be empty *)
  | If of 'a ast * 'a ast * 'a ast
  | Let of id * 'a ast
  | Do of 'a ast list (* the ast list is not supposed to be emty *)
  | Return
