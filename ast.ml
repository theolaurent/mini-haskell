
type var = string

type ast =
  | Const of string
  | Var of var
  | Abstr of var * ast
  | App of ast * ast
  | Let of var * ast * ast
