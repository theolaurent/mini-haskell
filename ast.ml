
type var = string

type const = string

type ast =
  | Const of const
  | Var of var
  | Abstr of var * ast
  | App of ast * ast
  | Let of var * ast * ast
