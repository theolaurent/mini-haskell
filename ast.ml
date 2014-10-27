(*
type var = string

type const = string

type ast =
  | Const of const
  | Var of var
  | Abstr of var * ast
  | App of ast * ast
  | Let of var * ast * ast
	   *)
			 
type var = string
type constructor = string
	     
type const =
| CBool of bool
| CInt of int
| CChar of char
| CPrim of string
type ast = (* on le mettra en prvate ou pas ? *) (* je vois pas trop l'interet du private, mais on peut *)
  | Const of const
  | Constructor of constructor
  | Var of var
  | Abstr of var * ast
  | App of ast * ast
  | Let of var * ast * ast


module Primitive = Map.Make(String);;

