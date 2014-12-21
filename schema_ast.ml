open Typer

exception Invalid_signature

module StrMap = Map.Make(String)

type identifier = string

type ty =
  | Identifier of identifier * ty list
  | Arrow of ty * ty

type terminal =
  | Bot
  | Ty of ty

type bound = Flexible | Rigid

type schema =
  S of binding list * terminal
and binding =
  identifier * bound * schema

let rec replaceTy b = function
  | Identifier (id, params) ->
    begin
      try
        let v = StrMap.find id b in
        if params <> []
        then raise Invalid_signature ;
        Ty.variable v
      with Not_found ->
        Ty.constructor id (List.map (replaceTy b) params)
    end
  | Arrow (x, y) ->
    Ty.arrow (replaceTy b x) (replaceTy b y)

let replaceTerminal b = function
  | Bot -> Schema.bot
  | Ty t -> Schema.ty (replaceTy b t)

let replaceBinding = function
  | Rigid -> Schema.BRigid
  | Flexible -> Schema.BFlexible

let rec bind b (S (bl, t)) =
  match bl with
  | [] -> replaceTerminal b t
  | (s, bindType, sch) :: bl ->
    let sch = bind b sch in
    let v = Var.fresh () in
    let mainSch = bind (StrMap.add s v b) (S (bl, t)) in
    Schema.forall (v, (replaceBinding bindType, sch)) mainSch

let convert sch =
  bind (StrMap.empty) sch


let generic_var v =
  (v, Flexible, S ([], Bot))
