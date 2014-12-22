
let id x = x

let exhaust_pattern () = failwith "ICE: this should not happen"

let string_to_list str =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) str ;
  List.rev !l

module OptionM = struct
  type 'a t = 'a option

  let bind f res = match res with
    | None -> None
    | Some x -> f x

  let return x = Some x

  let bind2 f res1 res2 = bind (fun a -> bind (f a) res2) res1

  let map f res = bind (fun x -> Some (f x)) res

  let map2 f res1 res2 = bind (fun a -> map (f a) res2) res1

  let map3 f res1 res2 res3 = bind (fun a -> map2 (f a) res2 res3) res1

  let map4 f res1 res2 res3 res4 =
    bind (fun a -> map3 (f a) res2 res3 res4) res1

  let map5 f res1 res2 res3 res4 res5 =
    bind (fun a -> map4 (f a) res2 res3 res4 res5) res1

  let sequence res =
    List.fold_right (map2 (fun r acc -> r :: acc)) res (Some [])

  let mapn f l = sequence (List.map (map f) l)

  let iter f v = match v with
    | None -> ()
    | Some x -> f x
end


module ResultM = struct
  type ('a, 'b) t =
    | Ok of 'a
    | Err of 'b list

  let bind f res = match res with
    | Ok x -> f x
    | Err l -> Err l

  let return x = Ok x

  let bind2 f res1 res2 = match (res1, res2) with
    | (Ok x, Ok y) -> f x y
    | (Err l1, Err l2) -> Err (l1 @ l2)
    | (Ok _, Err l) | (Err l, Ok _) -> Err l

  let map f res = bind (fun x -> Ok (f x)) res

  let map2 f res1 res2 = bind2 (fun x y -> Ok (f x y)) res1 res2

  let map3 f res1 res2 res3 = map2 (@@) (map2 f res1 res2) res3

  let map4 f res1 res2 res3 res4 =
    map2 (@@) (map3 f res1 res2 res3) res4

  let map5 f res1 res2 res3 res4 res5 =
    map2 (@@) (map4 f res1 res2 res3 res4) res5

  let sequence res =
    List.fold_right (map2 (fun r acc -> r :: acc)) res (Ok [])

  let mapn f l = sequence (List.map (map f) l)

  let new_err err = Err [ err ]
end
