
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

  let bind2 f res1 res2 = bind (fun a -> bind (f a) res2) res1

  let map f res = bind (fun x -> Some (f x)) res

  let map2 f res1 res2 = bind (fun a -> map (f a) res2) res1

  let map3 f res1 res2 res3 = bind (fun a -> map2 (f a) res2 res3) res1

  let map4 f res1 res2 res3 res4 =
    bind (fun a -> map3 (f a) res2 res3 res4) res1

  let map5 f res1 res2 res3 res4 res5 =
    bind (fun a -> map4 (f a) res2 res3 res4 res5) res1

  (* use fold_right ! fold_left returns the list *)
  let sequence res =
    List.fold_right (map2 (fun r acc -> r :: acc)) res (Some [])

  let mapn f l = sequence (List.map (map f) l)
end
