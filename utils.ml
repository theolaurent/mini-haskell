
let exhaust_pattern () = failwith "ICE: this should not happen"

let string_to_list str =
  let l = ref [] in
  String.iter (fun c -> l := c :: !l) str ;
  List.rev !l
