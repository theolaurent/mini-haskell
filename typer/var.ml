type t =  int
let fresh =
  let counter = ref 0 in
  fun () -> (incr counter ; !counter)

moduleSet =
  Set.Make (
      struct
	type u = t
	type t = u
	let compare = compare
      end
    )

