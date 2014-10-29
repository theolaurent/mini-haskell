type t =  int
let fresh =
  let counter = ref 0 in
  fun () -> (incr counter ; !counter)

let to_string v =
  let k = (v - 1) mod 26 in
  let n = (v - 1) / 26 in
  let pref = String.make 1 (Char.chr (Char.code 'a' + k)) in
  let suff = if n = 0 then "" else string_of_int n in
  pref ^ suff
	      
module Set =
  Set.Make (struct
	type u = t
	type t = u
	let compare = compare
      end
    )

