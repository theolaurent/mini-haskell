
module type S =
  sig
    val reset : unit -> unit
    val report : string -> Pos.t -> unit
    val get_all : unit -> string list
    val has_error_occured : unit -> bool
  end

module Init (P:sig val file : string end) : S =
  struct
    let q = Queue.create ()
    let reset () = Queue.clear q
    let report str p = Queue.push (str, p) q
    let get_all () =
      Queue.fold (fun res (msg, p) ->
		  let b = Pos.start p in
		  let e = Pos.stop  p in
                  let l = b.Lexing.pos_lnum in
                  let fc = b.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
                  let lc = e.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
                  (Format.sprintf
                     "File \"%s\", line %d, characters %d-%d:\n\t\t%s"
                   P.file l fc lc msg) :: res) [] q
      |> List.rev

    let has_error_occured () =
      not (Queue.is_empty q)
  end
