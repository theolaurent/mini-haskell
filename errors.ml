
module type S = sig
    val reset : unit -> unit
    val report : string -> Lexing.position -> Lexing.position -> unit
    val get_all : unit -> string list
end

module Init (P:sig val file : string end) : S = struct
  let q = Queue.create ()
  let reset () = Queue.clear q
  let report str p1 p2 = Queue.push (str, p1, p2) q
  let get_all () =
    Queue.fold (fun res (msg, b, e) ->
                let l = b.Lexing.pos_lnum in
                let fc = b.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
                let lc = e.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
                (Format.sprintf
                   "File \"%s\", line %d, characters %d-%d:\n\t\t%s"
                   P.file l fc lc msg) :: res) [] q
    |> List.rev
end
    
  
(* (msg,startpos,endpos) *)

(*
let report (b,e) =
 *)
