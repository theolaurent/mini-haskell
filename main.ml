(* Main Program *)

let usage = "usage: petitghc [options] file.hs"

let parse_only = ref false

let spec =
  [
    "--parse-only", Arg.Set parse_only, "  stop after parsing";
  ]

let file =
  let file = ref None in
  let set_file s =
    if not (Filename.check_suffix s ".hs") then
      raise (Arg.Bad "no .hs extension");
    file := Some s
  in
  Arg.parse spec set_file usage;
  match !file with Some f -> f | None -> Arg.usage spec usage; exit 1

let open_file () = (* TODO : more relevant messages *)
  try open_in file
  with Sys_error str -> ( Format.eprintf " %s@." str; exit 1 )

(*
let report (b,e) =
  let l = b.Lexing.pos_lnum in
  let fc = b.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
  let lc = e.Lexing.pos_cnum - b.Lexing.pos_bol + 1 in
  Format.eprintf "File \"%s\", line %d, characters %d-%d:\n" file l fc lc
 *)

let () =
  let c = open_file () in
  let lb = Lexing.from_channel c in
  let _ = Parser.main Lexer.token lb in
  if !parse_only then exit 0;

(*
  try (* TODO : error handling *)
    let f = Parser.file Lexer.next_token lb in
    close_in c;
    if !parse_only then exit 0;
    Interp.file f
  with
    | Lexer.Lexing_error s ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "lexical error: %s@." s;
	exit 1
    | Parser.Error ->
	report (lexeme_start_p lb, lexeme_end_p lb);
	eprintf "syntax error@.";
	exit 1
    | Interp.Error s ->
	eprintf "error: %s@." s;
	exit 1
    | e ->
	eprintf "Anomaly: %s\n@." (Printexc.to_string e);
	exit 2
 *)
