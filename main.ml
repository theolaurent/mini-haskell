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

let open_f file = (* TODO : more relevant messages *)
  try open_in file
  with Sys_error str -> ( Format.eprintf " %s@." str; exit 1 )

let () =
  let c = open_f file in
  let lb = Lexing.from_channel c in
  let module Err = Errors.Init (struct let file = file end) in
  let module Lex = Lexer.Make (Err) in
  let module Par = Parser.Make (Err) in
  let _ = match Par.main Lex.token lb with
    | Some x -> x
    | None -> begin
              List.iter print_endline (Err.get_all ()) ;
              exit 1 ;
            end
  in
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
