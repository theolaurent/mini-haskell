(* Main Program *)
open Typer

let usage = "usage: petitghc [options] file.hs"

let parse_only = ref false
let type_only = ref false
let print_ast = ref false
let print_type = ref false
		    
let spec =
  [
    "--parse-only", Arg.Set parse_only, "stop after parsing";
    "--type-only", Arg.Set type_only, "stop after typing" ;
    "--print-ast", Arg.Set print_ast, "output the internal representation" ;
    "--print-type", Arg.Set print_type, "output type of toplevel identifiers"
  ]

let prim = Definitions.prim
let env = Definitions.env

    

    
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
  let defs = Par.main Lex.token lb in
  if Err.has_error_occured ()
  then 
    begin
      List.iter print_endline (Err.get_all ()) ;
      exit 1 ;
    end ;

  if !print_ast
  then Printer.print_def_list Format.std_formatter defs ;
  
  if !parse_only then exit 0 ;
  
  let (_, defsType) = Inference.infer_potentially_mutually_recursive_definitions prim [] env defs in 
  if !print_type
  then begin
      List.iter
	(fun (x,s) ->
	 Format.fprintf Format.std_formatter "%s :: %a\n"
   			x Printer.print_schema (Schema.normal_form s.Schema.value)
	) defsType
    end

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
