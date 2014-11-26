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

module CheckMain(Err:Errors.S) =
  struct
    let check defs =
      try
	let sch = List.assoc "main" defs in
	let alpha = Var.fresh () in
	ignore (Unification.unify [(alpha, (Schema.BFlexible, sch))] (Ty.variable alpha) (Ty.constructor "IO" [Ty.constructor "unit" []]))
      with
      | Not_found -> Err.report "type error : main is not present" (Lexing.dummy_pos) (Lexing.dummy_pos)
      | Unification.Failure trace ->
	 Err.report "type error : While typing main..." (Lexing.dummy_pos) (Lexing.dummy_pos) ; 
	 List.iter (fun msg -> Err.report ("type error : " ^ msg) (Lexing.dummy_pos) (Lexing.dummy_pos)) trace
  end

    
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
  let module SyntaxErr = Errors.Init (struct let file = file end) in
  let module TypeErr = Errors.Init (struct let file = file end) in
  let module Lex = Lexer.Make (SyntaxErr) (TypeErr) in
  let module Par = Parser.Make (SyntaxErr) (TypeErr) in
  let defs = Par.main Lex.token lb in

  if SyntaxErr.has_error_occured ()
  then 
    begin
      List.iter print_endline (SyntaxErr.get_all ()) ;
      exit 1 ;
    end ;

  if !print_ast
  then Printer.print_def_list Format.std_formatter defs ;
  
  if !parse_only
  then exit 0 ;
  
  let module Inference = Inference.Make(TypeErr) in
  let module CheckMain = CheckMain(TypeErr) in
  let (_, defsType) = Inference.infer_potentially_mutually_recursive_definitions prim [] env defs in
  CheckMain.check defsType ;

  if TypeErr.has_error_occured ()
  then
    begin
      List.iter print_endline (TypeErr.get_all ()) ;
      exit 1
    end  ;
  
  if !print_type
  then
    begin
      List.iter
	(fun (x,s) ->
	 Format.fprintf Format.std_formatter "%s :: %a\n"
   			x Printer.print_schema (Schema.normal_form s.Schema.value)
	) defsType
    end

