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
    let check defs defsType =
      try
	let ({ Ast.annot = (Ast.Pos (spos, epos), _) ; _}, _) = List.find (fun (x,_) -> x.Ast.data = "main") defs in
	try
	  let sch = List.assoc "main" defsType in
	  let alpha = Var.fresh () in
	  ignore (Unification.unify [(alpha, (Schema.BFlexible, sch))] (Ty.variable alpha) (Ty.constructor "IO" [Ty.constructor "()" []]))
	with Unification.Failure trace ->
	  let msg = List.fold_left (fun s m -> s ^ "\n\t\t" ^ m) "type error : While typing main..." trace in
	  Err.report msg spos epos ;
      with Not_found -> Err.report "type error : main is not present" (Lexing.dummy_pos) (Lexing.dummy_pos)
  end

module CheckPrimitiveName(Err:Errors.S) =
  struct
    let primitives = ["div"; "rem"; "putChar" ; "error"]
    let check defs =
      List.iter (fun (d, _) ->
		 if List.mem d.Ast.data primitives
		 then begin
		     let (Ast.Pos (spos, epos), _) = d.Ast.annot in
		     Format.fprintf Format.str_formatter
				    "type error : %s is a primitive name" d.Ast.data ;
		     Err.report (Format.flush_str_formatter ()) spos epos
		   end) defs
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
  let module CheckPrimitiveName = CheckPrimitiveName(TypeErr) in
  let (_, defsType) = Inference.infer_potentially_mutually_recursive_definitions prim [] env defs Lexing.dummy_pos Lexing.dummy_pos in
  CheckMain.check defs defsType ;
  CheckPrimitiveName.check defs ;

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
   			x Schema_printer.print_schema (Schema.normal_form s.Schema.value)
	) defsType
    end

