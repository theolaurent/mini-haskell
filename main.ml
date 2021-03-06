(* Main Program *)
open Typer
open Mips

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

let env = Definitions.env

module CheckMain(Err:Errors.S) =
  struct
    let check defs defsType =
      try
	let ({ Ast.annot = (pos, _) ; _}, _) =
	  List.find (fun (x,_) -> x.Ast.data = "main") defs in
	try
	  let sch = List.assoc "main" defsType in
	  let alpha = Var.fresh () in
	  Unification.unify [(alpha, (Schema.BFlexible, sch))]
			    (Ty.variable alpha)
			    Definitions.(io !!"()")
	  |> ignore
	with Unification.Failure trace ->
	  let msg =
	    List.fold_left
	      (fun s m -> s ^ "\n\t\t" ^ m)
	      "type error : While typing main..." trace
	  in
	  Err.report msg pos ;
      with Not_found ->
	Err.report "type error : main is not present" Pos.dummy
  end

module CheckPrimitiveName(Err:Errors.S) =
  struct
    let primitives = ["div"; "rem"; "putChar" ; "error"]
    let check defs =
      List.iter
	(fun (d, _) ->
	 if List.mem d.Ast.data primitives
	 then
	   begin
	     let (pos, _) = d.Ast.annot in
	     Format.fprintf
	       Format.str_formatter
	       "type error : %s is a primitive name" d.Ast.data ;
	     Err.report (Format.flush_str_formatter ()) pos
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
  let (_, defsType) = Inference.infer_potentially_mutually_recursive_definitions [] env defs Pos.dummy in

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
    end ;

  if !type_only
  then exit 0 ;

  let _ =
    List.fold_left
      (fun globals ({ Ast.data = s ; _ }, _) -> Ast.VarSet.add s globals)
      Ast.VarSet.empty defs
    |> Ast.VarSet.add "putChar"
    |> Ast.VarSet.add "error"
    |> Ast.VarSet.add "div"
    |> Ast.VarSet.add "rem"
  in

  (* Stora all the global declarations, and the force the main *)
  let ir =
    let ir =
      List.fold_left
	(fun ir ({ Ast.data = s ; _ }, expr) ->
	 let expr = Ir.calc_free_vars_ast expr in
	 ir @ Ir.froze Ir.VarMap.empty expr @ [Ir.Store (Ir.GlobalVar s)])
	Ir.primitives defs
    in
    ir @ [Ir.Fetch (Ir.GlobalVar "main") ; Ir.Force]
  in

  (* Compile to mips *)
  let code =
    List.fold_left
      (fun code instr -> code ++ Compile.compile_instr instr)
      nop ir
  in
  (* Add labels for globals in data section *)
  let data =
    List.fold_left
      (fun code ({ Ast.data = s ; _}, _) -> code ++ label ("G" ^ s) ++ dword [0])
      nop defs
  in
  let program =
      {
	text =
	  label "main"
	  ++ code
	  ++ Compile.exit_code 0
	  ++ Compile.force ;
	data =
	  Compile.error_msg
	  ++ Compile.div_by_zero_msg
	  ++ label "GputChar" ++ dword [0]
	  ++ label "Gerror" ++ dword [0]
	  ++ label "Gdiv" ++ dword [0]
	  ++ label "Grem" ++ dword [0]
	  ++ data
      }
  in
  let file = String.sub file 0 (String.rindex file '.' + 1) ^ "s" in
  print_in_file ~file program
