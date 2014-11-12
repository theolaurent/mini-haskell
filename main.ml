(* Main Program *)

let usage = "usage: petitghc [options] file.hs"

let parse_only = ref false
let type_only = ref false
		     
let spec =
  [
    "--parse-only", Arg.Set parse_only, "stop after parsing";
    "--type-only", Arg.Set type_only, "stop after typing" ;
  ]

let prim : Schema.schema Ast.Primitive.t =
  let var = Var.fresh () in
  let var2 = Var.fresh () in
  let binop n t r = 
    Ast.Primitive.add n (Schema.ty
		       (Ty.arrow (Ty.constructor t [])
				 (Ty.arrow (Ty.constructor t [])
		       			   (Ty.constructor r []))))
  in
  Ast.Primitive.empty
  |> binop "plus"  "int"  "int"
  |> binop "minus" "int"  "int"
  |> binop "mult"  "int"  "int"
  |> binop "lt"    "int"  "bool"
  |> binop "gt"    "int"  "bool"
  |> binop "leq"   "int"  "bool"
  |> binop "geq"   "int"  "bool"
  |> binop "eq"    "int"  "bool"
  |> binop "neq"   "int"  "bool"
  |> binop "and"   "bool" "bool"
  |> binop "or"    "bool" "bool"
  |> Ast.Primitive.add "unary_minus"
		   (Schema.ty (Ty.arrow (Ty.constructor "int" []) (Ty.constructor "int" [])))
  |> Ast.Primitive.add "empty"
		   (Schema.forall (var, Schema.(BFlexible, bot))
				  (Schema.ty
				     (Ty.constructor "list" [Ty.variable var])))
  |> Ast.Primitive.add "cons"
		   (Schema.forall
		      (var, Schema.(BFlexible, bot))
		      (Schema.ty
			 (Ty.arrow (Ty.variable var)
				   (Ty.arrow (Ty.constructor "list" [Ty.variable var])
					     (Ty.constructor "list" [Ty.variable var])))))
  |> Ast.Primitive.add "if"
		   (Schema.forall
		      (var, Schema.(BFlexible,bot))
		      (Schema.ty
			 (Ty.arrow (Ty.constructor "bool" [])
				   (Ty.arrow (Ty.variable var)
					     (Ty.arrow (Ty.variable var)
						       (Ty.variable var))))))

  (* do (do a b) c 'a -> ('b -> 'b) *)
  |> Ast.Primitive.add "do"
		       (Schema.forall_map
			  [(var, Schema.(BFlexible, bot)) ; (var2, Schema.(BFlexible, bot))]
			  (Schema.ty
			     (Ty.arrow (Ty.variable var) (Ty.arrow (Ty.variable var2) (Ty.variable var2)))))
			  

  |> Ast.Primitive.add "match"
      (Schema.forall_map
	 [(var, Schema.(BFlexible, bot)) ; (var2, Schema.(BFlexible, bot))]
	 (Schema.ty
	    (Ty.arrow (Ty.constructor "list" [Ty.variable var])
	       (Ty.arrow (Ty.variable var2)
		  (Ty.arrow
		     (Ty.arrow (Ty.variable var) (Ty.arrow (Ty.constructor "list" [Ty.variable var]) (Ty.variable var2)))
		     (Ty.variable var2))))))

let env =
  let var = Var.fresh () in
  [
    ("putChar", Schema.ty (Ty.arrow (Ty.constructor "char" [])
			     (Ty.constructor "unit" []))) ;
    ("rem", Schema.ty
      (Ty.arrow
	 (Ty.constructor "int" [])
	 (Ty.arrow (Ty.constructor "int" []) (Ty.constructor "int" [])))) ;
    ("div", Schema.ty
      (Ty.arrow
	 (Ty.constructor "int" [])
	 (Ty.arrow (Ty.constructor "int" []) (Ty.constructor "int" [])))) ;
    ("error", Schema.forall (var, Schema.(BFlexible, bot))
			  (Schema.ty
			     (Ty.arrow (Ty.constructor "list" [Ty.constructor "char" []])
				       (Ty.variable var))))
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
  let defs = Par.main Lex.token lb in
  if Err.has_error_occured ()
  then 
    begin
      List.iter print_endline (Err.get_all ()) ;
      exit 1 ;
    end ;

  List.iter (fun (x,b) ->
    Format.fprintf Format.std_formatter "%s = %a\n" x Printer.print_ast b
  ) defs ;
  
  if !parse_only then exit 0
  else begin
        let (_, defsType) = Inference.infer_potentially_mutually_recursive_definitions prim [] env defs in 
	List.iter (fun (x,s) ->
		   Format.fprintf Format.std_formatter "%s <: %a\n"
   				  x Printer.print_schema (Schema.normal_form s)) defsType
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
