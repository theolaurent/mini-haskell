open Ast
(* Printers for ast *)
let print_const ff = function
  | CUnit -> Format.fprintf ff "()"
  | CBool b -> Format.fprintf ff "%b" b
  | CInt i -> Format.fprintf ff "%d" i
  | CChar c -> Format.fprintf ff "'%s'" (Char.escaped c)
  | CPrim "empty" -> Format.fprintf ff "[]"
  | CPrim s -> Format.fprintf ff "%s" s


let rec print_ast ff ast = match ast.data with
  | Spec s ->
     print_spec ff s
  | Const c -> print_const ff c
  | Var x -> Format.fprintf ff "%s" x
  | Abstr (x, body) ->
     Format.fprintf ff "λ%s. %a" x.data print_ast body
  | App (x, y) ->
     Format.fprintf ff "%a (%a)" print_ast x print_ast y
  | Let (l, e) ->
     begin
       Format.open_hovbox 0 ;
       Format.fprintf ff "let { %a} in@ %a" print_def_list l print_ast e ;
       Format.close_box ()
     end
  and print_spec ff = function
    | If (cond, btrue, bfalse) ->
       Format.fprintf ff "if %a@ then %a@ else %a"
		      print_ast cond
		      print_ast btrue
		      print_ast bfalse
    | Case (list, bempty, hd, tl, bnempty) ->
       Format.fprintf ff "case %a of {@ " print_ast list ;
       Format.open_hovbox 4 ;
       Format.fprintf ff "[] -> %a,@ %s : %s -> %a"
		      print_ast bempty
		      hd.data tl.data
		      print_ast bnempty ;
       Format.close_box () ;
       Format.fprintf ff "@ }@ "			
    | Do l ->
       Format.fprintf ff "do {@ " ;
       Format.open_hovbox 4 ;
       List.iter (fun instr ->
		  Format.fprintf ff "%a;@ " print_ast instr
		 ) l ;
       Format.close_box () ;
       Format.fprintf ff "@ }@ "
    | Return ->
       Format.fprintf ff "return ();@ "
  and print_def_list ff l =
    List.iter (fun (x,b) ->
	       Format.open_hovbox 4 ;
	       Format.fprintf ff "%s =@ %a;" x.data print_ast b ;
	       Format.close_box () ;
	       Format.print_cut ()) l

