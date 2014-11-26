(* Parser for Mini-Haskell *)
%{
  open Ast
  open Utils.OptionM

  let parsing_error msg startpos endpos =
    let () = Err.report ("syntax error: " ^ msg)
                        (startpos)
                        (endpos)
    in None

       (* TODO : improve error reporting by adding some error
                 cases in the grammar *)
  let check_uniqueness name spos epos l =
    let l = List.sort compare l in
    let rec pred = function
      | [] | [_] -> []
      | x :: y :: t ->
	 if name x = name y
	 then (x, y) :: pred (y :: t)
	 else pred (y :: t)
    in
    List.iter (fun (x, y) ->
	       let n = name x in
	       TypeErr.report ("type error: " ^ "identifier " ^ n ^ " is bound here...") (spos x) (epos x);
	       TypeErr.report ("type error; " ^ "... but is also bound there") (spos y) (epos y)
	      ) (pred l)
	      

  module Id =
    struct
      let name (x, _) = x
      let spos (_, (s, _)) = s
      let epos (_, (_, e)) = e
    end
	      
%}

%parameter <Err:Errors.S>
%parameter <TypeErr:Errors.S>
		  
%token <string> STR
%token <char> CHAR
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token <string> ID0
%token ELSE IF IN LET CASE THEN RETURN DO OF
%token LP RP LB RB LBK RBK
%token COL SCOL COM
%token BSLASH ARR EQ
%token OR AND
%token LT LEQ GT GEQ EQEQ NEQ
%token PLUS MINUS MULT
%token EOF

%nonassoc IN
%nonassoc ELSE
%nonassoc ARR
%left OR
%left AND
%left LT LEQ GT GEQ EQEQ NEQ
%right COL
%left PLUS MINUS
%left MULT
%nonassoc UMINUS

%start <Ast.def list> main

%%

main:
| ds = list(def0) EOF {
  
  List.fold_right (fun d l ->
    match d with
    | Some d -> d :: l
    | None -> l
  ) ds []
}
| error EOF           { ignore (parsing_error "..." $startpos($1) $endpos($1)) ; [] }

def0:
| id = ID0 l = list(ID) EQ e = expr { map (fun e ->
                                        (id, ast_lambda l e)
                                      ) e }

def:
| id = ID l = list(ID) EQ e = expr { map (fun e ->
					  (id, ast_lambda l e)
                                     ) e }

simple_expr:
| LP e = expr RP { map Utils.id e }
| LP error RP    { parsing_error "..." $startpos($2) $endpos($2) }
| id = ID { Some (Var id) }
| c = const { map Utils.id c }
| LBK l = separated_list(COM, expr) RBK { map (fun x -> ast_list x) (sequence l) }
| LBK error RBK { parsing_error "..." $startpos($2) $endpos($2) }

expr:
| l = nonempty_list(simple_expr) { map (fun x -> ast_app (List.hd x) (List.tl x)) (sequence l) }
| BSLASH l = nonempty_list(identifier) ARR e = expr {
						   check_uniqueness Id.name Id.spos Id.epos l ;
						   let (l, _) = List.split l in 
						   map (fun e -> ast_lambda l e) e
					 }
| BSLASH ARR expr
	 {
	   parsing_error "Expecting variable(s) in lambda-clause" $startpos($1) $endpos($2)
	 } 
| MINUS e = expr %prec UMINUS { map (fun x -> ast_unary_minus x) e }
(* why not a single case for binary operators ? *)
| a = expr OR b = expr { map2 (fun a b -> ast_or a b) a b }
| a = expr AND b = expr { map2 (fun a b -> ast_and a b) a b }
| a = expr LT b = expr { map2 (fun a b -> ast_lt a b) a b }
| a = expr LEQ b = expr { map2 (fun a b -> ast_leq a b) a b }
| a = expr GT b = expr { map2 (fun a b -> ast_gt a b) a b }
| a = expr GEQ b = expr { map2 (fun a b -> ast_geq a b) a b }
| a = expr EQEQ b = expr { map2 (fun a b -> ast_eq a b) a b }
| a = expr NEQ b = expr { map2 (fun a b -> ast_neq a b) a b }
| a = expr COL b = expr { map2 (fun a b -> ast_cons a b) a b }
| a = expr PLUS b = expr { map2 (fun a b -> ast_plus a b) a b }
| a = expr MINUS b = expr { map2 (fun a b -> ast_minus a b) a b }
| a = expr MULT b = expr { map2 (fun a b -> ast_mult a b) a b }
| IF c = expr THEN a = expr ELSE b = expr {
                           map3 (fun c a b -> ast_if c a b) c a b }
| LET l = binds IN e = expr { map2 (fun l e -> ast_let l e) l e }
| CASE e = expr OF LB LBK RBK ARR x = expr SCOL
                      hd = ID COL tl = ID ARR y = expr SCOL? RB
                             { map3 (fun e x y ->
                                     ast_case e x hd tl y)
                                    e x y }
| DO LB l = opt_separated_list(SCOL, expr) RB
    { List.fold_left (map2 ast_do) (List.hd l) (List.tl l) }
| DO LB SCOL RB 
	{ parsing_error "do expects one or more expressions" $startpos($2) $endpos($4) }
| DO LB RB
	{ parsing_error "do expects one or more expressions" $startpos($2) $endpos($3) }
| RETURN LP RP { Some (Ast.Const (Ast.CPrim "return ()")) }

binds:
| d = def { map (fun x -> [x]) d }
| LB l = opt_separated_list(SCOL, def) RB { sequence l }
| LB SCOL RB 
    { parsing_error "let expects one or more bindings" $startpos($1) $endpos($3) }
| LB RB
    { parsing_error "let expects one or more bindings" $startpos($1) $endpos($2) } 
    
opt_separated_list(sep, X):
| x = X ; sep? { [x] }
| x = X ; sep ; l = opt_separated_list(sep, X) { x :: l }

				
identifier:				
| i = ID { (i, ($startpos(i), $endpos(i))) }




const:
| c = CHAR { Some (Const (CChar c)) }
| i = INT { Some (Const (CInt  i)) }
| s = STR { let l = List.map (fun c -> Const (CChar c)) (Utils.string_to_list s)
            in Some (ast_list l) }
| b = BOOL { Some (Const (CBool b)) }
