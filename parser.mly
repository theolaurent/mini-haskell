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
%}

%parameter <Err:Errors.S>

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
| BSLASH l = nonempty_list(ID) ARR e = expr { map (fun e -> ast_lambda l e) e }
| BSLASH ARR e = expr
    { parsing_error "Expecting variable(s) in lambda-clause" $startpos($1) $endpos($2) } 
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
| DO l = bounded_separated_list(LB, SCOL, expr, RB)
    { List.fold_left (map2 ast_do) (List.hd l) (List.tl l) }
| DO LB SCOL RB 
	{ parsing_error "do expects one or more expressions" $startpos($2) $endpos($4) }
| DO LB RB
	{ parsing_error "do expects one or more expressions" $startpos($2) $endpos($3) }
| RETURN LP RP { Some (Ast.Const (Ast.CPrim "return ()")) }

binds:
| d = def { map (fun x -> [x]) d }
| l = bounded_separated_list(LB, SCOL, def, RB) { sequence l }
| LB SCOL RB 
    { parsing_error "let expects one or more bindings" $startpos($1) $endpos($3) }
| LB RB
    { parsing_error "let expects one or more bindings" $startpos($1) $endpos($2) } 
    
stopped_separated_list(sep, X, stop):
| x = X ; stop { [x] }
| x = X ; sep ; stop { [x] }
| x = X ; sep ; l = stopped_separated_list(sep, X, stop) { x :: l }
				
bounded_separated_list(start, sep, X, stop):
| start ; l = stopped_separated_list(sep, X, stop) { l }
				
				
const:
| c = CHAR { Some (Const (CChar c)) }
| i = INT { Some (Const (CInt  i)) }
| s = STR { let l = List.map (fun c -> Const (CChar c)) (Utils.string_to_list s)
            in Some (ast_list l) }
| b = BOOL { Some (Const (CBool b)) }
