(* Parser for Mini-Haskell *)
%{
  open Ast
  open Utils.OptionM

  let parsing_error msg i =
    let () = Err.report ("syntax error: " ^ msg)
                        (Parsing.rhs_start_pos i)
                        (Parsing.rhs_end_pos i)
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
%token BSLASH ARR
%token OR AND
%token LT LEQ GT GEQ EQ NEQ
%token PLUS MINUS MULT
%token EOF
%token LEX_ERROR

%nonassoc IN
%nonassoc ELSE
%nonassoc ARR
%left OR
%left AND
%left LT LEQ GT GEQ EQ NEQ
%right COL
%left PLUS MINUS
%left MULT
%nonassoc UMINUS

%start <Ast.def list option> main

%%

main:
| ds = list(def0) EOF { sequence ds }
| error EOF           { parsing_error "..." 1 }

def0:
| id = ID0 l = list(ID) EQ e = expr { map (fun e ->
                                        Def (id, ast_lambda l e)
                                      ) e }

def:
| id = ID l = list(ID) EQ e = expr { map (fun e ->
                                       (id, ast_lambda l e)
                                     ) e }

simple_expr:
| LP e = expr RP { map Utils.id e }
| LP error RP    { parsing_error "..." 2 }
| id = ID { Some (Var id) }
| c = const { map Utils.id c }
| LBK l = separated_list(COM, expr) RBK { map (fun x -> ast_list x) (sequence l) }
| LBK error RBK { parsing_error "..." 2 }

expr:
| l = nonempty_list(simple_expr) { map (fun x -> ast_app (List.hd x) (List.tl x)) (sequence l) }
| BSLASH l = nonempty_list(ID) ARR e = expr { map (fun e -> ast_lambda l e) e }
| MINUS e = expr %prec UMINUS { map (fun x -> ast_unary_minus x) e }
(* why not a single case for binary operators ? *)
| a = expr OR b = expr { map2 (fun a b -> ast_or a b) a b }
| a = expr AND b = expr { map2 (fun a b -> ast_and a b) a b }
| a = expr LT b = expr { map2 (fun a b -> ast_lt a b) a b }
| a = expr LEQ b = expr { map2 (fun a b -> ast_leq a b) a b }
| a = expr GT b = expr { map2 (fun a b -> ast_gt a b) a b }
| a = expr GEQ b = expr { map2 (fun a b -> ast_geq a b) a b }
| a = expr EQ b = expr { map2 (fun a b -> ast_eq a b) a b }
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
| DO LB l = separated_nonempty_list(SCOL, def) SCOL? RB { failwith "TODO" }
| RETURN LP RP { failwith "TODO" }

binds:
| d = def { map (fun x -> [x]) d }
| LB l = separated_nonempty_list(SCOL, def) SCOL? RB { sequence l }

const:
| c = CHAR { Some (Const (CChar c)) }
| i = INT { Some (Const (CInt  i)) }
| s = STR { let l = List.map (fun c -> Const (CChar c)) (Utils.string_to_list s)
            in Some (ast_list l) }
| b = BOOL { Some (Const (CBool b)) }
