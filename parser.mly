(* Parser for Mini-Haskell *)
%{
  open Ast
%}

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
%token <string * Lexing.position * Lexing.position> LEX_ERROR

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

%start <Ast.def list> main

%%

main:
| ds = list(def0) EOF { ds }

def0:
| id = ID0 l = list(ID) EQ e = expr { Def (id, ast_lambda l e) }

def:
| id = ID l = list(ID) EQ e = expr { (id, ast_lambda l e) }

simple_expr:
| LP e = expr RP { e }
| id = ID { Var id }
| c = const { c }
| LBK l = separated_list(COM, expr) RBK { ast_list l }

expr:
| l = nonempty_list(simple_expr) { ast_app (List.hd l) (List.tl l) }
| BSLASH l = nonempty_list(ID) ARR e = expr { ast_lambda l e }
| MINUS e = expr %prec UMINUS { ast_unary_minus e }
(* why not a single case for binary operators ? *)
| a = expr OR b = expr { ast_or a b }
| a = expr AND b = expr { ast_and a b }
| a = expr LT b = expr { ast_lt a b }
| a = expr LEQ b = expr { ast_leq a b }
| a = expr GT b = expr { ast_gt a b }
| a = expr GEQ b = expr { ast_geq a b }
| a = expr EQ b = expr { ast_eq a b }
| a = expr NEQ b = expr { ast_neq a b }
| a = expr COL b = expr { ast_cons a b }
| a = expr PLUS b = expr { ast_plus a b }
| a = expr MINUS b = expr { ast_minus a b }
| a = expr MULT b = expr { ast_mult a b }
| IF c = expr THEN a = expr ELSE b = expr { ast_if c a b }
| LET l = binds IN e = expr { ast_let l e }
| CASE e = expr OF LB LBK RBK ARR x = expr SCOL
                      hd = ID COL tl = ID ARR y = expr SCOL? RB
                                                        { ast_case e x hd tl y }
| DO LB l = separated_nonempty_list(SCOL, def) SCOL? RB { failwith "TODO" }
| RETURN LP RP { failwith "TODO" }

binds:
| d = def { [d] }
| LB l = separated_nonempty_list(SCOL, def) SCOL? RB { l }

const:
| c = CHAR { Const (CChar c) }
| i = INT { Const (CInt  i) }
| s = STR { let l = List.map (fun c -> Const (CChar c)) (Utils.string_to_list s)
            in ast_list l }
| b = BOOL { Const (CBool b) }
