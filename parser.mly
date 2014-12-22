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


let ann spos epos ?ty ast =
  annotate (pos spos epos) ?ty ast

module Id =
struct
  let name { data ; _ } = data
  let spos { annot = (Pos (s, _), _) ; _ } = s
  let epos { annot = (Pos (_, e), _) ; _ } = e
end

module Def =
struct
  let name d = Id.name (fst d)
  let spos d = Id.spos (fst d)
  let epos d = Id.epos (fst d)
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
%token COL SCOL COM DCOL
%token BSLASH ARR BARR EQ
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
| ds = list(def0) EOF
  {
    let l = List.fold_right
      (fun d l ->
	 match d with
         | Some d -> d :: l
         | None -> l
      ) ds []
    in
    check_uniqueness Def.name Def.spos Def.epos l ;
    l
  }
| error EOF
  { ignore (parsing_error "..." $startpos($1) $endpos($1)) ; [] }
  

def0:
| s = signature0 id = ID0 l = list(identifier) EQ e = expr
  {
    let (id', sch) = s in
    let ty =
      if id = id'
      then Some sch
      else None
    in          
    check_uniqueness Id.name Id.spos Id.epos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) ?ty id in
        (id, ast_lambda (pos $startpos(id) $endpos(e)) l e)
      ) e
  }
| id = ID0 l = list(identifier) EQ e = expr
  {
    check_uniqueness Id.name Id.spos Id.epos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) id in
        (id, ast_lambda (pos $startpos(id) $endpos(e)) l e)
      ) e
  }
  
def:
| s = signature SCOL id = ID l = list(identifier) EQ e = expr
  {
    let (id', sch) = s in
    let ty = if id = id' then Some sch else None in
    check_uniqueness Id.name Id.spos Id.epos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) ?ty id in
        (id, ast_lambda (pos $startpos(id) $endpos(e)) l e)
      ) e    
  }
| id = ID l = list(identifier) EQ e = expr
  {
    check_uniqueness Id.name Id.spos Id.epos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) id in
        (id, ast_lambda (pos $startpos(id) $endpos(e)) l e)
      ) e
  }

signature0:
| id = ID0 DCOL s = schema
  { (id, Schema_ast.convert s) }
  
signature:
| id = ID DCOL s = schema
  { (id, Schema_ast.convert s) }


schema:
| t = ty { Schema_ast.S ([], Schema_ast.Ty t) }
| l = separated_nonempty_list(COM, schema_binding) BARR t = ty
  {
    Schema_ast.S (l, Schema_ast.Ty t)
  }
      
schema_binding:
| id = ID { Schema_ast.generic_var id }              
| id = ID b = schema_bound LP s = schema RP { (id, b, s) }
          
schema_bound:
| GEQ { Schema_ast.Flexible }
| EQ  { Schema_ast.Rigid }

simple_ty:
| LP t = ty RP { t }
| i = ID { Schema_ast.Identifier (i, []) }

ty:
| i = ID l = list(simple_ty) { Schema_ast.Identifier (i, l) }     
| t1 = ty ARR t2 = ty { Schema_ast.Arrow (t1, t2) }

simple_expr:
| LP e = expr RP { map Utils.id e }
| LP error RP    { parsing_error "..." $startpos($2) $endpos($2) }
| id = ID { Some (ann $startpos(id) $endpos(id) (Var id)) }
| c = const { map Utils.id c }
| LBK l = separated_list(COM, expr) RBK
  {
    map (ast_list (pos $startpos($1) $endpos($3))) (sequence l)
  }
| LBK error RBK { parsing_error "..." $startpos($2) $endpos($2) }

expr:
| LP e = expr DCOL s = schema RP
  {
    map (fun e -> annotate (fst e.annot) ~ty:(Schema_ast.convert s) e.data) e
  }
| l = nonempty_list(simple_expr)
  {
    map (fun x -> ast_app (pos $startpos(l) $endpos(l))
            (List.hd x) (List.tl x)) (sequence l)
  }
| BSLASH l = nonempty_list(identifier) ARR e = expr
  {
    check_uniqueness Id.name Id.spos Id.epos l ;
    map (fun e -> ast_lambda (pos $startpos($1) $endpos(e)) l e) e
  }
| BSLASH ARR expr
  {
    parsing_error "Expecting variable(s) in lambda-clause" $startpos($1) $endpos($2)
  } 
| MINUS e = expr %prec UMINUS
  {
    map (ast_unary_minus @@ pos $startpos($1) $endpos(e)) e
  }
| a = expr OR    b = expr { map2 (ast_or    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr AND   b = expr { map2 (ast_and   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr LT    b = expr { map2 (ast_lt    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr LEQ   b = expr { map2 (ast_leq   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr GT    b = expr { map2 (ast_gt    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr GEQ   b = expr { map2 (ast_geq   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr EQEQ  b = expr { map2 (ast_eq    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr NEQ   b = expr { map2 (ast_neq   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr COL   b = expr { map2 (ast_cons  @@ pos $startpos(a) $endpos(b)) a b }
| a = expr PLUS  b = expr { map2 (ast_plus  @@ pos $startpos(a) $endpos(b)) a b }
| a = expr MINUS b = expr { map2 (ast_minus @@ pos $startpos(a) $endpos(b)) a b }
| a = expr MULT  b = expr { map2 (ast_mult  @@ pos $startpos(a) $endpos(b)) a b }
| IF c = expr THEN a = expr ELSE b = expr
  {
    map3 (ast_if @@ pos $startpos($1) $endpos(b)) c a b
  }
| LET l = binds IN e = expr
  {
    iter (check_uniqueness Def.name Def.spos Def.epos) l ;
    map2 (ast_let @@ pos $startpos($1) $endpos(e)) l e
  }
| CASE e = expr OF LB
  LBK RBK ARR x = expr SCOL
  hd = identifier COL tl = identifier ARR y = expr SCOL? RB
  {
    check_uniqueness Id.name Id.spos Id.epos [hd ; tl] ;
    map3
      (fun e x y ->
         ast_case (pos $startpos($1) $endpos($16)) e x hd tl y
      ) e x y
  }
| DO LB l = opt_separated_list(SCOL, expr) RB
  {
    let l = List.fold_right (fun x acc ->
        match (x, acc) with
        | (Some instr, Some acc) -> Some (instr :: acc)
          | _ -> None
      ) l (Some [])
    in
    map (ast_do @@ pos $startpos($1) $endpos($4)) l
  }
| DO LB SCOL RB 
  { parsing_error "do expects one or more expressions" $startpos($2) $endpos($4) }
| DO LB RB
  { parsing_error "do expects one or more expressions" $startpos($2) $endpos($3) }
| RETURN LP RP
  { Some (ast_return @@ pos $startpos($1) $endpos($3)) }
  
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


  
(*identifier0:				
| i = ID0 { annotate (pos $startpos(i) $endpos(i)) i }
| i = ID0 DCOL s = schema { annotate (pos $startpos(i) $endpos(i)) ~ty:(`Annot s) i }
*)
                  
identifier:				
| i = ID { annotate (pos $startpos(i) $endpos(i)) i }
| LP i = ID DCOL s = schema RP
  {
    let s = Schema_ast.convert s in
    annotate (pos $startpos(i) $endpos(i)) ~ty:s i }

const:
| c = CHAR { Some (ann $startpos(c) $endpos(c) (Const (CChar c))) }
| i = INT { Some (ann $startpos(i) $endpos(i)  (Const (CInt  i))) }
| s = STR
  {
    let l =
      List.map
        (fun c -> ann $startpos(s) $endpos(s) (Const (CChar c)))
        (Utils.string_to_list s)
    in
    Some (ast_list (pos $startpos(s) $endpos(s)) l)
  }
| b = BOOL { Some (ann $startpos(b) $endpos(b) (Const (CBool b))) }

