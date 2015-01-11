(* Parser for Mini-Haskell *)
%{
open Ast
open Utils.OptionM

let error (module ErrM: Errors.S) prefix msg pos =
  ErrM.report (prefix ^ msg) pos ;
  None

let parsing_error = error (module Err)     "syntax error: "
let typing_error  = error (module TypeErr) "typing error: "

let check_uniqueness name pos l =
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
      typing_error ("identifier " ^ n ^ " is bound here...") (pos x)
      |> ignore ;
      typing_error "... but is also bound there"             (pos y)
      |> ignore
    ) (pred l)


let ann start stop ?ty expr =
  annotate (Pos.position ~start ~stop) ?ty expr

let pos start stop = Pos.position ~start ~stop

module Id =
struct
  let name { data ; _ } = data
  let pos { annot = (p, _) ; _ } = p
end

module Def =
struct
  let name d = Id.name (fst d)
  let pos d = Id.pos (fst d)
end


%}

%parameter <Err:Errors.S>
%parameter <TypeErr:Errors.S>

%token <string> STR
%token <char> CHAR
%token <int> INT
%token <bool> BOOL
%token <string> ID
%token <string> LID
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
%right ARR
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
    check_uniqueness Def.name Def.pos l ;
    l
  }
| error EOF
  { ignore (parsing_error "..." (pos $startpos($1) $endpos($1))) ; [] }


def0:
| s = signature0 id = ID0 l = list(identifier) EQ e = expr
  {
    let (id', sch) = s in
    let ty =
      if id = id'
      then Some sch
      else None
    in
    check_uniqueness Id.name Id.pos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) ?ty id in
        (id, expr_lambda (pos $startpos(id) $endpos(e)) l e)
      ) e
  }
| id = ID0 l = list(identifier) EQ e = expr
  {
    check_uniqueness Id.name Id.pos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) id in
        (id, expr_lambda (pos $startpos(id) $endpos(e)) l e)
      ) e
  }

def:
| s = signature SCOL id = ID l = list(identifier) EQ e = expr
  {
    let (id', sch) = s in
    let ty = if id = id' then Some sch else None in
    check_uniqueness Id.name Id.pos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) ?ty id in
        (id, expr_lambda (pos $startpos(id) $endpos(e)) l e)
      ) e
  }
| id = ID l = list(identifier) EQ e = expr
  {
    check_uniqueness Id.name Id.pos l ;
    map (fun e ->
        let id = ann $startpos(id) $endpos(id) id in
        (id, expr_lambda (pos $startpos(id) $endpos(e)) l e)
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
| i = tid { Schema_ast.Identifier (i, []) }

ty:
| i = tid l = nonempty_list(simple_ty) { Schema_ast.Identifier (i, l) }
| t1 = ty ARR t2 = ty { Schema_ast.Arrow (t1, t2) }
| s = simple_ty { s }


tid:
| i = ID { i }
| i = LID { i }
| LP RP { "()" }

simple_expr:
| LP e = expr RP { map Utils.id e }
| LP error RP    { parsing_error "..." (pos $startpos($2) $endpos($2)) }
| id = ID { Some (ann $startpos(id) $endpos(id) (Var id)) }
| c = const { map Utils.id c }
| LBK l = separated_list(COM, expr) RBK
  {
    map (expr_list (Pos.position ~start:$startpos($1) ~stop:$endpos($3))) (sequence l)
  }
| LBK error RBK { parsing_error "..." (pos $startpos($2) $endpos($2)) }

expr:
| LP e = expr DCOL s = schema RP
  {
    map (fun e -> annotate (fst e.annot) ~ty:(Schema_ast.convert s) e.data) e
  }
| l = nonempty_list(simple_expr)
  {
    map (fun x -> expr_app (pos $startpos(l) $endpos(l))
            (List.hd x) (List.tl x)) (sequence l)
  }
| BSLASH l = nonempty_list(identifier) ARR e = expr
  {
    check_uniqueness Id.name Id.pos l ;
    map (fun e -> expr_lambda (pos $startpos($1) $endpos(e)) l e) e
  }
| BSLASH ARR expr
  {
    parsing_error "Expecting variable(s) in lambda-clause"
		  (pos $startpos($1) $endpos($2))
  }
| MINUS e = expr %prec UMINUS
  {
    map (expr_unary_minus @@ pos $startpos($1) $endpos(e)) e
  }
| a = expr OR    b = expr { map2 (expr_or    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr AND   b = expr { map2 (expr_and   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr LT    b = expr { map2 (expr_lt    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr LEQ   b = expr { map2 (expr_leq   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr GT    b = expr { map2 (expr_gt    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr GEQ   b = expr { map2 (expr_geq   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr EQEQ  b = expr { map2 (expr_eq    @@ pos $startpos(a) $endpos(b)) a b }
| a = expr NEQ   b = expr { map2 (expr_neq   @@ pos $startpos(a) $endpos(b)) a b }
| a = expr COL   b = expr { map2 (expr_cons  @@ pos $startpos(a) $endpos(b)) a b }
| a = expr PLUS  b = expr { map2 (expr_plus  @@ pos $startpos(a) $endpos(b)) a b }
| a = expr MINUS b = expr { map2 (expr_minus @@ pos $startpos(a) $endpos(b)) a b }
| a = expr MULT  b = expr { map2 (expr_mult  @@ pos $startpos(a) $endpos(b)) a b }
| IF c = expr THEN a = expr ELSE b = expr
  {
    map3 (expr_if @@ pos $startpos($1) $endpos(b)) c a b
  }
| LET l = binds IN e = expr
  {
    iter (check_uniqueness Def.name Def.pos) l ;
    map2 (expr_let @@ pos $startpos($1) $endpos(e)) l e
  }
| CASE e = expr OF LB
  LBK RBK ARR x = expr SCOL
  hd = identifier COL tl = identifier ARR y = expr SCOL? RB
  {
    check_uniqueness Id.name Id.pos [hd ; tl] ;
    map3
      (fun e x y ->
         expr_case (pos $startpos($1) $endpos($16)) e x hd tl y
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
    map (expr_do @@ pos $startpos($1) $endpos($4)) l
  }
| DO LB SCOL RB
  { parsing_error "do expects one or more expressions" (pos $startpos($2) $endpos($4)) }
| DO LB RB
  { parsing_error "do expects one or more expressions" (pos $startpos($2) $endpos($3)) }
| RETURN LP RP
  { Some (expr_return @@ pos $startpos($1) $endpos($3)) }

binds:
| d = def { map (fun x -> [x]) d }
| LB l = opt_separated_list(SCOL, def) RB { sequence l }
| LB SCOL RB
     { parsing_error "let expects one or more bindings"
		     (pos $startpos($1) $endpos($3)) }
| LB RB
     { parsing_error "let expects one or more bindings"
		     (pos $startpos($1) $endpos($2)) }


opt_separated_list(sep, X):
| x = X ; sep? { [x] }
| x = X ; sep ; l = opt_separated_list(sep, X) { x :: l }


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
    Some (expr_list (pos $startpos(s) $endpos(s)) l)
  }
| b = BOOL { Some (ann $startpos(b) $endpos(b) (Const (CBool b))) }
