(* Parser for Mini-Haskell *)

%{
  open Ast
%}

%token <string> STR
%token <char> CHAR
%token <int> INT
%token <string> ID
%token <string> ID0
%token ELSE IF IN LET CASE THEN RETURN DO
%token LP RP LB RB LBK RBK
%token COL SCOL COM
%token BSLASH ARR
%token OR AND
%token LE LEQ GE GEQ EQ NEQ
%token PLUS MINUS MULT DIV
%token EOF
%token <char> UNKNOWN

%start <Ast.expr> main

%%

main:
| EOF { failwith "OK" }

def0:
