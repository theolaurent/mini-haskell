(* Lexer for Mini-Haskell *)
{

  open Parser

  (* The keywords of the language *)
  let keywords = Hashtbl.create 23
  let () =
    List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
              [ ("else",   ELSE)   ;
                ("if",     IF)     ;
                ("in",     IN)     ;
                ("let",    LET)    ;
                ("case",   CASE)   ;
                ("then",   THEN)   ;
                ("return", RETURN) ;
                ("do",     DO)     ]
}

let digit = ['0'-'9']
let ident = ['a'-'z'] ([ 'a'-'z'] | ['A'-'Z'] | '_' | '\'' | ['0'-'9' ])*
let car = (['\032' - '\126'] # ['\\' '"']) | "\\\\" | "\\\"" | "\\n" | "\\t"

rule token = parse
  | [' ' '\t' '\r']      { token lexbuf }
  | '\n'                 { failwith "TODO" }
  | '\n' (ident as str)  { failwith "TODO" ; ID0 str }
  | digit* as str        { INT (int_of_string str) }
  | ident as str         { try Hashtbl.find keywords str
                           with Not_found -> ID str }
  | "--"                 { lcomment lexbuf }
  | '\''                 { char lexbuf }
  | "||"                 { OR }
  | "&&"                 { AND }
  | '<'                  { LE }
  | "<="                 { LEQ }
  | '>'                  { GE }
  | ">="                 { GEQ }
  | "=="                 { EQ }
  | "/="                 { NEQ }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '*'                  { MULT }
  | '/'                  { DIV }
  | '('                  { LP }
  | ')'                  { RP }
  | '{'                  { LB }
  | '}'                  { RB }
  | '['                  { LBK }
  | ']'                  { RBK }
  | ':'                  { COL }
  | ';'                  { SCOL }
  | ','                  { COM }
  | '\\'                 { BSLASH }
  | "->"                 { ARR }
  | eof                  { EOF }
  | _ as c               { UNKNOWN c }

and char = parse
  | (car as str) '\''      { failwith "TODO" ; CHAR '0' }

and string = parse
  | (car* as str) '"'    { STR str }

and lcomment = parse
  | '\n'                 { failwith "TODO" }
  | _                    { lcomment lexbuf  }
