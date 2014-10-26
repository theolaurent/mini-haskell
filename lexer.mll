(* Lexer for Mini-Haskell *)
{

  open Parser

  (* (msg,startpos,endpos) *)
  let lexing_error msg lexbuf =
    LEX_ERROR (msg, Lexing.lexeme_start_p lexbuf, Lexing.lexeme_end_p lexbuf)

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
  | '\n'                 { Lexing.new_line () ; token lexbuf }
  | digit+ as str        { INT (int_of_string str) }
  | ident as str         { try Hashtbl.find keywords str
                           with Not_found ->
				                     let pos = Lexing.lexeme_start_p in
				                     if pos.Lexing.cnum - pos.Lexing.bol = 0
				                     then ID0 str
				                     else ID str }
  | "True"               { BOOL true }
  | "False"              { BOOL false }
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
  | _                    { lexing_error "Unknown character" lexbuf }

and char = parse
  | (car as str) '\''    { CHAR str }
  | car [^ '\'']         { lexing_error "Too wide character literal" lexbuf
  | '\\' car             { lexing_error "Unknown escape sequence" lexbuf }
  | '\''                 { lexing_error "Empty character literal" lexbuf }
  | _                    { lexing_error "Unknown character" lexbuf }
                         (* is this error message relevant ? cf \n *)

(* multi-line strings do not seems to be accepted, is it supposed that way or is it a mistake ? *)
and string = parse
  | (car* as str) '"'    { STR str }
  | '\\' car             { lexing_error "Unknown escape sequence" lexbuf }
  | _                    { lexing_error "Unknown character" lexbuf }

and lcomment = parse
  | '\n'                 { Lexing.new_line () ; token lexbuf }
  | _                    { lcomment lexbuf  }
