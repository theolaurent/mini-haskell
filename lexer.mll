(* Lexer for Mini-Haskell *)
{

  open Parser
	 
  (* (msg,startpos,endpos) *)
  exception LexingError of string * Lexing.position * Lexing.position 
  let lexing_error msg lexbuf =
    raise (LexingError (msg,
			Lexing.lexeme_start_p lexbuf,
			Lexing.lexeme_end_p lexbuf))
							
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
  | [' ' '\t' '\r' '\n']      { token lexbuf }
  | digit+ as str        { INT (int_of_string str) }
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
  | _                    { lexing_error "Unknown character" lexbuf }
(*  | _ as c               { UNKNOWN c } *)

and char = parse
  | (car as str) '\''    { CHAR str }
  | car [^ '\'']         { lexing_error "Too wide character literal" lexbuf
  | '\\' car             { lexing_error "Unknown escape sequence" lexbuf }
  | '\''                 { lexing_error "Empty character literal" lexbuf }
  | _                    { lexing_error "Unknown character" lexbuf }

and string = parse
  | (car* as str) '"'    { STR str }
  | '\\' car             { lexing_error "Unknown escape sequence" lexbuf }
  | _                    { lexing_error "Unknown character" lexbuf }
			 
and lcomment = parse
  | '\n'                 { token lexbuf }
  | _                    { lcomment lexbuf  }
