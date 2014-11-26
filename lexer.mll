(* Lexer for Mini-Haskell *)
{

  module Make (Err:Errors.S) (ParseErr:Errors.S) = struct

  module P = Parser.Make (Err) (ParseErr)
  open P

  let lexing_error msg lexbuf =
    Err.report ("lexing error: " ^ msg)
               (Lexing.lexeme_start_p lexbuf)
               (Lexing.lexeme_end_p lexbuf)


  let unescape_char = function
    | "\\\\" -> '\\'
    | "\\\"" -> '"'
    | "\\t"  -> '\t'
    | "\\n"  -> '\n'
    | s ->
      if String.length s = 1
      then s.[0]
      else assert false
	 
  let unescape l =
    let s = Bytes.make (List.length l) '\000' in
    let rec impl i l =
      match l with
	| [] -> ()
	| c :: l -> begin
	  Bytes.set s i (unescape_char c) ;
	  impl (pred i) l
	end
    in impl (List.length l - 1) l ;
    Bytes.to_string s

  (* The keywords of the language *)
  let keywords = Hashtbl.create 23
  let () =
    List.iter (fun (kwd, tok) -> Hashtbl.add keywords kwd tok)
              [ ("else",   ELSE)   ;
                ("if",     IF)     ;
                ("in",     IN)     ;
                ("let",    LET)    ;
                ("case",   CASE)   ;
                ("of",     OF)     ;
                ("then",   THEN)   ;
                ("return", RETURN) ;
                ("do",     DO)     ]
}

let digit = ['0'-'9']
let ident = ['a'-'z'] ([ 'a'-'z'] | ['A'-'Z'] | '_' | '\'' | ['0'-'9' ])*
let car = (['\032' - '\126'] # ['\\' '"']) | "\\\\" | "\\\"" | "\\n" | "\\t"

	      (*
let car = (['\032' - '\126'] # ['\\' '\'']) (* what about \' ?? *)
let car_str = (['\032' - '\126'] # ['\\' '"']) | "\\\\" | "\\\"" | "\\n" | "\\t" | "\'"
 *)
	      
rule token = parse
  | [' ' '\t' '\r']      { token lexbuf }
  | '\n'                 { Lexing.new_line lexbuf ; token lexbuf }
  | digit+ as str        { INT (int_of_string str) }
  | ident as str         { try Hashtbl.find keywords str
                           with Not_found ->
			     let pos = Lexing.lexeme_start_p lexbuf in
			     if pos.Lexing.pos_cnum - pos.Lexing.pos_bol = 0
			     then ID0 str
			     else ID str }
  | "True"               { BOOL true }
  | "False"              { BOOL false }
  | "--"                 { lcomment lexbuf }
  | '\''                 { char lexbuf } (* what about strings ? *)
  | '\"'                 { string [] lexbuf }
  | "||"                 { OR }
  | "&&"                 { AND }
  | "="                  { EQ }
  | '<'                  { LT }
  | "<="                 { LEQ }
  | '>'                  { GT }
  | ">="                 { GEQ }
  | "=="                 { EQEQ }
  | "/="                 { NEQ }
  | '+'                  { PLUS }
  | '-'                  { MINUS }
  | '*'                  { MULT }
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
  | _                    { lexing_error "Unknown character" lexbuf ; token lexbuf }

and char = parse
  | (car as c) '\''      { CHAR (unescape_char c) }
(*  | "\\\\'"             { CHAR '\\' }
  | "\\\"'"             { CHAR '\"' }
  | "\\''"              { CHAR '\'' }
  | "\\n'"              { CHAR '\n' }
  | "\\t'"              { CHAR '\t' }
 *)
  | '\n'                { lexing_error "Newline in char litteral" lexbuf ;
			  Lexing.new_line lexbuf ; char lexbuf } 
  | '\\' car '\''       { lexing_error "Unknown escape sequence" lexbuf ; CHAR '\000' }
  | car                 { lexing_error "Too many character literal" lexbuf ; char lexbuf }
  | '\''                { lexing_error "Empty character literal" lexbuf ; CHAR '\000' }
  | eof                 { lexing_error "Unterminated char" lexbuf ; CHAR '\000' }
  | _                   { lexing_error "Unknown character in character literal" lexbuf ; char lexbuf }


and string str = parse
  | '\"'           { STR (unescape str) }
  | (car (*_str*) as c) { string (c :: str) lexbuf }
  | '\n'           { lexing_error "Newline in string litteral" lexbuf ;
		     Lexing.new_line lexbuf ; string [] lexbuf
		   }
  | '\\'           { lexing_error "Unknown escape sequence" lexbuf ; 
		     string [] lexbuf }
  | eof            { lexing_error "Unterminated string" lexbuf ; 
		     STR "" }
  | _              { lexing_error "Unknown character in string literal" lexbuf ; 
		     string [] lexbuf }
      
and lcomment = parse
  | '\n'                 { Lexing.new_line lexbuf ; token lexbuf }
  | eof                  { lexing_error "Unterminated comment" lexbuf ; EOF }
  | _                    { lcomment lexbuf  }

			 
				    

{
  end
}
