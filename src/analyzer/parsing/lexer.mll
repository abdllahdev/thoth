{
  open Lexing
  open Parser
  open Error_handler.Handler

  let next_line lexbuf =
    let pos = lexbuf.lex_curr_p in
    lexbuf.lex_curr_p <-
      { pos with pos_bol = lexbuf.lex_curr_pos;
                pos_lnum = pos.pos_lnum + 1
      }
}

(* Helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

(* Regexes for tokens *)
let int = '-'? digit+
let id = (alpha)(alpha|digit|'_')*
let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let attribute = ('@')(id)

(* Lexer rules *)
rule read_token =
  parse
  | '{'             { LEFT_BRACE }
  | '}'             { RIGHT_BRACE }
  | '['             { LEFT_BRACKET }
  | ']'             { RIGHT_BRACKET }
  | '('             { LEFT_PARAN }
  | ')'             { RIGHT_PARAN }
  | '/'             { SLASH }
  | '?'             { QUESTION_MARK }
  | '<'             { LT }
  | '>'             { GT }
  | '='             { EQUAL }
  | "=="            { EQ }
  | "!="            { NOT_EQ }
  | "<="            { LT_OR_EQ }
  | ">="            { GT_OR_EQ }
  | ':'             { COLON }
  | ';'             { SEMICOLON }
  | ','             { COMMA }
  | '.'             { DOT }
  | "[|"            { read_typescript (Buffer.create 17) lexbuf }
  | '"'             { read_string (Buffer.create 17) lexbuf }
  | "[]"            { LIST_MODIFIER }
  | "</"            { CLOSING_TAG }
  | "<>"            { FRAGMENT_OPENING }
  | "</>"           { FRAGMENT_CLOSING }
  | "[%"            { EXPRESSION_OPENING }
  | "%]"            { EXPRESSION_CLOSING }
  | "//"            { read_single_line_comment lexbuf }
  | "/*"            { read_multi_line_comment lexbuf }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "not"           { NOT }
  | "and"           { AND }
  | "or"            { OR }
  | "connect"       { CONNECT }
  | "with"          { WITH }
  | "if"            { IF }
  | "else"          { ELSE }
  | "endif"         { ENDIF }
  | "for"           { FOR }
  | "endfor"        { ENDFOR }
  | "in"            { IN }
  | "model"         { MODEL }
  | "query"         { QUERY }
  | "component"     { COMPONENT }
  | "page"          { PAGE }
  | "app"           { APP }
  | "render"        { RENDER }
  | "let"           { LET }
  | "Now"           { NOW }
  | "Custom"        { CUSTOM }
  | "FindMany"      { FIND_MANY }
  | "FindUnique"    { FIND_UNIQUE }
  | "Create"        { CREATE }
  | "Update"        { UPDATE }
  | "Delete"        { DELETE }
  | "SignupForm"    { SIGNUP_FORM }
  | "LoginForm"     { LOGIN_FORM }
  | "LogoutButton"  { LOGOUT_BUTTON }
  | "type"          { TYPE }
  | "as"            { AS }
  | attribute       { ATTRIBUTE (Lexing.lexeme lexbuf) }
  | int             { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id              { ID (Lexing.lexeme lexbuf) }
  | whitespace      { read_token lexbuf }
  | newline         { next_line lexbuf; read_token lexbuf }
  | eof             { EOF }
  | _               { raise_syntax_error lexbuf.lex_curr_p (Lexing.lexeme lexbuf) }

and read_string buf = parse
  | '"'           { STRING (Buffer.contents buf) }
  | '\\' '/'      { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'     { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'      { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'      { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'      { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'      { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'      { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+ { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _             { raise_syntax_error lexbuf.lex_curr_p (Lexing.lexeme lexbuf) }
  | eof           { raise_syntax_error lexbuf.lex_curr_p (Lexing.lexeme lexbuf) }

and read_single_line_comment = parse
  | newline { next_line lexbuf; read_token lexbuf }
  | eof     { EOF }
  | _       { read_single_line_comment lexbuf }

and read_multi_line_comment = parse
  | "*/"    { read_token lexbuf }
  | newline { next_line lexbuf; read_multi_line_comment lexbuf }
  | eof     { raise_syntax_error lexbuf.lex_curr_p (Lexing.lexeme lexbuf) }
  | _       { read_multi_line_comment lexbuf }

and read_typescript buf = parse
  | "|]"    { TYPESCRIPT (Buffer.contents buf) }
  | _       { Buffer.add_string buf (Lexing.lexeme lexbuf); read_typescript buf lexbuf }
  | eof     { EOF }
