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
rule token =
  parse
  | '{'             { LEFT_BRACE }
  | '}'             { RIGHT_BRACE }
  | '('             { LEFT_PARAN }
  | ')'             { RIGHT_PARAN }
  | '/'             { SLASH }
  | '?'             { QUESTION_MARK }
  | '<'             { LT }
  | '>'             { GT }
  | '='             { EQUAL }
  | '!'             { NOT }
  | "=="            { EQ }
  | "!="            { NOT_EQ }
  | "<="            { LT_OR_EQ }
  | ">="            { GT_OR_EQ }
  | ':'             { COLON }
  | ';'             { SEMICOLON }
  | ','             { COMMA }
  | '.'             { DOT }
  | '"'             { read_string (Buffer.create 17) lexbuf }
  | "[]"            { LIST_MODIFIER }
  | "</"            { CLOSING_TAG }
  | "<>"            { FRAGMENT_OPENING }
  | "</>"           { FRAGMENT_CLOSING }
  | "true"          { TRUE }
  | "false"         { FALSE }
  | "if"            { IF }
  | "then"          { THEN }
  | "else"          { ELSE }
  | "for"           { FOR }
  | "in"            { IN }
  | "model"         { MODEL }
  | "query"         { QUERY }
  | "where"         { WHERE }
  | "data"          { DATA }
  | "search"        { SEARCH }
  | "component"     { COMPONENT }
  | "page"          { PAGE }
  | "render"        { RENDER }
  | "let"           { LET }
  | "now"           { NOW }
  | "findMany"      { FIND_MANY }
  | "findUnique"    { FIND_UNIQUE }
  | "create"        { CREATE }
  | "update"        { UPDATE }
  | "delete"        { DELETE }
  | "as"            { AS }
  | "onError"       { ON_ERROR }
  | "onLoading"     { ON_LOADING }
  | "onSuccess"     { ON_SUCCESS }
  | "formFields"    { FORM_FIELDS }
  | "TextField"     { TEXT_FIELD }
  | "EmailField"    { EMAIL_FIELD }
  | "PasswordField" { PASSWORD_FIELD }
  | "NumberField"   { NUMBER_FIELD }
  | "name"          { NAME }
  | "type"          { TYPE }
  | "isVisible"     { IS_VISIBLE }
  | "style"         { STYLE }
  | "defaultValue"  { DEFAULT_VALUE }
  | "formButton"    { FORM_BUTTON }
  | "@on"           { ON }
  | "@permission"   { PERMISSION }
  | "@at"           { AT }
  | attribute       { ATTRIBUTE (Lexing.lexeme lexbuf) }
  | int             { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id              { ID (Lexing.lexeme lexbuf) }
  | whitespace      { token lexbuf }
  | newline         { next_line lexbuf; token lexbuf }
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
