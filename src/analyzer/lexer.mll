{
  open Lexing
  open Parser

  exception SyntaxError of string

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

(* Lexer rules *)
rule token =
  parse
  | '{'                    { LEFT_BRACE }
  | '}'                    { RIGHT_BRACE }
  | '['                    { LEFT_BRACKET }
  | ']'                    { RIGHT_BRACKET }
  | '('                    { LEFT_PARAN }
  | ')'                    { RIGHT_PARAN }
  | '='                    { EQUAL }
  | '?'                    { QUESTION_MARK }
  | ';'                    { SEMICOLON }
  | '"'                    { read_string (Buffer.create 17) lexbuf }
  | "true"                 { TRUE }
  | "false"                { FALSE }
  | "model"                { MODEL }
  | "@id"                  { MODEL_FIELD_ATTR_ID }
  | "@default"             { MODEL_FIELD_ATTR_DEFAULT }
  | "@unique"              { MODEL_FIELD_ATTR_UNIQUE }
  | "@relation"            { MODEL_FIELD_ATTR_RELATION }
  | "@updatedAt"           { MODEL_FIELD_ATTR_UPDATED_AT }
  | "@ignore"              { MODEL_FIELD_ATTR_IGNORE }
  | "optional"             { MODEL_FIELD_MODIFIER_OPTIONAL }
  | "list"                 { MODEL_FIELD_MODIFIER_LIST }
  | "autoincrement"        { MODEL_FIELD_ATTR_FUNC_AUTOINCREMENT }
  | "now"                  { MODEL_FIELD_ATTR_FUNC_NOW }
  | int                    { INT (int_of_string (Lexing.lexeme lexbuf))}
  | id                     { ID (Lexing.lexeme lexbuf) }
  | whitespace             { token lexbuf }
  | newline                { next_line lexbuf; token lexbuf }
  | eof                    { EOF }
  | _                      { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_string buf = parse
  | '"'             { STRING (Buffer.contents buf) }
  | '\\' '/'        { Buffer.add_char buf '/'; read_string buf lexbuf }
  | '\\' '\\'       { Buffer.add_char buf '\\'; read_string buf lexbuf }
  | '\\' 'b'        { Buffer.add_char buf '\b'; read_string buf lexbuf }
  | '\\' 'f'        { Buffer.add_char buf '\012'; read_string buf lexbuf }
  | '\\' 'n'        { Buffer.add_char buf '\n'; read_string buf lexbuf }
  | '\\' 'r'        { Buffer.add_char buf '\r'; read_string buf lexbuf }
  | '\\' 't'        { Buffer.add_char buf '\t'; read_string buf lexbuf }
  | [^ '"' '\\']+   { Buffer.add_string buf (Lexing.lexeme lexbuf); read_string buf lexbuf }
  | _               { raise (SyntaxError ("Illegal string character: " ^ Lexing.lexeme lexbuf)) }
  | eof             { raise (SyntaxError ("String is not terminated")) }
