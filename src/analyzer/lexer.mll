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

(* Define helper regexes *)
let digit = ['0'-'9']
let alpha = ['a'-'z' 'A'-'Z']

let whitespace = [' ' '\t']+
let newline = '\r' | '\n' | "\r\n"
let id = (alpha) (alpha|digit|'_')*

rule read =
  parse
  | '{'         { LEFT_BRACE }
  | '}'         { RIGHT_BRACE }
  | "="         { EQUAL }
  | "model"     { MODEL }
  | "Psl"       { read_psl (Buffer.create 32) lexbuf }
  | id          { ID (Lexing.lexeme lexbuf) }
  | whitespace  { read lexbuf }
  | newline     { next_line lexbuf; read lexbuf }
  | eof         { EOF }
  | _           { raise (SyntaxError ("Unexpected char: " ^ Lexing.lexeme lexbuf)) }

and read_psl buf =
  parse
  | '{'         { LEFT_BRACE }
  | '}'         { PSL_BLOCK (Buffer.contents buf) }
  | _           { Buffer.add_string buf (Lexing.lexeme lexbuf); read_psl buf lexbuf }
  | eof         { raise (SyntaxError ("Missing '}'")) }
