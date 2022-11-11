open Analyzer
open Core

let print_error_position (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) =
  try Ok (Parser.program Lexer.token lexbuf) with
  | Lexer.SyntaxError msg ->
      let error_msg = Fmt.str "%s: %s@." (print_error_position lexbuf) msg in
      Error (Error.of_string error_msg)
  | Parser.Error ->
      let error_msg =
        Fmt.str "%s: Syntax error@." (print_error_position lexbuf)
      in
      Error (Error.of_string error_msg)

let parse_file (filename : string) =
  let file_content = In_channel.read_all filename in
  let lexbuf = Lexing.from_string file_content in
  parse_with_error lexbuf

let () =
  print_string "\nStarted parsing the program\n";
  let filename = "test.ra" in
  match parse_file filename with
  | Ok program -> print_string (Pprint.program_to_string program)
  | Error error -> print_string (Error.to_string_hum error)
