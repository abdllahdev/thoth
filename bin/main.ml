open Sys
open Core

let parse_with_error (lexbuf : Lexing.lexbuf) =
  try Ok (Parsing.Parser.ast Parsing.Lexer.token lexbuf)
  with Parsing.Parser.Error ->
    Error_handler.Handler.raise_syntax_error
      (Ast.Pprinter.string_of_loc lexbuf.lex_curr_p)
      (Lexing.lexeme lexbuf)

let parse_file (filename : string) =
  let file_content = In_channel.read_all filename in
  let lexbuf = Lexing.from_string file_content in
  parse_with_error lexbuf

let () =
  let filename = getcwd () ^ "/examples/test.ra" in
  print_string (Fmt.str "Parsing %s\n" filename);
  match parse_file filename with
  | Ok ast ->
      Type_checker.run_type_checker ast;
      let app_specs = Specs.App_specs.generate_app_specs ast in
      Generator.App_generator.generate_app app_specs
  | Error error -> print_string (Core.Error.to_string_hum error)
