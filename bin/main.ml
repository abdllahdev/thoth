open Sys
open Core

let parse_with_error (lexbuf : Lexing.lexbuf) =
  try Ok (Parsing.Parser.ast Parsing.Lexer.token lexbuf)
  with Parsing.Parser.Error ->
    Error_handler.Handler.raise_syntax_error lexbuf.lex_curr_p
      (Lexing.lexeme lexbuf)

let parse_file (filename : string) =
  let file_content = In_channel.read_all filename in
  let lexbuf = Lexing.from_string file_content in
  parse_with_error lexbuf

let () =
  let filename = getcwd () ^ "/examples/todo.ra" in
  Fmt.str "Parsing %s\n" filename |> print_string;
  match parse_file filename with
  | Ok ast ->
      let global_env = Type_checker.Environment.GlobalEnvironment.create () in
      Type_checker.Checker.run global_env ast;
      Specs.App_specs.generate_app_specs global_env ast
      |> Generator.App_generator.generate_app
  | Error error -> Core.Error.to_string_hum error |> print_string
