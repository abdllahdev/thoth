open Core

let print_error_position (lexbuf : Lexing.lexbuf) =
  let pos = lexbuf.lex_curr_p in
  Fmt.str "Line:%d Position:%d" pos.pos_lnum (pos.pos_cnum - pos.pos_bol + 1)

let parse_with_error (lexbuf : Lexing.lexbuf) =
  try Ok (Parsing.Parser.ast Parsing.Lexer.token lexbuf) with
  | Error_handler.Errors.SyntaxError msg ->
      let error_msg = Fmt.str "%s: %s@." (print_error_position lexbuf) msg in
      Error (Core.Error.of_string error_msg)
  | Parsing.Parser.Error ->
      let error_msg =
        Fmt.str "%s: Syntax error@." (print_error_position lexbuf)
      in
      Error (Core.Error.of_string error_msg)

let parse_file (filename : string) =
  let file_content = In_channel.read_all filename in
  let lexbuf = Lexing.from_string file_content in
  parse_with_error lexbuf

let () =
  let filename = "./examples/test.ra" in
  print_string (Fmt.str "Parsing %s\n" filename);
  match parse_file filename with
  | Ok ast ->
      Type_checker.run_type_checker ast;
      print_string (Ast.Pprinter.string_of_ast ast)
      (* let db_specs = Db_specs.generate_db_specs ast in
         print_string (Db_generator.generate_db db_specs) *)
  | Error error -> print_string (Core.Error.to_string_hum error)
