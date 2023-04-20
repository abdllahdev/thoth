open Sys
open Core
open Cmdliner

let parse_with_error (lexbuf : Lexing.lexbuf) =
  try Ok (Parsing.Parser.ast Parsing.Lexer.read_token lexbuf)
  with Parsing.Parser.Error ->
    Error_handler.Handler.raise_syntax_error lexbuf.lex_curr_p
      (Lexing.lexeme lexbuf)

let parse_file (filename : string) =
  let file_content = In_channel.read_all filename in
  let lexbuf = Lexing.from_string file_content in
  parse_with_error lexbuf

let input_file =
  let doc = "Input file" in
  Arg.(required & pos 0 (some string) None & info [] ~docv:"INPUT_FILE" ~doc)

let output_dir =
  let doc = "Output directory" in
  Arg.(
    value & opt string ".out"
    & info [ "o"; "output_dir" ] ~docv:"OUTPUT_DIR" ~doc)

let db_name =
  let doc = "Database name" in
  Arg.(required & pos 1 (some string) None & info [] ~docv:"DB_NAME" ~doc)

let server_port =
  let doc = "Server port" in
  Arg.(value & opt int 4000 & info [ "server_port" ] ~docv:"SERVER_PORT" ~doc)

let main input_file db_name output_dir server_port =
  let input_file = Fmt.str "%s/%s" (getcwd ()) input_file in
  Fmt.str "Parsing %s\n" input_file |> print_string;
  match parse_file input_file with
  | Ok ast ->
      let global_env = Type_checker.Environment.GlobalEnvironment.create () in
      Type_checker.Checker.run global_env ast;
      let app_specs = Specs.App_specs.generate_app_specs global_env ast in
      Generator.App_generator.generate_app app_specs output_dir db_name
        server_port
  | Error error -> Core.Error.to_string_hum error |> print_string

let cmd =
  let doc = "thoth" in
  let man = [ `S "DESCRIPTION"; `P "Multitier web language" ] in
  Cmd.v
    (Cmd.info "Thoth" ~version:"1.0" ~doc ~man)
    Term.(const main $ input_file $ db_name $ output_dir $ server_port)

let () = exit (Cmd.eval cmd)
