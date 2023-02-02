open Ast.Ast_types
open Ast.Filtered_ast
open Db_specs
open Server_specs

type app_specs = { db : db_specs; server : server_specs }

let generate_app_specs (Ast declarations) =
  let { model_declarations; query_declarations; _ } =
    get_filtered_ast declarations
  in
  let db = generate_db_specs model_declarations in
  let server = generate_server_specs query_declarations in
  { db; server }
