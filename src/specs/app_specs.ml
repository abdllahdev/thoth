open Ast.Ast_types
open Ast.Filtered_ast
open Db_specs
open Server_specs
open Client_specs

type app_specs = { db : db_specs; server : server_specs; client : client_specs }

let generate_app_specs global_env (app_declaration, declarations) =
  let {
    model_declarations;
    query_declarations;
    component_declarations;
    page_declarations;
  } =
    get_filtered_ast declarations
  in
  let db = generate_db_specs model_declarations in
  let server =
    generate_server_specs global_env app_declaration query_declarations
  in
  let client =
    generate_client_specs global_env app_declaration component_declarations
      page_declarations server
  in
  { db; server; client }
