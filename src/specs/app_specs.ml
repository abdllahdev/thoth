open Ast.Ast_types
open Ast.Filtered_ast
open Db_specs
open Server_specs
open Ui_specs

type app_specs = { db : db_specs; server : server_specs; ui : ui_specs }

let generate_app_specs (Ast declarations) : app_specs =
  let {
    model_declarations;
    query_declarations;
    component_declarations;
    page_declarations;
  } =
    get_filtered_ast declarations
  in
  let db = generate_db_specs model_declarations in
  let server = generate_server_specs query_declarations in
  let ui = generate_ui_specs page_declarations component_declarations in
  { db; server; ui }
