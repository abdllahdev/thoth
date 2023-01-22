open Ast.Ast_types
open Ast.Filtered_ast
open Db_specs

type app_specs = { db : db_specs }

let generate_app_specs (Ast declarations) : app_specs =
  let { model_declarations; _ } = get_filtered_ast declarations in
  let db = generate_db_specs model_declarations in
  { db }
