open Specs.App_specs
open Db_generator
open Server_generator
open Ui_generator

let generate_app (app_specs : app_specs) : unit =
  let { db; server; ui } = app_specs in
  generate_server server;
  generate_db db;
  generate_ui ui
