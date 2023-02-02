open Specs.App_specs
open Db_generator
open Server_generator

let generate_app app_specs =
  let { db; server } = app_specs in
  generate_server server;
  generate_db db
