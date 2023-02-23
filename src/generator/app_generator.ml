open Core_unix
open Specs.App_specs
open Db_generator
open Server_generator
open Client_generator

let generate_app app_specs =
  let { db; server; client } = app_specs in
  generate_db db;
  generate_server server;
  generate_client client;
  system (Fmt.str "cd %s/.out/client && yarn && yarn prettier" (getcwd ()))
  |> ignore;
  system (Fmt.str "cd %s/.out/server && yarn && yarn prettier" (getcwd ()))
  |> ignore
