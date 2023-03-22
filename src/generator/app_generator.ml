open Core_unix
open Specs.App_specs
open Db_generator
open Server_generator
open Client_generator
open File_generator

let generate_app app_specs output_dir db_name server_port =
  let server = getcwd () ^ "/templates/server" in
  let client = getcwd () ^ "/templates/client" in
  create_folder output_dir server;
  create_folder output_dir client;
  let { db; server; client } = app_specs in
  generate_db db output_dir;
  generate_server server output_dir server_port db_name;
  generate_client client output_dir server_port;
  system
    (Fmt.str "cd %s/%s/client && yarn && yarn prettier" (getcwd ()) output_dir)
  |> ignore;
  system
    (Fmt.str "cd %s/%s/server && yarn && yarn prettier" (getcwd ()) output_dir)
  |> ignore
