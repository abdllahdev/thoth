open Specs.App_specs
open Db_generator

let generate_app (app_specs : app_specs) : string =
  let { db } = app_specs in
  generate_db db
