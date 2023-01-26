open Sys
open Core
open Jingoo
open Specs.Db_specs
open File_generator

let generate_field (field_specs : field_specs) : Jg_types.tvalue =
  let { id; field_type; field_attrs } = field_specs in
  Jg_types.Tobj
    [
      ("id", Jg_types.Tstr id);
      ("type", Jg_types.Tstr field_type);
      ("attrs", Jg_types.Tstr field_attrs);
    ]

let generate_model (model_specs : model_specs) : Jg_types.tvalue =
  let { id; body } = model_specs in
  Jg_types.Tobj
    [
      ("id", Jg_types.Tstr id);
      ("fields", Jg_types.Tlist (List.map ~f:generate_field body));
    ]

let generate_db (db_specs : db_specs) : unit =
  let { models } = db_specs in
  let models_template = getcwd () ^ "/templates/db/schema.prisma" in
  let models_code =
    Jg_template.from_file models_template
      ~models:[ ("models", Jg_types.Tlist (List.map ~f:generate_model models)) ]
  in
  let models_file = getcwd () ^ "/.out/server/prisma/schema.prisma" in
  write_file models_file models_code
