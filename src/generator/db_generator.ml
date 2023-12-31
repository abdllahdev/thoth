open Core
open Core_unix
open Jingoo
open Specs.Db_specs
open File_generator

let generate_field field_specs =
  let { id; field_type; field_attrs } = field_specs in
  Jg_types.Tobj
    [
      ("id", Jg_types.Tstr id);
      ("type", Jg_types.Tstr field_type);
      ("attrs", Jg_types.Tstr field_attrs);
    ]

let generate_model model_specs =
  let { id; body } = model_specs in
  Jg_types.Tobj
    [
      ("id", Jg_types.Tstr id);
      ("fields", Jg_types.Tlist (List.map body ~f:generate_field));
    ]

let generate_db db_specs output_dir =
  let { models } = db_specs in
  let models_template = getcwd () ^ "/templates/db/schema.jinja" in
  let models_code =
    Jg_template.from_file models_template
      ~models:[ ("models", Jg_types.Tlist (List.map models ~f:generate_model)) ]
  in
  let models_file =
    Fmt.str "%s/%s/server/prisma/schema.prisma" (getcwd ()) output_dir
  in
  write_file models_file models_code
