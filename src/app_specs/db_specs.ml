open Analyzer.Ast
open General_specs

type id = string
type field_specs = { id : string; field_type : string; field_attrs : string }
type model_specs = { id : string; fields : field_specs list }

type db_config = {
  user : string;
  password : string;
  host : string;
  name : string;
}

(* TODO: add db config *)
type db_specs = { models : model_specs list }

let generate_attr_arg_specs (arg : Model.attr_arg) : string =
  match arg with
  | AttrArgString (_, str) -> generate_literals str
  | AttrArgRef (_, id) -> Fmt.str "[%s]" id
  | AttrArgBoolean (_, boolean) -> generate_literals boolean
  | AttrArgInt (_, number) -> generate_literals number
  | AttrArgFunc (_, func) -> Fmt.str "%s()" func

let generate_attr_specs (Model.Attribute (_, id, args)) : string =
  if List.length args > 0 then
    match id with
    | "@relatoin" ->
        Fmt.str "%s(%s)" id
          (String.concat ", " (List.map generate_attr_arg_specs args))
    | "@default" ->
        Fmt.str "%s(%s)" id
          (String.concat ", " (List.map generate_attr_arg_specs args))
    | _ -> ""
  else
    match id with
    | "@id" -> Fmt.str "%s @default(autoincrement())" id
    | _ -> Fmt.str "%s" id

let generate_attrs_specs (field_attrs : Model.attribute list) : string =
  String.concat " " (List.map generate_attr_specs field_attrs)

let generate_field_type_specs (field_type : typ) : string =
  match field_type with
  | Scalar scalar_type -> generate_scalar_type scalar_type
  | Composite composite_type -> generate_composite_type composite_type

let generate_field_specs (Model.Field (_, id, field_type, field_attrs)) :
    field_specs =
  let field_type = generate_field_type_specs field_type in
  let field_attrs = generate_attrs_specs field_attrs in
  { id; field_type; field_attrs }

(* let generate_model_specs (Model (_, id, fields)) : model_specs =
     let fields = List.map generate_field_specs fields in
     { id; fields }

   let generate_db_specs (Ast declarations) : db_specs =
        let models = List.map generate_model_specs declarations in
        { models }

      (* TODO: add queries *)
      type app_specs = { db : db_specs }

      let generate_app_specs (Ast declarations) : app_specs =
        match decl *)
