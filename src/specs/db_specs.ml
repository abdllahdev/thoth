open Core
open Ast.Ast_types
open Ast.Pprinter

type id = string
type field_specs = { id : string; field_type : string; field_attrs : string }
type model_specs = { id : string; body : field_specs list }

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
  | AttrArgString (_, str) -> string_of_literal str
  | AttrArgRef (_, id) -> Fmt.str "[%s]" id
  | AttrArgBoolean (_, boolean) -> string_of_literal boolean
  | AttrArgInt (_, number) -> string_of_literal number
  | AttrArgNow _ -> Fmt.str "now()"

let generate_attr_specs (Model.Attribute (_, id, args)) : string =
  if List.length args > 0 then
    match id with
    | "@relatoin" ->
        Fmt.str "%s(%s)" id
          (String.concat ~sep:", " (List.map ~f:generate_attr_arg_specs args))
    | "@default" ->
        Fmt.str "%s(%s)" id
          (String.concat ~sep:", " (List.map ~f:generate_attr_arg_specs args))
    | _ -> ""
  else
    match id with
    | "@id" -> Fmt.str "%s @default(autoincrement())" id
    | _ -> Fmt.str "%s" id

let generate_attrs_specs (field_attrs : Model.attribute list) : string =
  String.concat ~sep:" " (List.map ~f:generate_attr_specs field_attrs)

let generate_field_type_specs (field_type : typ) : string =
  match field_type with
  | Scalar scalar_type -> string_of_scalar_type scalar_type
  | Composite composite_type -> string_of_composite_type composite_type

let generate_field_specs (Model.Field (_, id, field_type, field_attrs)) :
    field_specs =
  let field_type = generate_field_type_specs field_type in
  let field_attrs = generate_attrs_specs field_attrs in
  { id; field_type; field_attrs }

let generate_model_specs (model : model_declaration) : model_specs =
  let _, id, body = model in
  let body = List.map ~f:generate_field_specs body in
  { id; body }

let generate_db_specs (models : model_declaration list) : db_specs =
  let models = List.map ~f:generate_model_specs models in
  { models }
