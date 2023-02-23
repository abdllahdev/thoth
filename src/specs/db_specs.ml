open Core
open Ast.Ast_types
open Ast.Formatter

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

let generate_attr_arg_specs arg =
  match arg with
  | Model.AttrArgLiteral literal -> string_of_literal literal
  | Model.AttrArgRef (_, id) -> Fmt.str "[%s]" id
  | Model.AttrArgNow _ -> Fmt.str "now()"

let generate_attr_specs (Model.Attribute (_, id, args)) =
  if List.length args > 0 then
    match id with
    | "@relation" ->
        Fmt.str "%s(%s)" id
          (String.concat ~sep:", "
             [
               Fmt.str "fields: %s"
                 (generate_attr_arg_specs (List.nth_exn args 0));
               Fmt.str "references: %s"
                 (generate_attr_arg_specs (List.nth_exn args 1));
             ])
    | "@default" ->
        Fmt.str "%s(%s)" id
          (String.concat ~sep:", " (List.map args ~f:generate_attr_arg_specs))
    | _ -> ""
  else
    match id with
    | "@id" -> Fmt.str "%s @default(autoincrement())" id
    | _ -> Fmt.str "%s" id

let generate_attrs_specs field_attrs =
  String.concat ~sep:" " (List.map field_attrs ~f:generate_attr_specs)

let generate_field_specs (Model.Field (_, id, field_type, field_attrs)) =
  let field_type = string_of_type field_type in
  let field_attrs = generate_attrs_specs field_attrs in
  { id; field_type; field_attrs }

let generate_model_specs model =
  let _, id, body = model in
  let body = List.map body ~f:generate_field_specs in
  { id; body }

let generate_db_specs models =
  let models = List.map models ~f:generate_model_specs in
  { models }
