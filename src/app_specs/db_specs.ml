open Analyzer.Ast

type id = string
type field_specs = { id : string; field_type : string; field_attrs : string }
type model_specs = { id : string; fields : field_specs list }

type db_config = {
  user : string;
  password : string;
  host : string;
  name : string;
}

type db_specs = { models : model_specs list }

let generate_attr_arg_specs (arg : Model.field_attr_arg) : string =
  match arg with
  | AttrArgString (_, str) -> Fmt.str "%s" str
  | AttrArgRef (_, id) -> id
  | AttrArgBoolean (_, boolean) -> Fmt.str "%b" boolean
  | AttrArgNumber (_, number) -> Fmt.str "%d" number
  | AttrArgFunc (_, func) -> Fmt.str "%s()" func

let generate_attr_specs (Model.Attribute (_, id, args)) : string =
  if List.length args > 0 then
    Fmt.str "%s(%s)" id
      (String.concat ", " (List.map generate_attr_arg_specs args))
  else Fmt.str "%s" id

let generate_attrs_specs (field_attrs : Model.field_attr list) : string =
  String.concat " " (List.map generate_attr_specs field_attrs)

let generate_field_type_specs (field_type : Model.field_type)
    (field_type_modifier : Model.field_type_modifier) : string =
  let field_type_modifier =
    match field_type_modifier with
    | Model.NoModifier -> ""
    | Model.List -> "[]"
    | Model.Optional -> "?"
  in

  match field_type with
  | Model.FieldTypeString -> Fmt.str "%s%s" "String" field_type_modifier
  | FieldTypeInt -> Fmt.str "%s%s" "Int" field_type_modifier
  | FieldTypeJson -> Fmt.str "%s%s" "Json" field_type_modifier
  | FieldTypeBoolean -> Fmt.str "%s%s" "Boolean" field_type_modifier
  | FieldTypeFloat -> Fmt.str "%s%s" "Float" field_type_modifier
  | FieldTypeDecimal -> Fmt.str "%s%s" "Decimal" field_type_modifier
  | FieldTypeDateTime -> Fmt.str "%s%s" "DateTime" field_type_modifier
  | FieldTypeBigInt -> Fmt.str "%s%s" "BigInt" field_type_modifier
  | FieldTypeBytes -> Fmt.str "%s%s" "Bytes" field_type_modifier
  | FieldTypeCustom (_, customType) ->
      Fmt.str "%s%s" customType field_type_modifier

let generate_field_specs
    (Model.Field (_, id, field_type, field_type_modifier, field_attrs)) :
    field_specs =
  let field_type = generate_field_type_specs field_type field_type_modifier in
  let field_attrs = generate_attrs_specs field_attrs in
  { id; field_type; field_attrs }

let generate_model_specs (Model (_, id, fields)) : model_specs =
  let fields = List.map generate_field_specs fields in
  { id; fields }

let generate_models_specs declarations =
  List.map generate_model_specs declarations

let generate_db_specs (Ast declarations) : db_specs =
  let models = generate_models_specs declarations in
  { models }
