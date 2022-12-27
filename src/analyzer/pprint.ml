open Ast
open Core

module ModelPrinter = struct
  let field_attr_arg_to_string (arg : Model.field_attr_arg) =
    match arg with
    | Model.AttrArgString str -> Fmt.str "%s" str
    | Model.AttrArgBoolean boolean -> Fmt.str "%b" boolean
    | Model.AttrArgRefList list -> Fmt.str "[%s]" (String.concat ~sep:", " list)
    | Model.AttrArgFunc func -> Fmt.str "%s()" func
    | Model.AttrArgNumber num -> Fmt.str "%d" num

  let rec field_attr_args_to_string (args : Model.field_attr_arg list) : string
      =
    match args with
    | [] -> ""
    | arg :: args ->
        field_attr_arg_to_string arg ^ ", " ^ field_attr_args_to_string args

  let field_attr_to_string (attr : Model.field_attr) : string =
    match attr with
    | Model.AttributeNoArgs id -> Fmt.str "%s, " id
    | Model.AttributeWithArgs (id, args) ->
        Fmt.str "%s(%s), " id (field_attr_args_to_string args)

  let rec field_attrs_to_string (attrs : Model.field_attr list) : string =
    match attrs with
    | [] -> ""
    | attr :: attrs -> field_attr_to_string attr ^ field_attrs_to_string attrs

  let field_type_modifier_to_string (modifier : Model.field_type_modifier) :
      string =
    match modifier with Model.List -> "List" | Model.Optional -> "Optional"

  let field_type_to_string (field_type : Model.field_type) : string =
    match field_type with
    | Model.String -> "String"
    | Model.Int -> "Int"
    | Model.Json -> "Json"
    | Model.Boolean -> "Boolean"
    | Model.Float -> "Float"
    | Model.Decimal -> "Decimal"
    | Model.DateTime -> "DateTime"
    | Model.BigInt -> "BigInt"
    | Model.Bytes -> "Bytes"
    | Model.Custom typ -> typ

  let field_to_string (field : Model.field) : string =
    match field with
    | Model.FieldNoModiferNoAttrs (id, field_type) ->
        Fmt.str "%s(Type(%s))" id (field_type_to_string field_type)
    | Model.FieldWithModifierNoAttrs (id, field_type, modifier) ->
        Fmt.str "%s(Type(%s), Modifier(%s))" id
          (field_type_to_string field_type)
          (field_type_modifier_to_string modifier)
    | Model.FieldNoModiferWithAttrs (id, field_type, attrs) ->
        Fmt.str "%s(Type(%s), Attr(%s))" id
          (field_type_to_string field_type)
          (field_attrs_to_string attrs)
    | Model.FieldWithModiferWithAttrs (id, field_type, modifier, attrs) ->
        Fmt.str "%s(Type(%s), Modifier(%s), Attr(%s))" id
          (field_type_to_string field_type)
          (field_type_modifier_to_string modifier)
          (field_attrs_to_string attrs)

  let rec fields_to_string (fields : Model.field list) : string =
    match fields with
    | [] -> ""
    | field :: fields ->
        Fmt.str "   %s\n" (field_to_string field) ^ fields_to_string fields

  let fields_to_string (fields : Model.fields) : string =
    match fields with
    | Model.Fields fields -> Fmt.str "\n%s" (fields_to_string fields)

  let declaration_to_string (definition : Model.declaration) : string =
    match definition with
    | Model.Declaration (id, fields) ->
        Fmt.str "model %s {%s}\n" id (fields_to_string fields)
end

let declaration_to_string (declaration : declaration) : string =
  match declaration with
  | Model declaration -> ModelPrinter.declaration_to_string declaration

let rec program_to_string (Program program) : string =
  match program with
  | [] -> ""
  | definition :: definitions ->
      declaration_to_string definition ^ program_to_string (Program definitions)
