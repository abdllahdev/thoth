open Ast
open Core

let string_of_loc loc =
  Fmt.str "Line:%d Position:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

module ModelPrinter = struct
  let string_of_field_attr_arg (arg : Model.field_attr_arg) =
    match arg with
    | Model.AttrArgString (_, str) -> Fmt.str "\"%s\"" str
    | Model.AttrArgBoolean (_, boolean) -> Fmt.str "%b" boolean
    | Model.AttrArgRefList (_, list) ->
        Fmt.str "[%s]" (String.concat ~sep:", " list)
    | Model.AttrArgFunc (_, func) -> Fmt.str "%s()" func
    | Model.AttrArgNumber (_, num) -> Fmt.str "%d" num

  let rec string_of_field_attr_args (args : Model.field_attr_arg list) : string
      =
    match args with
    | [] -> ""
    | [ arg ] -> string_of_field_attr_arg arg
    | arg :: args ->
        string_of_field_attr_arg arg ^ ", " ^ string_of_field_attr_args args

  let string_of_field_attr (attr : Model.field_attr) : string =
    match attr with
    | Model.AttributeNoArgs (_, id) -> Fmt.str "%s" id
    | Model.AttributeWithArgs (_, id, args) ->
        Fmt.str "%s(%s)" id (string_of_field_attr_args args)

  let rec string_of_field_attrs (attrs : Model.field_attr list) : string =
    match attrs with
    | [] -> ""
    | [ attr ] -> string_of_field_attr attr
    | attr :: attrs ->
        string_of_field_attr attr ^ ", " ^ string_of_field_attrs attrs

  let string_of_field_type_modifier (modifier : Model.field_type_modifier) :
      string =
    match modifier with Model.List -> "[]" | Model.Optional -> "?"

  let string_of_field_type (field_type : Model.field_type) : string =
    match field_type with
    | Model.String _ -> "String"
    | Model.Int _ -> "Int"
    | Model.Json _ -> "Json"
    | Model.Boolean _ -> "Boolean"
    | Model.Float _ -> "Float"
    | Model.Decimal _ -> "Decimal"
    | Model.DateTime _ -> "DateTime"
    | Model.BigInt _ -> "BigInt"
    | Model.Bytes _ -> "Bytes"
    | Model.Custom (_, typ) -> typ

  let string_of_field (field : Model.field) : string =
    match field with
    | Model.FieldNoModifierNoAttrs (_, id, field_type) ->
        Fmt.str "\"%s\", %s" id (string_of_field_type field_type)
    | Model.FieldWithModifierNoAttrs (_, id, field_type, modifier) ->
        Fmt.str "\"%s\", %s%s" id
          (string_of_field_type field_type)
          (string_of_field_type_modifier modifier)
    | Model.FieldNoModifierWithAttrs (_, id, field_type, attrs) ->
        Fmt.str "\"%s\", %s, %s" id
          (string_of_field_type field_type)
          (string_of_field_attrs attrs)
    | Model.FieldWithModifierWithAttrs (_, id, field_type, modifier, attrs) ->
        Fmt.str "\"%s\", %s%s, %s" id
          (string_of_field_type field_type)
          (string_of_field_type_modifier modifier)
          (string_of_field_attrs attrs)

  let rec string_of_fields (fields : Model.field list) : string =
    match fields with
    | [] -> ""
    | [ field ] -> Fmt.str "\n      Field(%s)" (string_of_field field)
    | field :: fields ->
        Fmt.str "\n      Field(%s)" (string_of_field field)
        ^ string_of_fields fields
end

let string_of_declaration (declaration : declaration) : string =
  match declaration with
  | Model (_, model_id, model_fields) ->
      Fmt.str "  Model(\n    \"%s\",\n    [%s\n    ]\n  )" model_id
        (ModelPrinter.string_of_fields model_fields)

let rec string_of_declarations (declarations : declaration list) : string =
  match declarations with
  | [] -> ""
  | [ declaration ] -> string_of_declaration declaration
  | declaration :: declarations ->
      string_of_declaration declaration
      ^ ",\n"
      ^ string_of_declarations declarations

let string_of_program (Program declarations) : string =
  Fmt.str "Program(\n%s\n)" (string_of_declarations declarations)
