open Ast
open Core

let string_of_loc loc =
  Fmt.str "Line:%d Position:%d" loc.Lexing.pos_lnum
    (loc.Lexing.pos_cnum - loc.Lexing.pos_bol + 1)

let string_of_scalar_type (scalar_type : scalar_type) : string =
  match scalar_type with
  | String -> "String"
  | Int -> "Int"
  | Json -> "Json"
  | Boolean -> "Boolean"
  | DateTime -> "DateTime"
  | Bytes -> "Bytes"
  | CustomType custom_type -> custom_type

let string_of_composite_type (composite_type : composite_type) : string =
  match composite_type with
  | List scalar_type -> Fmt.str "%s[]" (string_of_scalar_type scalar_type)
  | Optional scalar_type -> Fmt.str "%s?" (string_of_scalar_type scalar_type)
  | OptionalList scalar_type ->
      Fmt.str "%s[]?" (string_of_scalar_type scalar_type)

let string_of_literal (literal : literal) : string =
  match literal with
  | StringLiteral (_, str) -> str
  | BooleanLiteral (_, boolean) -> Fmt.str "%b" boolean
  | IntLiteral (_, num) -> Fmt.str "%d" num

module ModelPrinter = struct
  let string_of_field_attr_arg (arg : Model.attr_arg) =
    match arg with
    | Model.AttrArgString (_, str) -> Fmt.str "\"%s\"" (string_of_literal str)
    | Model.AttrArgBoolean (_, boolean) -> string_of_literal boolean
    | Model.AttrArgRef (_, ref) -> Fmt.str "%s" ref
    | Model.AttrArgFunc (_, func) -> Fmt.str "%s()" func
    | Model.AttrArgInt (_, num) -> string_of_literal num

  let rec string_of_field_attr_args (args : Model.attr_arg list) : string =
    match args with
    | [] -> ""
    | [ arg ] -> string_of_field_attr_arg arg
    | arg :: args ->
        string_of_field_attr_arg arg ^ ", " ^ string_of_field_attr_args args

  let string_of_field_attr (attr : Model.attribute) : string =
    match attr with
    | Model.Attribute (_, id, args) ->
        Fmt.str "%s(%s)" id (string_of_field_attr_args args)

  let rec string_of_field_attrs (attrs : Model.attribute list) : string =
    match attrs with
    | [] -> ""
    | [ attr ] -> string_of_field_attr attr
    | attr :: attrs ->
        string_of_field_attr attr ^ ", " ^ string_of_field_attrs attrs

  let string_of_field_type (field_type : typ) : string =
    match field_type with
    | Scalar scalar_type -> string_of_scalar_type scalar_type
    | Composite composite_type -> string_of_composite_type composite_type

  let string_of_field (field : Model.field) : string =
    match field with
    | Model.Field (_, id, field_type, attrs) ->
        if List.length attrs > 0 then
          Fmt.str "\"%s\", %s, %s" id
            (string_of_field_type field_type)
            (string_of_field_attrs attrs)
        else Fmt.str "\"%s\", %s" id (string_of_field_type field_type)

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
  | Model (_, id, body) ->
      Fmt.str "  Model(\n    \"%s\",\n    [%s\n    ]\n  )" id
        (ModelPrinter.string_of_fields body)
  | Query (_, _, _) -> ""

let rec string_of_declarations (declarations : declaration list) : string =
  match declarations with
  | [] -> ""
  | [ declaration ] -> string_of_declaration declaration
  | declaration :: declarations ->
      string_of_declaration declaration
      ^ ",\n"
      ^ string_of_declarations declarations

let string_of_ast (Ast declarations) : string =
  Fmt.str "Ast(\n%s\n)" (string_of_declarations declarations)
