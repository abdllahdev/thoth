open Ast
open Core

module ModelPrinter = struct
  let field_attr_arg_to_string (arg : Model.field_attr_arg) : string =
    match arg with
    | Model.Argument value -> (
        match value with
        | Model.AttrArgString str -> Fmt.str "%s" str
        | Model.AttrArgBoolean boolean -> Fmt.str "%b" boolean
        | Model.AttrArgList refs ->
            Fmt.str "[%s]" (String.concat ~sep:", " refs)
        | Model.AttrArgFunc func -> ( match func with Now -> Fmt.str "now")
        | Model.AttrArgNumber num -> Fmt.str "%d" num)

  let field_attr_id_to_string (id : Model.field_attr_id) : string =
    match id with
    | Id -> Fmt.str "@id"
    | Unique -> Fmt.str "@unique"
    | Ignore -> Fmt.str "@ignore"
    | Default -> Fmt.str "@default"
    | UpdatedAt -> Fmt.str "@updatedAt"
    | Relation -> Fmt.str "@relation"

  let field_attr_to_string (attr : Model.field_attr) : string =
    match attr with
    | Model.AttributeNoArgs id -> Fmt.str "%s, " (field_attr_id_to_string id)
    | Model.AttributeWithArgs (id, arg) ->
        Fmt.str "%s(%s), "
          (field_attr_id_to_string id)
          (field_attr_arg_to_string arg)

  let rec field_attrs_to_string (attrs : Model.field_attr list) : string =
    match attrs with
    | [] -> ""
    | attr :: attrs -> field_attr_to_string attr ^ field_attrs_to_string attrs

  let field_type_modifier_to_string (modifier : Model.field_type_modifier) :
      string =
    match modifier with Model.List -> "List" | Model.Optional -> "Optional"

  let field_to_string (field : Model.field) : string =
    match field with
    | Model.FieldNoModiferNoAttrs (id, typ) -> Fmt.str "%s(Type(%s))" id typ
    | Model.FieldWithModifierNoAttrs (id, typ, modifier) ->
        Fmt.str "%s(Type(%s), Modifier(%s))" id typ
          (field_type_modifier_to_string modifier)
    | Model.FieldNoModiferWithAttrs (id, typ, attrs) ->
        Fmt.str "%s(Type(%s), Attr(%s))" id typ (field_attrs_to_string attrs)
    | Model.FieldWithModiferWithAttrs (id, typ, modifier, attrs) ->
        Fmt.str "%s(Type(%s), Modifier(%s), Attr(%s))" id typ
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
        Fmt.str "model %s = {%s}\n" id (fields_to_string fields)
end

let declaration_to_string (declaration : declaration) : string =
  match declaration with
  | Model declaration -> ModelPrinter.declaration_to_string declaration

let rec program_to_string (Program program) : string =
  match program with
  | [] -> ""
  | definition :: definitions ->
      declaration_to_string definition ^ program_to_string (Program definitions)
