open Core
open Ast
open Ast.Ast_types
open Error_handler.Handler
open Symbol_table

let get_custom_scalar_type (scalar_type : scalar_type) : string option =
  match scalar_type with CustomType str -> Some str | _ -> None

let get_custom_type (typ : typ) : string option =
  match typ with
  | Scalar scalar_type -> get_custom_scalar_type scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> get_custom_scalar_type scalar_type
      | Optional scalar_type -> get_custom_scalar_type scalar_type
      | OptionalList scalar_type -> get_custom_scalar_type scalar_type)

let extract_scalar_type (typ : typ) : scalar_type =
  match typ with
  | Scalar scalar_type -> scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> scalar_type
      | Optional scalar_type -> scalar_type
      | OptionalList scalar_type -> scalar_type)

let check_field_attr (global_table : 'a GlobalSymbolTable.t)
    (model_table : 'a LocalSymbolTable.t) (field_id : id)
    (Model.Attribute (loc, id, args)) : unit =
  let args_length = List.length args in
  match id with
  | "@id" | "@unique" | "@ignore" | "@updatedAt" ->
      if args_length >= 1 then
        raise_argument_number_error
          (Pprinter.string_of_loc loc)
          0 args_length id
  | "@default" -> (
      if args_length > 1 || phys_equal args_length 0 then
        raise_argument_number_error
          (Pprinter.string_of_loc loc)
          1 args_length id
      else
        let arg = List.hd_exn args in
        let field_record : ModelManager.field_record =
          LocalSymbolTable.lookup model_table ~key:field_id
        in
        let field_type = extract_scalar_type field_record.typ in
        match arg with
        | Model.AttrArgRef (loc, ref) ->
            raise_type_error
              (Pprinter.string_of_loc loc)
              (Pprinter.string_of_scalar_type field_type)
              ref "Reference" id
        | Model.AttrArgNow loc -> (
            match field_type with
            | DateTime -> ()
            | _ ->
                raise_type_error
                  (Pprinter.string_of_loc loc)
                  (Pprinter.string_of_scalar_type field_type)
                  "now" "DateTime" id)
        | Model.AttrArgString (loc, str) -> (
            match field_type with
            | String -> ()
            | _ ->
                raise_type_error
                  (Pprinter.string_of_loc loc)
                  (Pprinter.string_of_scalar_type field_type)
                  (Pprinter.string_of_literal str)
                  "String" id)
        | Model.AttrArgBoolean (loc, boolean) -> (
            match field_type with
            | Boolean -> ()
            | _ ->
                raise_type_error
                  (Pprinter.string_of_loc loc)
                  (Pprinter.string_of_scalar_type field_type)
                  (Pprinter.string_of_literal boolean)
                  "Boolean" id)
        | Model.AttrArgInt (loc, number) -> (
            match field_type with
            | Int -> ()
            | _ ->
                raise_type_error
                  (Pprinter.string_of_loc loc)
                  (Pprinter.string_of_scalar_type field_type)
                  (Pprinter.string_of_literal number)
                  "Int" id))
  | "@relation" -> (
      if args_length > 3 || args_length < 3 then
        raise_argument_number_error
          (Pprinter.string_of_loc loc)
          3 args_length id;
      let relation_name = List.nth_exn args 0 in
      (match relation_name with
      | Model.AttrArgString _ -> ()
      | Model.AttrArgBoolean (loc, boolean) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "String"
            (Pprinter.string_of_literal boolean)
            "Boolean" id
      | Model.AttrArgNow loc ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "String" "now" "DateTime" id
      | Model.AttrArgInt (loc, number) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "String"
            (Pprinter.string_of_literal number)
            "Int" id
      | Model.AttrArgRef (loc, ref) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "String" ref "Reference" id);
      (let relation_field = List.nth_exn args 1 in
       match relation_field with
       | Model.AttrArgRef (loc, field) ->
           let field_attrs =
             (LocalSymbolTable.lookup model_table ~key:field).field_attrs_table
           in
           if not (LocalSymbolTable.contains model_table ~key:field) then
             raise_name_error (Pprinter.string_of_loc loc) "field" field
           else if not (LocalSymbolTable.contains field_attrs ~key:"@unique")
           then
             raise_type_error
               (Pprinter.string_of_loc loc)
               "UniqueField" field "NonUniqueField" id
       | Model.AttrArgString (loc, str) ->
           raise_type_error
             (Pprinter.string_of_loc loc)
             "Reference"
             (Pprinter.string_of_literal str)
             "String" id
       | Model.AttrArgBoolean (loc, boolean) ->
           raise_type_error
             (Pprinter.string_of_loc loc)
             "Reference"
             (Pprinter.string_of_literal boolean)
             "Boolean" id
       | Model.AttrArgNow loc ->
           raise_type_error
             (Pprinter.string_of_loc loc)
             "Reference" "now" "DateTime" id
       | Model.AttrArgInt (loc, number) ->
           raise_type_error
             (Pprinter.string_of_loc loc)
             "Reference"
             (Pprinter.string_of_literal number)
             "Int" id);
      let relation_ref = List.nth_exn args 2 in
      match relation_ref with
      | Model.AttrArgRef (_, ref) ->
          if not (GlobalSymbolTable.contains global_table ~key:ref) then
            raise_name_error (Pprinter.string_of_loc loc) "model" ref;

          if not (GlobalSymbolTable.check_type global_table ~key:ref ModelType)
          then
            raise_type_error
              (Pprinter.string_of_loc loc)
              "Model" ref
              (Pprinter.string_of_declaration_type
                 (GlobalSymbolTable.get_declaration_type global_table ~key:ref))
              id
      | Model.AttrArgString (loc, str) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "Reference"
            (Pprinter.string_of_literal str)
            "String" id
      | Model.AttrArgBoolean (loc, boolean) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "Reference"
            (Pprinter.string_of_literal boolean)
            "Boolean" id
      | Model.AttrArgNow loc ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "Reference" "now" "DateTime" id
      | Model.AttrArgInt (loc, number) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "Reference"
            (Pprinter.string_of_literal number)
            "Int" id)
  | _ -> raise_name_error (Pprinter.string_of_loc loc) "attribute" id

let rec check_field_attrs (global_table : 'a GlobalSymbolTable.t)
    (model_table : 'a LocalSymbolTable.t) (field_id : id)
    (field_attrs : Model.attribute list) : unit =
  match field_attrs with
  | [] -> ()
  | field_attr :: field_attrs ->
      check_field_attr global_table model_table field_id field_attr;
      check_field_attrs global_table model_table field_id field_attrs

let check_field_type (global_table : 'a GlobalSymbolTable.t) (field_type : typ)
    (loc : loc) : unit =
  let custom_type = get_custom_type field_type in
  match custom_type with
  | Some custom_type ->
      if not (GlobalSymbolTable.contains global_table ~key:custom_type) then
        raise_name_error (Pprinter.string_of_loc loc) "type" custom_type
  | None -> ()

let check_field (global_table : 'a GlobalSymbolTable.t)
    (model_table : 'a LocalSymbolTable.t) (field : Model.field) : unit =
  match field with
  | Field (loc, id, field_type, field_attrs) ->
      check_field_type global_table field_type loc;
      check_field_attrs global_table model_table id field_attrs

let rec check_fields (global_table : 'a GlobalSymbolTable.t)
    (model_table : 'a LocalSymbolTable.t) (fields : Model.field list) : unit =
  match fields with
  | [] -> ()
  | field :: fields ->
      check_field global_table model_table field;
      check_fields global_table model_table fields

let check_model (global_table : 'a GlobalSymbolTable.t)
    (model_table : 'a LocalSymbolTable.t) (fields : Model.field list) : unit =
  check_fields global_table model_table fields
