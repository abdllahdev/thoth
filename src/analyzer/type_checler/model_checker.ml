open Ast
open Ast.Ast_types
open Error_handler.Error
open Symbol_table
open Helper

let check_field_attr (global_table : 'a GlobalSymbolTable.t)
    (model_table : 'a LocalSymbolTable.t) (field_id : id)
    (Model.Attribute (loc, id, args)) : unit =
  let args_length = List.length args in
  match id with
  | "@id" | "@unique" | "@ignore" | "@updatedAt" ->
      if args_length >= 1 then
        raise
          (TypeError
             (Fmt.str
                "TypeError@(%s): Expected 0 argument in '%s' but received %d."
                (Pprinter.string_of_loc loc)
                id args_length))
  | "@default" -> (
      if args_length > 1 || args_length == 0 then
        raise
          (TypeError
             (Fmt.str
                "TypeError@(%s): Expected 1 argument in '%s' but received %d."
                (Pprinter.string_of_loc loc)
                id args_length))
      else
        let arg = List.hd args in
        let field_record : ModelManager.field_record =
          LocalSymbolTable.lookup model_table field_id
        in
        let field_type = extract_scalar_type field_record.typ in
        match arg with
        | Model.AttrArgRef (loc, _) ->
            raise
              (TypeError
                 (Fmt.str
                    "TypeError@(%s): Attribute '%s' can only be of type \
                     string, boolean, number, or 'now()' func"
                    (Pprinter.string_of_loc loc)
                    id))
        | Model.AttrArgFunc (loc, func) -> (
            if not (String.equal func "now") then
              raise
                (NameError
                   (Fmt.str "NameError@(%s): Undefined function %s"
                      (Pprinter.string_of_loc loc)
                      func));
            match field_type with
            | DateTime -> ()
            | _ ->
                raise
                  (TypeError
                     (Fmt.str
                        "TypeError@(%s): Attribute '%s' value must match the \
                         field type"
                        (Pprinter.string_of_loc loc)
                        id)))
        | Model.AttrArgString (loc, _) -> (
            match field_type with
            | String -> ()
            | _ ->
                raise
                  (TypeError
                     (Fmt.str
                        "TypeError@(%s): Attribute '%s' value must match the \
                         field type"
                        (Pprinter.string_of_loc loc)
                        id)))
        | Model.AttrArgBoolean (loc, _) -> (
            match field_type with
            | Boolean -> ()
            | _ ->
                raise
                  (TypeError
                     (Fmt.str
                        "TypeError@(%s): Attribute '%s' value must match the \
                         field type"
                        (Pprinter.string_of_loc loc)
                        id)))
        | Model.AttrArgInt (loc, _) -> (
            match field_type with
            | Int -> ()
            | _ ->
                raise
                  (TypeError
                     (Fmt.str
                        "TypeError@(%s): Attribute '%s' value must match the \
                         field type"
                        (Pprinter.string_of_loc loc)
                        id))))
  | "@relation" -> (
      if args_length > 3 || args_length < 3 then
        raise
          (TypeError
             (Fmt.str
                "TypeError@(%s): Expected 3 argument in '%s' but received %d."
                (Pprinter.string_of_loc loc)
                id args_length));
      let relation_name = List.nth args 0 in
      (match relation_name with
      | Model.AttrArgString _ -> ()
      | _ ->
          raise
            (TypeError
               (Fmt.str "TypeError@(%s): Relation name must be string."
                  (Pprinter.string_of_loc loc))));
      (let relation_field = List.nth args 1 in
       match relation_field with
       | Model.AttrArgRef (loc, field) ->
           let field_attrs =
             (LocalSymbolTable.lookup model_table field).field_attrs_table
           in
           if not (LocalSymbolTable.contains model_table field) then
             raise
               (TypeError
                  (Fmt.str "TypeError@(%s): Field '%s' is not defined"
                     (Pprinter.string_of_loc loc)
                     field))
           else if not (LocalSymbolTable.contains field_attrs "@unique") then
             raise
               (TypeError
                  (Fmt.str "TypeError@(%s): Field '%s' must be unique"
                     (Pprinter.string_of_loc loc)
                     field))
       | _ ->
           raise
             (TypeError
                (Fmt.str "TypeError@(%s): Relation fields must be a reference."
                   (Pprinter.string_of_loc loc))));
      let relation_ref = List.nth args 2 in
      match relation_ref with
      | Model.AttrArgRef (_, ref) ->
          if not (GlobalSymbolTable.contains global_table ref) then
            raise
              (TypeError
                 (Fmt.str "TypeError@(%s): Model '%s' is not defined"
                    (Pprinter.string_of_loc loc)
                    ref));

          if not (GlobalSymbolTable.check_type global_table ref ModelType) then
            raise
              (TypeError
                 (Fmt.str
                    "TypeError@(%s): Relation reference must be of type \
                     'Model' but received %s of type '%s'"
                    (Pprinter.string_of_loc loc)
                    ref
                    (Pprinter.string_of_declaration_type
                       (GlobalSymbolTable.get_declaration_type global_table ref))))
      | _ ->
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Relation references must be a reference"
                  (Pprinter.string_of_loc loc))))
  | _ ->
      raise
        (NameError
           (Fmt.str "NameError@(%s): Undefined model field attribute '%s'"
              (Pprinter.string_of_loc loc)
              id))

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
      if not (GlobalSymbolTable.contains global_table custom_type) then
        raise
          (NameError
             (Fmt.str "NameError@(%s): Undefined type '%s'"
                (Pprinter.string_of_loc loc)
                custom_type))
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