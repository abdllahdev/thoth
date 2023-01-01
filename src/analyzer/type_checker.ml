open Ast
open Error
open Symbol_table
open Core

module ModelTypeChecker = struct
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
        if args_length > 1 || equal_int args_length 0 then
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Expected 1 argument in '%s' but received %d."
                  (Pprinter.string_of_loc loc)
                  id args_length))
        else
          let arg = List.hd args in
          match arg with
          | Some arg -> (
              let field_record : SymbolTableManager.field_record =
                LocalSymbolTable.lookup model_table field_id
              in
              let field_type = field_record.field_type in
              match arg with
              | Model.AttrArgFunc (loc, _)
              | Model.AttrArgRefList (loc, _)
              | Model.AttrArgRef (loc, _) ->
                  raise
                    (TypeError
                       (Fmt.str
                          "TypeError@(%s): Attribute '%s' can only be of type \
                           string, boolean or number"
                          (Pprinter.string_of_loc loc)
                          id))
              (* TODO: Implement this to check the attribute type matches the field type *)
              | Model.AttrArgString (loc, _) -> (
                  match field_type with
                  | Model.FieldTypeString -> ()
                  | _ ->
                      raise
                        (TypeError
                           (Fmt.str
                              "TypeError@(%s): Attribute '%s' value must match \
                               the field type"
                              (Pprinter.string_of_loc loc)
                              id)))
              | Model.AttrArgBoolean (loc, _) -> (
                  match field_type with
                  | Model.FieldTypeBoolean -> ()
                  | _ ->
                      raise
                        (TypeError
                           (Fmt.str
                              "TypeError@(%s): Attribute '%s' value must match \
                               the field type"
                              (Pprinter.string_of_loc loc)
                              id)))
              | Model.AttrArgNumber (loc, _) -> (
                  match field_type with
                  | Model.FieldTypeInt | Model.FieldTypeDecimal
                  | Model.FieldTypeFloat | Model.FieldTypeBigInt ->
                      ()
                  | _ ->
                      raise
                        (TypeError
                           (Fmt.str
                              "TypeError@(%s): Attribute '%s' value must match \
                               the field type"
                              (Pprinter.string_of_loc loc)
                              id))))
          | None -> ())
    | "@relation" -> (
        if args_length > 3 || args_length < 3 then
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Expected 3 argument in '%s' but received %d."
                  (Pprinter.string_of_loc loc)
                  id args_length));
        let relationName = List.nth args 0 in
        (match relationName with
        | Some argString -> (
            match argString with
            | Model.AttrArgString _ -> ()
            | _ ->
                raise
                  (TypeError
                     (Fmt.str "TypeError@(%s): Relation name must be string."
                        (Pprinter.string_of_loc loc))))
        | None -> ());
        (let relationFields = List.nth args 1 in
         match relationFields with
         | Some argList -> (
             match argList with
             | Model.AttrArgRef (loc, field) ->
                 if not (LocalSymbolTable.contains model_table field) then
                   raise
                     (TypeError
                        (Fmt.str "TypeError@(%s): Field '%s' is not defined"
                           (Pprinter.string_of_loc loc)
                           field))
             | _ ->
                 raise
                   (TypeError
                      (Fmt.str
                         "TypeError@(%s): Relation fields must be a reference."
                         (Pprinter.string_of_loc loc))))
         | None -> ());
        let relationTables = List.nth args 2 in
        match relationTables with
        | Some argList -> (
            match argList with
            | Model.AttrArgRef (_, ref) ->
                if not (GlobalSymbolTable.contains global_table ref) then
                  raise
                    (TypeError
                       (Fmt.str "TypeError@(%s): Model '%s' is not defined"
                          (Pprinter.string_of_loc loc)
                          ref))
            | _ ->
                raise
                  (TypeError
                     (Fmt.str
                        "TypeError@(%s): Relation references must be a \
                         reference."
                        (Pprinter.string_of_loc loc))))
        | None -> ())
    | _ -> raise (NameError (Fmt.str "NameError: Undefined attribute '%s'." id))

  let rec check_field_attrs (global_table : 'a GlobalSymbolTable.t)
      (model_table : 'a LocalSymbolTable.t) (field_id : id)
      (field_attrs : Model.field_attr list) : unit =
    match field_attrs with
    | [] -> ()
    | field_attr :: field_attrs ->
        check_field_attr global_table model_table field_id field_attr;
        check_field_attrs global_table model_table field_id field_attrs

  let check_field_type (global_table : 'a GlobalSymbolTable.t)
      (field_type : Model.field_type) : unit =
    match field_type with
    | FieldTypeCustom (loc, field_type_custom) ->
        if not (GlobalSymbolTable.contains global_table field_type_custom) then
          raise
            (NameError
               (Fmt.str "NameError@(%s): Undefined type '%s'"
                  (Pprinter.string_of_loc loc)
                  field_type_custom))
    | _ -> ()

  let check_field (global_table : 'a GlobalSymbolTable.t)
      (model_table : 'a LocalSymbolTable.t) (field : Model.field) : unit =
    match field with
    | Field (_, id, field_type, _, field_attrs) ->
        check_field_type global_table field_type;
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
end

module TypeChecker = struct
  let check_declaration (global_table : 'a GlobalSymbolTable.t)
      (declaration : declaration) : unit =
    match declaration with
    | Model (_, id, fields) ->
        let model_table = GlobalSymbolTable.lookup global_table id in
        ModelTypeChecker.check_model global_table model_table fields

  let rec semantic_check (global_table : 'a GlobalSymbolTable.t)
      (Ast declarations) : unit =
    match declarations with
    | [] -> ()
    | declaration :: declarations ->
        check_declaration global_table declaration;
        semantic_check global_table (Ast declarations)

  let run (Ast declarations) : unit =
    let global_table = GlobalSymbolTable.create () in
    SymbolTableManager.populate global_table (Ast declarations);
    semantic_check global_table (Ast declarations)
end
