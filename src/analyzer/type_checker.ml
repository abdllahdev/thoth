open Ast
open Error
open Symbol_table

module ModelTypeChecker = struct
  let check_field_attr (_ : 'a GlobalSymbolTable.t) (_ : 'a LocalSymbolTable.t)
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
                  "TypeError@(%s): Expected 1 argument in '@default' but \
                   received %d."
                  (Pprinter.string_of_loc loc)
                  args_length))
        else
          let arg = List.hd args in
          match arg with
          | Model.AttrArgFunc (_, _) ->
              raise
                (TypeError
                   (Fmt.str
                      "TypeError@(%s): Attribute '@default' doesn't accept \
                       functions as arguments."
                      (Pprinter.string_of_loc loc)))
          | Model.AttrArgString _ -> ()
          | Model.AttrArgBoolean _ -> ()
          | Model.AttrArgNumber _ -> ()
          | Model.AttrArgRefList _ -> ())
    | "@relation" ->
        if args_length > 3 || args_length < 3 then
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Expected 3 argument in '@relation' but \
                   received %d."
                  (Pprinter.string_of_loc loc)
                  args_length))
    | _ -> raise (NameError (Fmt.str "NameError: Unknown attribute '%s'." id))

  let rec check_field_attrs (global_table : 'a GlobalSymbolTable.t)
      (model_table : 'a LocalSymbolTable.t)
      (field_attrs : Model.field_attr list) : unit =
    match field_attrs with
    | [] -> ()
    | field_attr :: field_attrs ->
        check_field_attr global_table model_table field_attr;
        check_field_attrs global_table model_table field_attrs

  let check_field_type (global_table : 'a GlobalSymbolTable.t)
      (field_type : Model.field_type) : unit =
    match field_type with
    | FieldTypeCustom (loc, field_type_custom) ->
        if not (GlobalSymbolTable.contains global_table field_type_custom) then
          raise
            (NameError
               (Fmt.str "NameError@(%s): Unknown type '%s'"
                  (Pprinter.string_of_loc loc)
                  field_type_custom))
    | _ -> ()

  let check_field (global_table : 'a GlobalSymbolTable.t)
      (model_table : 'a LocalSymbolTable.t) (field : Model.field) : unit =
    match field with
    | Field (_, _, field_type, _, field_attrs) ->
        check_field_type global_table field_type;
        check_field_attrs global_table model_table field_attrs

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
