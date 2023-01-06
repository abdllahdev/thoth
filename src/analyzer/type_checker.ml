open Ast
open Error
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
                  (Fmt.str
                     "TypeError@(%s): Relation fields must be a reference."
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

            if not (GlobalSymbolTable.check_type global_table ref ModelType)
            then
              raise
                (TypeError
                   (Fmt.str
                      "TypeError@(%s): Relation reference must be of type \
                       'Model' but received %s of type '%s'"
                      (Pprinter.string_of_loc loc)
                      ref
                      (Pprinter.string_of_declaration_type
                         (GlobalSymbolTable.get_declaration_type global_table
                            ref))))
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

  let check_field_type (global_table : 'a GlobalSymbolTable.t)
      (field_type : typ) (loc : loc) : unit =
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
end

module QueryTypeChecker = struct
  let rec check_models (global_table : 'a GlobalSymbolTable.t)
      (models : Query.model list) : unit =
    match models with
    | [] -> ()
    | model :: models ->
        (match model with
        | loc, model_id ->
            if not (GlobalSymbolTable.contains global_table model_id) then
              raise
                (NameError
                   (Fmt.str "NameError@(%s): Undefined model '%s'"
                      (Pprinter.string_of_loc loc)
                      model_id))
            else if
              not (GlobalSymbolTable.check_type global_table model_id ModelType)
            then
              raise
                (TypeError
                   (Fmt.str
                      "TypeError@(%s): Query model reference must be of type \
                       'Model' but received %s of type '%s'"
                      (Pprinter.string_of_loc loc)
                      model_id
                      (Pprinter.string_of_declaration_type
                         (GlobalSymbolTable.get_declaration_type global_table
                            model_id)))));
        check_models global_table models

  let check_where_arg (global_table : 'a GlobalSymbolTable.t) (loc : loc)
      (model : Query.model) (field : string) : unit =
    let _, model_id = model in
    let model_table =
      Option.get (GlobalSymbolTable.get_table global_table model_id)
    in
    if not (LocalSymbolTable.contains model_table field) then
      raise
        (TypeError
           (Fmt.str "TypeError@(%s): Field '%s' doesn't exist in model '%s'"
              (Pprinter.string_of_loc loc)
              field model_id));
    let field_record : ModelManager.field_record =
      LocalSymbolTable.lookup model_table field
    in
    let field_attrs_table = field_record.field_attrs_table in
    if not (LocalSymbolTable.contains model_table field) then
      raise
        (TypeError
           (Fmt.str "TypeError@(%s): Field '%s' doesn't exist in model '%s'"
              (Pprinter.string_of_loc loc)
              field model_id))
    else if
      not
        (LocalSymbolTable.contains field_attrs_table "@unique"
        || LocalSymbolTable.contains field_attrs_table "@id")
    then
      raise
        (TypeError
           (Fmt.str "TypeError@(%s): Field '%s' must be unique"
              (Pprinter.string_of_loc loc)
              field))

  let check_filter_arg (global_table : 'a GlobalSymbolTable.t) (loc : loc)
      (model : Query.model) (fields : string list) : unit =
    let _, model_id = model in
    let model_table =
      Option.get (GlobalSymbolTable.get_table global_table model_id)
    in
    ignore
      (List.map
         (fun field ->
           if not (LocalSymbolTable.contains model_table field) then
             raise
               (TypeError
                  (Fmt.str
                     "TypeError@(%s): Field '%s' doesn't exist in model '%s'"
                     (Pprinter.string_of_loc loc)
                     field model_id)))
         fields)

  let check_data_arg (global_table : 'a GlobalSymbolTable.t) (loc : loc)
      (model : Query.model) (fields : string list) : unit =
    let _, model_id = model in
    let model_table =
      Option.get (GlobalSymbolTable.get_table global_table model_id)
    in
    ignore
      (List.map
         (fun field ->
           if not (LocalSymbolTable.contains model_table field) then
             raise
               (TypeError
                  (Fmt.str
                     "TypeError@(%s): Field '%s' doesn't exist in model '%s'"
                     (Pprinter.string_of_loc loc)
                     field model_id)))
         fields)

  let check_args (global_table : 'a GlobalSymbolTable.t) (loc : loc)
      (typ : Query.typ) (model : Query.model) (args : Query.arg list) : unit =
    match typ with
    | Query.FindAll -> (
        let arg = List.hd args in
        match arg with
        | Query.Where (loc, _) | Query.Data (loc, _) ->
            raise
              (TypeError
                 (Fmt.str
                    "TypeError@(%s): Query of type 'findAll' only takes a \
                     'filter' argument"
                    (Pprinter.string_of_loc loc)))
        | Query.Filter (loc, fields) ->
            check_filter_arg global_table loc model fields)
    | Query.FindUnique -> (
        let arg = List.hd args in
        match arg with
        | Query.Filter (loc, _) | Query.Data (loc, _) ->
            raise
              (TypeError
                 (Fmt.str
                    "TypeError@(%s): Query of type 'findUnqiue' only takes a \
                     'where' argument"
                    (Pprinter.string_of_loc loc)))
        | Query.Where (loc, field) ->
            check_where_arg global_table loc model field)
    | Query.Create -> (
        let arg = List.hd args in
        match arg with
        | Query.Filter (loc, _) | Query.Where (loc, _) ->
            raise
              (TypeError
                 (Fmt.str
                    "TypeError@(%s): Query of type 'create' only takes a \
                     'data' argument"
                    (Pprinter.string_of_loc loc)))
        | Query.Data (loc, fields) ->
            check_data_arg global_table loc model fields)
    | Query.Update ->
        if List.length args != 2 then
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Query of type 'update' only takes a 'where' \
                   and 'data' arguments"
                  (Pprinter.string_of_loc loc)));
        ignore
          (List.map
             (fun arg ->
               match arg with
               | Query.Filter (loc, _) ->
                   raise
                     (TypeError
                        (Fmt.str
                           "TypeError@(%s): Query of type 'update' only takes \
                            a 'where' and 'data' arguments"
                           (Pprinter.string_of_loc loc)))
               | Query.Where (loc, field) ->
                   check_where_arg global_table loc model field
               | Query.Data (loc, fields) ->
                   check_data_arg global_table loc model fields)
             args)
    | Query.Delete -> (
        let arg = List.hd args in
        match arg with
        | Query.Filter (loc, _) | Query.Data (loc, _) ->
            raise
              (TypeError
                 (Fmt.str
                    "TypeError@(%s): Query of type 'delete' only takes a \
                     'where' argument"
                    (Pprinter.string_of_loc loc)))
        | Query.Where (loc, field) ->
            check_where_arg global_table loc model field)

  let check_query_body (global_table : 'a GlobalSymbolTable.t) (loc : loc)
      (body : Query.body) : unit =
    match body with
    | typ, args, models, _ ->
        check_models global_table models;
        check_args global_table loc typ (List.hd models) args
end

module TypeChecker = struct
  let check_declaration (global_table : 'a GlobalSymbolTable.t)
      (declaration : declaration) : unit =
    match declaration with
    | Model (_, id, body) ->
        let model_table =
          Option.get (GlobalSymbolTable.get_table global_table id)
        in
        ModelTypeChecker.check_model global_table model_table body
    | Query (loc, _, body) ->
        QueryTypeChecker.check_query_body global_table loc body

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
