open Ast
open Ast.Ast_types
open Symbol_table
open Error_handler.Error

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
      | Query.Where (loc, field) -> check_where_arg global_table loc model field
      )
  | Query.Create -> (
      let arg = List.hd args in
      match arg with
      | Query.Filter (loc, _) | Query.Where (loc, _) ->
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Query of type 'create' only takes a 'data' \
                   argument"
                  (Pprinter.string_of_loc loc)))
      | Query.Data (loc, fields) -> check_data_arg global_table loc model fields
      )
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
                         "TypeError@(%s): Query of type 'update' only takes a \
                          'where' and 'data' arguments"
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
                  "TypeError@(%s): Query of type 'delete' only takes a 'where' \
                   argument"
                  (Pprinter.string_of_loc loc)))
      | Query.Where (loc, field) -> check_where_arg global_table loc model field
      )

let check_query_body (global_table : 'a GlobalSymbolTable.t) (loc : loc)
    (body : Query.body) : unit =
  match body with
  | typ, args, models, _ ->
      check_models global_table models;
      check_args global_table loc typ (List.hd models) args
