open Core
open Ast
open Ast.Ast_types
open Symbol_table
open Error_handler.Handler

let rec check_models (global_table : 'a GlobalSymbolTable.t) (id : id)
    (models : Query.model list) : unit =
  match models with
  | [] -> ()
  | model :: models ->
      (match model with
      | loc, model_id ->
          if not (GlobalSymbolTable.contains global_table ~key:model_id) then
            raise_name_error (Pprinter.string_of_loc loc) "model" model_id
          else if
            not
              (GlobalSymbolTable.check_type global_table ~key:model_id ModelType)
          then
            raise_type_error
              (Pprinter.string_of_loc loc)
              "Model" model_id
              (Pprinter.string_of_declaration_type
                 (GlobalSymbolTable.get_declaration_type global_table
                    ~key:model_id))
              id);
      check_models global_table id models

let check_where_arg (global_table : 'a GlobalSymbolTable.t) (loc : loc)
    (id : id) (model : Query.model) (field : string) : unit =
  let _, model_id = model in
  let model_table = GlobalSymbolTable.get_table global_table ~key:model_id in
  if not (LocalSymbolTable.contains model_table ~key:field) then
    raise_name_error (Pprinter.string_of_loc loc) "field" field;
  let field_record : field_record =
    LocalSymbolTable.lookup model_table ~key:field
    |> SymbolTableManager.get_model_info
  in
  let field_attrs_table = field_record.field_attrs_table in
  if not (LocalSymbolTable.contains model_table ~key:field) then
    raise_name_error (Pprinter.string_of_loc loc) "field" field
  else if
    not
      (LocalSymbolTable.contains field_attrs_table ~key:"@unique"
      || LocalSymbolTable.contains field_attrs_table ~key:"@id")
  then
    raise_type_error
      (Pprinter.string_of_loc loc)
      "UniqueField" field "NonUniqueField" id

let check_filter_arg (global_table : 'a GlobalSymbolTable.t) (loc : loc)
    (model : Query.model) (fields : string list) : unit =
  let _, model_id = model in
  let model_table = GlobalSymbolTable.get_table global_table ~key:model_id in
  ignore
    (List.map
       ~f:(fun field ->
         if not (LocalSymbolTable.contains model_table ~key:field) then
           raise_name_error (Pprinter.string_of_loc loc) "field" field)
       fields)

let check_data_arg (global_table : 'a GlobalSymbolTable.t) (loc : loc)
    (model : Query.model) (fields : string list) : unit =
  let _, model_id = model in
  let model_table = GlobalSymbolTable.get_table global_table ~key:model_id in
  ignore
    (List.map
       ~f:(fun field ->
         if not (LocalSymbolTable.contains model_table ~key:field) then
           raise_name_error (Pprinter.string_of_loc loc) "field" field)
       fields)

let check_args (global_table : 'a GlobalSymbolTable.t) (loc : loc)
    (typ : Query.typ) (id : id) (model : Query.model) (args : Query.arg list) :
    unit =
  match typ with
  | Query.FindMany -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Where (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "FilterArgument" "where" "WhereArgument" id
      | Query.Data (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "FilterArgument" "data" "DataArgument" id
      | Query.Filter (loc, fields) ->
          check_filter_arg global_table loc model fields)
  | Query.FindUnique -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Filter (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "WhereArgument" "filter" "FilterArgument" id
      | Query.Data (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "WhereArgument" "data" "DataArgument" id
      | Query.Where (loc, field) ->
          check_where_arg global_table loc id model field)
  | Query.Create -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Filter (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "DataArgument" "filter" "FilterArgument" id
      | Query.Where (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "DataArgument" "where" "WhereArgument" id
      | Query.Data (loc, fields) -> check_data_arg global_table loc model fields
      )
  | Query.Update ->
      let args_length = List.length args in
      if not (equal_int args_length 2) then
        raise_argument_number_error
          (Pprinter.string_of_loc loc)
          2 args_length id;
      ignore
        (List.map
           ~f:(fun arg ->
             match arg with
             | Query.Filter (loc, _) ->
                 raise_type_error
                   (Pprinter.string_of_loc loc)
                   "DataArgument and WhereArgument" "filter" "FilterArgument" id
             | Query.Where (loc, field) ->
                 check_where_arg global_table loc id model field
             | Query.Data (loc, fields) ->
                 check_data_arg global_table loc model fields)
           args)
  | Query.Delete -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Filter (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "WhereArgument" "filter" "FilterArgument" id
      | Query.Data (loc, _) ->
          raise_type_error
            (Pprinter.string_of_loc loc)
            "WhereArgument" "data" "DataArgument" id
      | Query.Where (loc, field) ->
          check_where_arg global_table loc id model field)

let check_query (global_table : 'a GlobalSymbolTable.t) (loc : loc) (id : id)
    (body : Query.body) : unit =
  let typ, args, models, _ = body in
  check_models global_table id models;
  check_args global_table loc typ id (List.hd_exn models) args
