open Core
open Ast
open Ast.Ast_types
open Environment
open Error_handler.Handler

let rec check_models global_env id models =
  match models with
  | [] -> ()
  | model :: models ->
      (match model with
      | loc, model_id ->
          if not (GlobalEnvironment.contains global_env ~key:model_id) then
            raise_undefined_error loc "model" model_id;
          let declaration_value =
            GlobalEnvironment.lookup global_env ~key:model_id
          in
          if not (GlobalEnvironment.check_type declaration_value ModelType) then
            raise_type_error loc "Model" model_id
              (GlobalEnvironment.infer_type declaration_value
              |> Pprinter.string_of_declaration_type)
              ~id);
      check_models global_env id models

let check_where_arg global_env loc id model field =
  let _, model_id = model in
  let model_value =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in
  if not (LocalEnvironment.contains model_value ~key:field) then
    raise_undefined_error loc "field" field ~declaration_id:model_id
      ~declaration_type:"model";
  let field_info : GlobalEnvironment.field_value =
    LocalEnvironment.lookup model_value ~key:field
  in
  let field_attrs_table = field_info.field_attrs_table in
  if not (LocalEnvironment.contains model_value ~key:field) then
    raise_undefined_error loc "field" field ~declaration_id:model_id
      ~declaration_type:"model"
  else if
    not
      (LocalEnvironment.contains field_attrs_table ~key:"@unique"
      || LocalEnvironment.contains field_attrs_table ~key:"@id")
  then raise_type_error loc "UniqueField" field "NonUniqueField" ~id

let check_filter_arg global_env loc model fields =
  let _, model_id = model in
  let model_value =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in
  ignore
    (List.map
       ~f:(fun field ->
         if not (LocalEnvironment.contains model_value ~key:field) then
           raise_undefined_error loc "field" field ~declaration_id:model_id
             ~declaration_type:"model")
       fields)

let check_data_arg global_env loc model fields =
  let _, model_id = model in
  let model_value =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in
  ignore
    (List.map
       ~f:(fun field ->
         if not (LocalEnvironment.contains model_value ~key:field) then
           raise_undefined_error loc "field" field ~declaration_id:model_id
             ~declaration_type:"model")
       fields)

let check_args global_env loc typ id model args : unit =
  match typ with
  | Query.FindMany -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Where (loc, _) ->
          raise_type_error loc "FilterArgument" "where" "WhereArgument" ~id
      | Query.Data (loc, _) ->
          raise_type_error loc "FilterArgument" "data" "DataArgument" ~id
      | Query.Filter (loc, fields) ->
          check_filter_arg global_env loc model fields)
  | Query.FindUnique -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Filter (loc, _) ->
          raise_type_error loc "WhereArgument" "filter" "FilterArgument" ~id
      | Query.Data (loc, _) ->
          raise_type_error loc "WhereArgument" "data" "DataArgument" ~id
      | Query.Where (loc, field) ->
          check_where_arg global_env loc id model field)
  | Query.Create -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Filter (loc, _) ->
          raise_type_error loc "DataArgument" "filter" "FilterArgument" ~id
      | Query.Where (loc, _) ->
          raise_type_error loc "DataArgument" "where" "WhereArgument" ~id
      | Query.Data (loc, fields) -> check_data_arg global_env loc model fields)
  | Query.Update ->
      let args_length = List.length args in
      if not (equal_int args_length 2) then
        raise_argument_number_error loc 2 args_length id;
      ignore
        (List.map
           ~f:(fun arg ->
             match arg with
             | Query.Filter (loc, _) ->
                 raise_type_error loc "DataArgument and WhereArgument" "filter"
                   "FilterArgument" ~id
             | Query.Where (loc, field) ->
                 check_where_arg global_env loc id model field
             | Query.Data (loc, fields) ->
                 check_data_arg global_env loc model fields)
           args)
  | Query.Delete -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Filter (loc, _) ->
          raise_type_error loc "WhereArgument" "filter" "FilterArgument" ~id
      | Query.Data (loc, _) ->
          raise_type_error loc "WhereArgument" "data" "DataArgument" ~id
      | Query.Where (loc, field) ->
          check_where_arg global_env loc id model field)

let check_query global_env query =
  let loc, id, typ, args, models, _ = query in
  check_models global_env id models;
  check_args global_env loc typ id (List.hd_exn models) args
