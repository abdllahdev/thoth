open Core
open Ast.Pprinter
open Ast.Ast_types
open Environment
open Error_handler.Handler

let check_where_arg global_env loc id model field =
  let _, model_id = model in
  let model_value =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in

  if not (LocalEnvironment.contains model_value ~key:field) then
    raise_undefined_error loc "field" field ~declaration_id:model_id
      ~declaration_type:ModelDeclaration;

  let field_info : GlobalEnvironment.field_value =
    LocalEnvironment.lookup model_value ~key:field
  in

  let field_attrs_table = field_info.field_attrs_table in
  if not (LocalEnvironment.contains model_value ~key:field) then
    raise_undefined_error loc "field" field ~declaration_id:model_id
      ~declaration_type:ModelDeclaration
  else if
    not
      (LocalEnvironment.contains field_attrs_table ~key:"@unique"
      || LocalEnvironment.contains field_attrs_table ~key:"@id")
  then
    raise_unique_field_error loc Model.UniqueField field Model.NonUniqueField
      ~id

let check_filter_arg global_env loc model fields =
  let _, model_id = model in
  let model_value =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in
  ignore
    (List.map fields ~f:(fun field ->
         if not (LocalEnvironment.contains model_value ~key:field) then
           raise_undefined_error loc "field" field ~declaration_id:model_id
             ~declaration_type:ModelDeclaration))

let check_data_arg global_env loc model fields =
  let _, model_id = model in
  let model_value =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in
  ignore
    (List.map fields ~f:(fun field ->
         if not (LocalEnvironment.contains model_value ~key:field) then
           raise_undefined_error loc "field" field ~declaration_id:model_id
             ~declaration_type:ModelDeclaration))

let check_args global_env loc typ id model args : unit =
  match typ with
  | Query.FindMany -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Where (loc, _) ->
          raise_query_argument_error loc id [ Query.SearchArgument ]
            Query.WhereArgument
      | Query.Data (loc, _) ->
          raise_query_argument_error loc id [ Query.SearchArgument ]
            Query.DataArgument
      | Query.Search (loc, fields) ->
          check_filter_arg global_env loc model fields)
  | Query.FindUnique -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Search (loc, _) ->
          raise_query_argument_error loc id [ Query.WhereArgument ]
            Query.SearchArgument
      | Query.Data (loc, _) ->
          raise_query_argument_error loc id [ Query.WhereArgument ]
            Query.DataArgument
      | Query.Where (loc, field) ->
          check_where_arg global_env loc id model field)
  | Query.Create -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Search (loc, _) ->
          raise_query_argument_error loc id [ Query.DataArgument ]
            Query.SearchArgument
      | Query.Where (loc, _) ->
          raise_query_argument_error loc id [ Query.DataArgument ]
            Query.WhereArgument
      | Query.Data (loc, fields) -> check_data_arg global_env loc model fields)
  | Query.Update ->
      let args_length = List.length args in
      if not (equal_int args_length 2) then
        raise_argument_number_error loc 2 args_length id;
      ignore
        (List.map args ~f:(fun arg ->
             match arg with
             | Query.Search (loc, _) ->
                 raise_query_argument_error loc id
                   [ Query.WhereArgument; Query.DataArgument ]
                   Query.SearchArgument
             | Query.Where (loc, field) ->
                 check_where_arg global_env loc id model field
             | Query.Data (loc, fields) ->
                 check_data_arg global_env loc model fields))
  | Query.Delete -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Search (loc, _) ->
          raise_query_argument_error loc id [ Query.WhereArgument ]
            Query.SearchArgument
      | Query.Data (loc, _) ->
          raise_query_argument_error loc id [ Query.WhereArgument ]
            Query.DataArgument
      | Query.Where (loc, field) ->
          check_where_arg global_env loc id model field)

let check_query global_env query =
  let loc, id, typ, return_type, args, models, _ = query in
  (* Check query models *)
  let check_model model =
    match model with
    | loc, model_id ->
        if not (GlobalEnvironment.contains global_env ~key:model_id) then
          raise_undefined_error loc "model" model_id;
        let declaration_value =
          GlobalEnvironment.lookup global_env ~key:model_id
        in
        if not (GlobalEnvironment.check_type declaration_value ModelDeclaration)
        then
          raise_declaration_type_error loc ModelDeclaration model_id
            (GlobalEnvironment.infer_type declaration_value)
            ~id
  in
  List.iter models ~f:check_model;

  (* Check query arguments *)
  check_args global_env loc typ id (List.hd_exn models) args;

  (* Check query return type *)
  match return_type with
  | Some return_type ->
      let expected_return_type =
        (GlobalEnvironment.lookup global_env ~key:id
        |> GlobalEnvironment.get_query_value)
          .return_type
      in

      if
        not
          (String.equal
             (string_of_type return_type)
             (string_of_type expected_return_type))
      then
        raise_query_return_type_error loc typ expected_return_type return_type
  | None -> ()
