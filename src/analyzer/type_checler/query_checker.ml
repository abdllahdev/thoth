open Core
open Ast.Formatter
open Ast.Ast_types
open Ast.Helper
open Environment
open Error_handler.Handler

let check_where_arg global_env loc id model fields =
  let field = List.hd_exn fields in
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
  let model_table =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in
  List.iter fields ~f:(fun field ->
      if not (LocalEnvironment.contains model_table ~key:field) then
        raise_undefined_error loc "field" field ~declaration_id:model_id
          ~declaration_type:ModelDeclaration)

let check_data_arg global_env loc query_id query_type model fields =
  let _, model_id = model in
  let model_table =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in

  (* Check for required fields *)
  (if
   String.equal
     (QueryFormatter.string_of_query_type query_type)
     (QueryFormatter.string_of_query_type Query.Create)
  then
   let check_required_fields ~key ~(data : GlobalEnvironment.field_value) =
     let flatten_fields =
       List.fold fields ~init:[] ~f:(fun lst field ->
           let field, relation_fields = field in
           match relation_fields with
           | Some (_, model_field) -> lst @ [ field ] @ [ model_field ]
           | None -> lst @ [ field ])
     in
     let attributes_table = data.field_attrs_table in
     if
       not
         (LocalEnvironment.contains attributes_table ~key:"@default"
         || LocalEnvironment.contains attributes_table ~key:"@id"
         || LocalEnvironment.contains attributes_table ~key:"@updatedAt")
     then
       if not (List.mem flatten_fields key ~equal:String.equal) then
         raise_required_argument_error loc key data.typ query_id
   in
   Hashtbl.iteri model_table ~f:check_required_fields);

  (* Check if data fields exists in the model *)
  let check_fields field =
    let field, relation_fields = field in
    if not (LocalEnvironment.contains model_table ~key:field) then
      raise_undefined_error loc "field" field ~declaration_id:model_id
        ~declaration_type:ModelDeclaration;

    (* Check model relation fields *)
    match relation_fields with
    | Some (reference_field, model_field) -> (
        if not (LocalEnvironment.contains model_table ~key:model_field) then
          raise_undefined_error loc "field" model_field ~declaration_id:model_id
            ~declaration_type:ModelDeclaration;

        let relation_field = LocalEnvironment.lookup model_table ~key:field in

        let attributes_table = relation_field.field_attrs_table in

        let relation_args =
          LocalEnvironment.lookup attributes_table ~key:"@relation"
        in

        match List.nth_exn relation_args 1 with
        | Model.AttrArgRef (_, ref) ->
            if not (String.equal reference_field ref) then
              raise_undefined_error loc "field" reference_field
                ~declaration_id:
                  (get_scalar_type relation_field.typ |> string_of_scalar_type)
                ~declaration_type:ModelDeclaration
        | _ -> ())
    | None -> ()
  in
  List.iter fields ~f:check_fields

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
      | Query.Where (loc, fields) ->
          check_where_arg global_env loc id model fields)
  | Query.Create -> (
      let arg = List.hd_exn args in
      match arg with
      | Query.Search (loc, _) ->
          raise_query_argument_error loc id [ Query.DataArgument ]
            Query.SearchArgument
      | Query.Where (loc, _) ->
          raise_query_argument_error loc id [ Query.DataArgument ]
            Query.WhereArgument
      | Query.Data (loc, fields) ->
          check_data_arg global_env loc id typ model fields)
  | Query.Update ->
      let args_length = List.length args in
      if not (equal_int args_length 2) then
        raise_argument_number_error loc 2 args_length id;
      List.iter args ~f:(fun arg ->
          match arg with
          | Query.Search (loc, _) ->
              raise_query_argument_error loc id
                [ Query.WhereArgument; Query.DataArgument ]
                Query.SearchArgument
          | Query.Where (loc, field) ->
              check_where_arg global_env loc id model field
          | Query.Data (loc, fields) ->
              check_data_arg global_env loc id typ model fields)
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
