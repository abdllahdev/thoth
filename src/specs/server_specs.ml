open Core
open Error_handler.Handler
open Ast.Ast_types
open Ast.Formatter
open Ast.Helper
open Type_checker.Environment

type typed_variable = string * string list
type field = Field of typed_variable | Object of (string * field)

type query_args_specs = {
  where : typed_variable list option;
  search : typed_variable list option;
  data : field list option;
}

type query_specs = {
  query_id : string;
  query_type : Query.typ;
  return_type : typ;
  query_args : query_args_specs;
  query_model : string;
  query_permissions : string list option;
}

type validator_specs = { validator_id : string; fields : query_args_specs }

type required_args = {
  requires_where : bool;
  requires_search : bool;
  requires_data : bool;
}

type controller_function_specs = {
  function_id : string;
  function_type : string;
  middlewares : string list;
  required_args : required_args;
}

type route_specs = {
  route_id : string;
  route_type : string;
  middlewares : string list;
  route_param : string option;
}

type server_specs = {
  controllers_specs : (string, controller_function_specs list) Hashtbl.t;
  routes_specs : (string, route_specs list) Hashtbl.t;
  validators_specs : (string, validator_specs list) Hashtbl.t;
  auth_specs : auth_config option;
}

let convert_type typ =
  let convert_scalar_type scalar_type =
    match scalar_type with
    | Int -> [ "number" ]
    | String -> [ "string" ]
    | Boolean -> [ "boolean" ]
    | DateTime -> [ "date" ]
    | CustomType typ -> [ typ ]
    | _ -> raise_compiler_error ()
  in

  let convert_composite_type composite_type =
    match composite_type with
    | List scalar_type -> convert_scalar_type scalar_type @ [ "array" ]
    | Optional scalar_type -> convert_scalar_type scalar_type @ [ "optional" ]
    | OptionalList scalar_type ->
        convert_scalar_type scalar_type @ [ "optional"; "array" ]
  in

  match typ with
  | Composite composite_type -> convert_composite_type composite_type
  | Scalar scalar_type -> convert_scalar_type scalar_type

let get_query_args global_env model query_type query_args =
  let _, model_id = model in
  let model_fields =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in

  let where =
    List.map query_args ~f:(function
      | Query.Where (_, fields) ->
          Some
            (List.map fields ~f:(fun field ->
                 let arg_type =
                   (LocalEnvironment.lookup model_fields ~key:field).typ
                 in
                 (field, convert_type arg_type)))
      | _ -> None)
    |> List.find ~f:(function Some _ -> true | None -> false)
    |> Option.value_or_thunk ~default:(fun () -> None)
  in

  let search =
    List.map query_args ~f:(function
      | Query.Search (_, fields) ->
          Some
            (List.map fields ~f:(fun field ->
                 let arg_type =
                   (LocalEnvironment.lookup model_fields ~key:field).typ
                 in
                 (field, convert_type arg_type)))
      | _ -> None)
    |> List.find ~f:(function Some _ -> true | None -> false)
    |> Option.value_or_thunk ~default:(fun () -> None)
  in

  let data =
    List.map query_args ~f:(function
      | Query.Data (_, fields) ->
          Some
            (List.map fields ~f:(fun field ->
                 let field, relations = field in
                 let arg_type =
                   (LocalEnvironment.lookup model_fields ~key:field).typ
                 in
                 match relations with
                 | Some relations ->
                     let reference_field, relation_field = relations in
                     let reference_field_type =
                       (LocalEnvironment.lookup model_fields ~key:relation_field)
                         .typ
                     in
                     Object
                       ( field,
                         if
                           String.equal
                             (QueryFormatter.string_of_query_type query_type)
                             (QueryFormatter.string_of_query_type Query.Update)
                         then
                           Field
                             ( reference_field,
                               convert_type arg_type @ [ "optional" ] )
                         else
                           Field
                             (reference_field, convert_type reference_field_type)
                       )
                 | None ->
                     if
                       String.equal
                         (QueryFormatter.string_of_query_type query_type)
                         (QueryFormatter.string_of_query_type Query.Update)
                     then Field (field, convert_type arg_type @ [ "optional" ])
                     else Field (field, convert_type arg_type)))
      | _ -> None)
    |> List.find ~f:(function Some _ -> true | None -> false)
    |> Option.value_or_thunk ~default:(fun () -> None)
  in
  { where; search; data }

let get_query_specs global_env (query : query_declaration) =
  let _, query_id, query_type, _, body, model, permissions = query in
  let _, model_id = model in
  let permissions =
    match permissions with
    | Some permissions ->
        Some
          (List.map permissions ~f:(fun permission ->
               let _, id = permission in
               id))
    | None -> None
  in
  let return_type =
    (GlobalEnvironment.lookup global_env ~key:query_id
    |> GlobalEnvironment.get_query_value)
      .return_type
  in
  let query_args = get_query_args global_env model query_type body in
  {
    query_id;
    query_type;
    return_type;
    query_args;
    query_model = model_id;
    query_permissions = permissions;
  }

let group_queries queries =
  let get_groups groups query =
    let { return_type; _ } = query in
    let service_name = get_scalar_type return_type |> string_of_scalar_type in
    (if Hashtbl.mem groups service_name then
     let temp = Hashtbl.find_exn groups service_name in
     let group = query :: temp in
     Hashtbl.set groups ~key:service_name ~data:group
    else
      let group = [ query ] in
      Hashtbl.add_exn groups ~key:service_name ~data:group);
    groups
  in
  let groups = Hashtbl.create (module String) in
  List.fold_left ~f:get_groups ~init:groups queries

let generate_controllers_specs queries =
  let get_controller_function lst query =
    let { query_id; query_type; query_args; query_permissions; _ } = query in
    let function_type = QueryFormatter.string_of_query_type query_type in
    let required_args =
      let { where; search; data } = query_args in
      let requires_where = match where with Some _ -> true | None -> false in
      let requires_search =
        match search with Some _ -> true | None -> false
      in
      let requires_data = match data with Some _ -> true | None -> false in
      { requires_where; requires_search; requires_data }
    in
    let middlewares =
      match query_permissions with
      | Some permissions -> permissions
      | None -> []
    in
    let controller_function =
      { function_id = query_id; function_type; middlewares; required_args }
    in
    controller_function :: lst
  in
  List.fold_left ~init:[] ~f:get_controller_function queries

let generate_routes_specs queries =
  let get_route lst query =
    let { query_id; query_type; query_args; query_permissions; _ } = query in
    let route_type = QueryFormatter.string_of_query_type query_type in
    let { where; _ } = query_args in
    let route_param =
      match where with
      | Some where ->
          let id, _ = List.hd_exn where in
          Some id
      | None -> None
    in
    let middlewares =
      match query_permissions with
      | Some permissions -> permissions
      | None -> []
    in
    let route = { route_id = query_id; route_type; route_param; middlewares } in
    route :: lst
  in
  List.fold_left ~init:[] ~f:get_route queries

let generate_validators_specs queries =
  let get_validator lst query =
    let { query_id; query_args; _ } = query in
    let validator = { validator_id = query_id; fields = query_args } in
    validator :: lst
  in
  List.fold_left ~init:[] ~f:get_validator queries

let generate_server_specs global_env app_declaration query_declarations =
  let queries_specs =
    List.map query_declarations ~f:(fun query_declaration ->
        get_query_specs global_env query_declaration)
  in
  let groups = group_queries queries_specs in
  let controllers_specs =
    Hashtbl.mapi groups ~f:(fun ~key:_ ~data -> generate_controllers_specs data)
  in
  let routes_specs =
    Hashtbl.mapi groups ~f:(fun ~key:_ ~data -> generate_routes_specs data)
  in
  let validators_specs =
    Hashtbl.mapi groups ~f:(fun ~key:_ ~data -> generate_validators_specs data)
  in
  let auth_specs = get_auth_config app_declaration in
  { controllers_specs; routes_specs; validators_specs; auth_specs }
