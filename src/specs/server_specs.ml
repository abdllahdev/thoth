open Core
open Ast.Ast_types
open Ast.Pprinter
open Ast.Helper
open Type_checker.Environment

type typed_variable = string * typ

type query_args_specs = {
  where : typed_variable option;
  search : typed_variable list option;
  data : typed_variable list option;
}

type query_specs = {
  query_id : string;
  query_type : Query.typ;
  return_type : typ;
  query_args : query_args_specs;
  query_models : string list;
  query_permissions : string list option;
}

type validator_specs = {
  validator_id : string;
  where_field : (string * string) option;
  search_fields : (string * string) list option;
  data_fields : (string * string) list option;
}

type required_args = {
  requires_where : bool;
  requires_search : bool;
  requires_data : bool;
}

type controller_function_specs = {
  function_id : string;
  function_type : string;
  required_args : required_args;
}

type route_specs = {
  route_id : string;
  route_type : string;
  route_param : string option;
}

type server_specs = {
  controllers_table : (string, controller_function_specs list) Hashtbl.t;
  routes_table : (string, route_specs list) Hashtbl.t;
  validators_table : (string, validator_specs list) Hashtbl.t;
}

let get_query_args global_env models args =
  let model_id = List.nth_exn models 0 in
  let model_fields =
    GlobalEnvironment.lookup global_env ~key:model_id
    |> GlobalEnvironment.get_model_value
  in

  let where =
    List.map args ~f:(function
      | Query.Where (_, id) ->
          let arg_type = (LocalEnvironment.lookup model_fields ~key:id).typ in
          Some (id, arg_type)
      | _ -> None)
    |> List.find ~f:(function Some _ -> true | None -> false)
    |> Option.value_or_thunk ~default:(fun () -> None)
  in

  let search =
    List.map args ~f:(function
      | Query.Search (_, ids) ->
          Some
            (List.map ids ~f:(fun id ->
                 let arg_type =
                   (LocalEnvironment.lookup model_fields ~key:id).typ
                 in
                 (id, arg_type)))
      | _ -> None)
    |> List.find ~f:(function Some _ -> true | None -> false)
    |> Option.value_or_thunk ~default:(fun () -> None)
  in

  let data =
    List.map args ~f:(function
      | Query.Data (_, ids) ->
          Some
            (List.map ids ~f:(fun id ->
                 let arg_type =
                   (LocalEnvironment.lookup model_fields ~key:id).typ
                 in
                 (id, arg_type)))
      | _ -> None)
    |> List.find ~f:(function Some _ -> true | None -> false)
    |> Option.value_or_thunk ~default:(fun () -> None)
  in
  { where; search; data }

let get_query_specs global_env (query : query_declaration) =
  let _, query_id, query_type, _, args, models, permissions = query in
  let model_id =
    List.map models ~f:(fun model ->
        let _, id = model in
        id)
  in
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
  let query_args = get_query_args global_env model_id args in
  {
    query_id;
    query_type;
    return_type;
    query_args;
    query_models = model_id;
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
    let { query_id; query_type; query_args; _ } = query in
    let function_type = QueryPrinter.string_of_query_type query_type in
    let required_args =
      let { where; search; data } = query_args in
      let requires_where = match where with Some _ -> true | None -> false in
      let requires_search =
        match search with Some _ -> true | None -> false
      in
      let requires_data = match data with Some _ -> true | None -> false in
      { requires_where; requires_search; requires_data }
    in
    let controller_function =
      { function_id = query_id; function_type; required_args }
    in
    controller_function :: lst
  in
  List.fold_left ~init:[] ~f:get_controller_function queries

let generate_routes_specs queries =
  let get_route lst query =
    let { query_id; query_type; query_args; _ } = query in
    let route_type = QueryPrinter.string_of_query_type query_type in
    let { where; _ } = query_args in
    let route_param =
      match where with Some (id, _) -> Some id | None -> None
    in
    let route = { route_id = query_id; route_type; route_param } in
    route :: lst
  in
  List.fold_left ~init:[] ~f:get_route queries

let generate_validators_specs queries =
  let get_validator lst query =
    let { query_id; query_args; _ } = query in
    let { where; search; data } = query_args in
    let where_field =
      match where with
      | Some (field, typ) -> Some (field, string_of_type typ)
      | None -> None
    in
    let search_fields =
      match search with
      | Some search ->
          Some
            (List.map search ~f:(fun (field, typ) ->
                 (field, string_of_type typ)))
      | None -> None
    in
    let data_fields =
      match data with
      | Some data ->
          Some
            (List.map data ~f:(fun (field, typ) -> (field, string_of_type typ)))
      | None -> None
    in
    let validator =
      { validator_id = query_id; where_field; search_fields; data_fields }
    in
    validator :: lst
  in
  List.fold_left ~init:[] ~f:get_validator queries

let generate_server_specs global_env query_declarations =
  let queries_specs =
    List.map query_declarations ~f:(fun query_declaration ->
        get_query_specs global_env query_declaration)
  in
  let groups = group_queries queries_specs in
  let controllers_table =
    Hashtbl.mapi groups ~f:(fun ~key:_ ~data -> generate_controllers_specs data)
  in
  let routes_table =
    Hashtbl.mapi groups ~f:(fun ~key:_ ~data -> generate_routes_specs data)
  in
  let validators_table =
    Hashtbl.mapi groups ~f:(fun ~key:_ ~data -> generate_validators_specs data)
  in
  { controllers_table; routes_table; validators_table }
