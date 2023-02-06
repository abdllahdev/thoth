open Core
open Ast.Ast_types
open Ast.Pprinter

type service_function = { id : string; typ : string }

type service_specs = {
  name : string;
  service_functions : service_function list;
}

type controller_function = {
  id : string;
  typ : string;
  where : string option;
  filter : string list option;
  data : string list option;
}

type controller_specs = {
  name : string;
  controller_functions : controller_function list;
}

type route = { id : string; typ : string; where : string option }
type route_specs = { name : string; list : route list }

type server_specs = {
  services : service_specs list;
  controllers : controller_specs list;
  routes : route_specs list;
}

let group_queries queries =
  let groups_builder groups query =
    let _, _, _, _, _, models, _ = query in
    let model = List.hd_exn models in
    let _, service_name = model in
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
  List.fold_left ~f:groups_builder ~init:groups queries |> Hashtbl.data

let get_model_name queries =
  let _, _, _, _, _, models, _ = List.hd_exn queries in
  let model = List.hd_exn models in
  let _, name = model in
  String.lowercase name

let generate_service_specs queries =
  let name = get_model_name queries in
  let get_service_function lst query =
    let _, id, typ, _, _, _, _ = query in
    let typ = QueryPrinter.string_of_query_type typ in
    let service_function = { id; typ } in
    service_function :: lst
  in
  let service_functions =
    List.fold_left ~init:[] ~f:get_service_function queries
  in
  { name; service_functions }

let generate_controller_specs (queries : query_declaration list) =
  let name = get_model_name queries in
  let get_controller_function lst (query : query_declaration) =
    let _, id, typ, _, args, _, _ = query in
    let typ = QueryPrinter.string_of_query_type typ in

    let where =
      List.map args ~f:(function
        | Query.Where (_, field) -> Some field
        | _ -> None)
      |> List.find ~f:(function Some _ -> true | None -> false)
      |> Option.value_or_thunk ~default:(fun () -> None)
    in

    let filter =
      List.map args ~f:(function
        | Query.Filter (_, fields) -> Some fields
        | _ -> None)
      |> List.find ~f:(function Some _ -> true | None -> false)
      |> Option.value_or_thunk ~default:(fun () -> None)
    in

    let data =
      List.map args ~f:(function
        | Query.Data (_, fields) -> Some fields
        | _ -> None)
      |> List.find ~f:(function Some _ -> true | None -> false)
      |> Option.value_or_thunk ~default:(fun () -> None)
    in

    let controller_function = { id; typ; where; filter; data } in
    controller_function :: lst
  in
  let controller_functions =
    List.fold_left ~init:[] ~f:get_controller_function queries
  in
  { name; controller_functions }

let generate_routes_specs (queries : query_declaration list) =
  let name = get_model_name queries in
  let get_controller_function lst (query : query_declaration) =
    let _, id, typ, _, args, _, _ = query in
    let typ = QueryPrinter.string_of_query_type typ in

    let where =
      List.map args ~f:(function
        | Query.Where (_, field) -> Some field
        | _ -> None)
      |> List.find ~f:(function Some _ -> true | None -> false)
      |> Option.value_or_thunk ~default:(fun () -> None)
    in

    let route = { id; typ; where } in
    route :: lst
  in
  let list = List.fold_left ~init:[] ~f:get_controller_function queries in
  { name; list }

let generate_server_specs (queries : query_declaration list) =
  let groups = group_queries queries in
  let services =
    List.map groups ~f:(fun group -> generate_service_specs group)
  in
  let controllers =
    List.map groups ~f:(fun group -> generate_controller_specs group)
  in
  let routes = List.map groups ~f:(fun group -> generate_routes_specs group) in
  { services; controllers; routes }
