open Core
open Ast.Ast_types
open Ast.Pprinter

type query_specs = { id : string; typ : string }
type service_specs = { name : string; service_functions : query_specs list }

type controller_specs = {
  name : string;
  controller_function : query_specs list;
}

type routes_specs = { name : string; route_function : query_specs list }
type server_specs = { services : service_specs list }

let group_queries (queries : query_declaration list) :
    query_declaration list list =
  let groups_builder groups query =
    let _, _, body = query in
    let _, _, models, _ = body in
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

let generate_service_specs (queries : query_declaration list) : service_specs =
  let name =
    let _, _, body = List.hd_exn queries in
    let _, _, models, _ = body in
    let model = List.hd_exn models in
    let _, name = model in
    String.lowercase name
  in

  let get_type lst query =
    let _, id, body = query in
    let typ, _, _, _ = body in
    let typ = QueryPrinter.string_of_query_type typ in
    let query = { id; typ } in
    query :: lst
  in

  let service_functions = List.fold_left ~init:[] ~f:get_type queries in
  { name; service_functions }

let generate_server_specs (queries : query_declaration list) : server_specs =
  let groups = group_queries queries in
  let services =
    List.map ~f:(fun group -> generate_service_specs group) groups
  in
  { services }
