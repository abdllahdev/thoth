open Sys
open Core
open Jingoo
open Specs.Server_specs

let string_of_option = function Some str -> str | None -> ""
let list_of_option = function Some lst -> lst | None -> []

let generate_service (service_specs : service_specs) : unit =
  let { name; service_functions } = service_specs in
  let service_file = getcwd () ^ "/templates/server/src/services/.service.js" in
  ignore
    (Jg_template.from_file service_file
       ~models:
         [
           ( "service",
             Jg_types.Tobj
               [
                 ("name", Jg_types.Tstr name);
                 ( "service_functions",
                   Jg_types.Tlist
                     (List.map
                        ~f:(fun service_function ->
                          Jg_types.Tobj
                            [
                              ("id", Jg_types.Tstr service_function.id);
                              ("type", Jg_types.Tstr service_function.typ);
                            ])
                        service_functions) );
               ] );
         ])

let generate_services (services : service_specs list) : unit =
  ignore (List.map ~f:generate_service services);
  let names =
    List.map ~f:(fun element -> Jg_types.Tstr element.name) services
  in
  let services_index_file =
    getcwd () ^ "/templates/server/src/services/index.js"
  in
  ignore
    (Jg_template.from_file services_index_file
       ~models:[ ("names", Jg_types.Tlist names) ])

let generate_controller (controller_sepcs : controller_specs) : unit =
  let { name; controller_functions } = controller_sepcs in
  let controller_file =
    getcwd () ^ "/templates/server/src/controllers/.controller.js"
  in
  ignore
    (Jg_template.from_file controller_file
       ~models:
         [
           ( "controller",
             Jg_types.Tobj
               [
                 ("name", Jg_types.Tstr name);
                 ( "controller_functions",
                   Jg_types.Tlist
                     (List.map
                        ~f:(fun controller_function ->
                          Jg_types.Tobj
                            [
                              ("id", Jg_types.Tstr controller_function.id);
                              ("type", Jg_types.Tstr controller_function.typ);
                              ( "where",
                                Jg_types.Tstr
                                  (string_of_option controller_function.where)
                              );
                              ( "filter",
                                Jg_types.Tlist
                                  (List.map
                                     ~f:(fun field -> Jg_types.Tstr field)
                                     (list_of_option controller_function.filter))
                              );
                              ( "data",
                                Jg_types.Tlist
                                  (List.map
                                     ~f:(fun field -> Jg_types.Tstr field)
                                     (list_of_option controller_function.data))
                              );
                            ])
                        controller_functions) );
               ] );
         ])

let generate_controller (controllers : controller_specs list) : unit =
  ignore (List.map ~f:generate_controller controllers);
  let names =
    List.map ~f:(fun element -> Jg_types.Tstr element.name) controllers
  in
  let services_index_file =
    getcwd () ^ "/templates/server/src/controllers/index.js"
  in
  ignore
    (Jg_template.from_file services_index_file
       ~models:[ ("names", Jg_types.Tlist names) ])

let generate_route (route_specs : route_specs) : unit =
  let { name; list } = route_specs in
  let controller_file = getcwd () ^ "/templates/server/src/routes/.routes.js" in
  ignore
    (Jg_template.from_file controller_file
       ~models:
         [
           ( "route",
             Jg_types.Tobj
               [
                 ("name", Jg_types.Tstr name);
                 ( "list",
                   Jg_types.Tlist
                     (List.map
                        ~f:(fun element ->
                          Jg_types.Tobj
                            [
                              ("id", Jg_types.Tstr element.id);
                              ("type", Jg_types.Tstr element.typ);
                              ( "where",
                                Jg_types.Tstr (string_of_option element.where)
                              );
                            ])
                        list) );
               ] );
         ])

let generate_routes (routes : route_specs list) : unit =
  ignore (List.map ~f:generate_route routes);
  let names = List.map ~f:(fun element -> Jg_types.Tstr element.name) routes in
  let services_index_file =
    getcwd () ^ "/templates/server/src/routes/index.js"
  in
  ignore
    (Jg_template.from_file services_index_file
       ~models:[ ("names", Jg_types.Tlist names) ])

let generate_server (server_specs : server_specs) : unit =
  let { services; controllers; routes } = server_specs in
  generate_services services;
  generate_controller controllers;
  generate_routes routes
