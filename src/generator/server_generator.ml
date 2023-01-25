open Sys
open Core
open Jingoo
open Specs.Server_specs
open File_generator

let string_of_option = function Some str -> str | None -> ""
let list_of_option = function Some lst -> lst | None -> []

let generate_service (service_specs : service_specs) : unit =
  let { name; service_functions } = service_specs in
  let service_template = getcwd () ^ "/templates/server/services/.service.js" in
  let service_code =
    Jg_template.from_file service_template
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
        ]
  in
  let service_file =
    getcwd () ^ "/.out/server/src/services/" ^ name ^ ".service.js"
  in
  write_file service_file service_code

let generate_services (services : service_specs list) : unit =
  ignore (List.map ~f:generate_service services);
  let names =
    List.map ~f:(fun element -> Jg_types.Tstr element.name) services
  in
  let services_index_template =
    getcwd () ^ "/templates/server/services/index.js"
  in
  let services_index_code =
    Jg_template.from_file services_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let services_index_file = getcwd () ^ "/.out/server/src/services/index.js" in
  write_file services_index_file services_index_code

let generate_controller (controller_sepcs : controller_specs) : unit =
  let { name; controller_functions } = controller_sepcs in
  let controller_template =
    getcwd () ^ "/templates/server/controllers/.controller.js"
  in
  let controller_code =
    Jg_template.from_file controller_template
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
                                 (string_of_option controller_function.where) );
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
        ]
  in
  let controller_file =
    getcwd () ^ "/.out/server/src/controllers/" ^ name ^ ".controller.js"
  in
  write_file controller_file controller_code

let generate_controllers (controllers : controller_specs list) : unit =
  ignore (List.map ~f:generate_controller controllers);
  let names =
    List.map ~f:(fun element -> Jg_types.Tstr element.name) controllers
  in
  let controllers_index_template =
    getcwd () ^ "/templates/server/controllers/index.js"
  in
  let controllers_index_code =
    Jg_template.from_file controllers_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let controller_index_file =
    getcwd () ^ "/.out/server/src/controllers/index.js"
  in
  write_file controller_index_file controllers_index_code

let generate_route (route_specs : route_specs) : unit =
  let { name; list } = route_specs in
  let route_template = getcwd () ^ "/templates/server/routes/.routes.js" in
  let route_code =
    Jg_template.from_file route_template
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
                               Jg_types.Tstr (string_of_option element.where) );
                           ])
                       list) );
              ] );
        ]
  in
  let route_file =
    getcwd () ^ "/.out/server/src/routes/" ^ name ^ ".routes.js"
  in
  write_file route_file route_code

let generate_routes (routes : route_specs list) : unit =
  ignore (List.map ~f:generate_route routes);
  let names = List.map ~f:(fun element -> Jg_types.Tstr element.name) routes in
  let routes_index_file = getcwd () ^ "/templates/server/routes/index.js" in
  let routes_index_code =
    Jg_template.from_file routes_index_file
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let routes_index_file = getcwd () ^ "/.out/server/src/routes/index.js" in
  write_file routes_index_file routes_index_code

let generate_server (server_specs : server_specs) : unit =
  let destination = getcwd () ^ "/templates/server/server" in
  create_folder destination;
  let { services; controllers; routes } = server_specs in
  generate_services services;
  generate_controllers controllers;
  generate_routes routes
