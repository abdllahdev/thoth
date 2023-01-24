open Sys
open Core
open Jingoo
open Specs.Server_specs

let generate_service (service_specs : service_specs) : unit =
  let { name; service_functions } = service_specs in
  let service_file = getcwd () ^ "/templates/server/src/services/.service.js" in
  let service =
    Jg_template.from_file service_file
      ~models:
        [
          ( "service",
            Jg_types.Tobj
              [
                ("name", Jg_types.Tstr name);
                ( "service_functions",
                  Jg_types.Tlist
                    (List.map
                       ~f:(fun query ->
                         Jg_types.Tobj
                           [
                             ("id", Jg_types.Tstr query.id);
                             ("type", Jg_types.Tstr query.typ);
                           ])
                       service_functions) );
              ] );
        ]
  in
  print_string service

let generate_services (services : service_specs list) : unit =
  ignore (List.map ~f:generate_service services);
  let names =
    List.map ~f:(fun service -> Jg_types.Tstr service.name) services
  in
  let services_index_file =
    getcwd () ^ "/templates/server/src/services/index.js"
  in
  let services_index =
    Jg_template.from_file services_index_file
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  print_string services_index

let generate_server (server_specs : server_specs) : unit =
  generate_services server_specs.services
