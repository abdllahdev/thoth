open Sys
open Core
open Jingoo
open Specs.Server_specs

let generate_service (service_specs : service_specs) : unit =
  let { name; queries } = service_specs in
  let service_file = getcwd () ^ "/templates/server/src/services/.service.js" in
  let service =
    Jg_template.from_file service_file
      ~models:
        [
          ( "service",
            Jg_types.Tobj
              [
                ("name", Jg_types.Tstr name);
                ( "queries",
                  Jg_types.Tlist
                    (List.map
                       ~f:(fun query ->
                         Jg_types.Tobj
                           [
                             ("id", Jg_types.Tstr query.id);
                             ("type", Jg_types.Tstr query.typ);
                           ])
                       queries) );
              ] );
        ]
  in
  print_string service

let generate_server (server_specs : server_specs) : unit =
  let { services } = server_specs in
  ignore (List.map ~f:generate_service services)
