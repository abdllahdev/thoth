open Jingoo
open Core
open Core_unix
open Specs.Server_specs
open File_generator

let string_of_option = function Some str -> str | None -> ""
let list_of_option = function Some lst -> lst | None -> []

let generate_controller name controller_functions =
  let controller_template =
    getcwd () ^ "/templates/server/src/controllers/template.jinja"
  in
  let controller_code =
    Jg_template.from_file controller_template
      ~models:
        [
          ("name", Jg_types.Tstr (String.uncapitalize name));
          ( "functions",
            Jg_types.Tlist
              (List.map controller_functions ~f:(fun controller_function ->
                   let { function_id; function_type; required_args } =
                     controller_function
                   in
                   let { requires_where; requires_search; requires_data } =
                     required_args
                   in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr function_id);
                       ("type", Jg_types.Tstr function_type);
                       ("requires_where", Jg_types.Tbool requires_where);
                       ("requires_search", Jg_types.Tbool requires_search);
                       ("requires_data", Jg_types.Tbool requires_data);
                     ])) );
        ]
  in
  let controller_file =
    getcwd () ^ "/.out/server/src/controllers/" ^ String.uncapitalize name
    ^ ".ts"
  in
  write_file controller_file controller_code

let generate_controllers controllers_specs auth_specs =
  Hashtbl.iteri controllers_specs ~f:(fun ~key ~data ->
      generate_controller key data);
  let names =
    let names =
      List.map (Hashtbl.keys controllers_specs) ~f:(fun name ->
          Jg_types.Tstr (String.uncapitalize name))
    in
    match auth_specs with
    | Some auth_specs ->
        let { user_model; _ } = auth_specs in
        names
        @ [
            Jg_types.Tstr "auth"; Jg_types.Tstr (String.uncapitalize user_model);
          ]
    | None -> names
  in
  let controllers_index_template =
    getcwd () ^ "/templates/server/src/controllers/index.jinja"
  in
  let controllers_index_code =
    Jg_template.from_file controllers_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let controller_index_file =
    getcwd () ^ "/.out/server/src/controllers/index.ts"
  in
  write_file controller_index_file controllers_index_code

let generate_route name routes =
  let route_template =
    getcwd () ^ "/templates/server/src/routes/template.jinja"
  in
  let route_code =
    Jg_template.from_file route_template
      ~models:
        [
          ("name", Jg_types.Tstr (String.uncapitalize name));
          ( "list",
            Jg_types.Tlist
              (List.map routes ~f:(fun route ->
                   let { route_id; route_type; route_param; middlewares } =
                     route
                   in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr route_id);
                       ("type", Jg_types.Tstr route_type);
                       ( "middlewares",
                         Jg_types.Tlist
                           (List.map middlewares ~f:(fun middleware ->
                                Jg_types.Tstr middleware)) );
                       ( "where",
                         match route_param with
                         | Some id -> Jg_types.Tstr id
                         | None -> Jg_types.Tnull );
                     ])) );
        ]
  in
  let route_file =
    getcwd () ^ "/.out/server/src/routes/" ^ String.uncapitalize name ^ ".ts"
  in
  write_file route_file route_code

let generate_routes routes_specs auth_specs =
  Hashtbl.iteri routes_specs ~f:(fun ~key ~data -> generate_route key data);
  let names =
    let names =
      List.map (Hashtbl.keys routes_specs) ~f:(fun name ->
          Jg_types.Tstr (String.uncapitalize name))
    in
    match auth_specs with
    | Some auth_specs ->
        let { user_model; _ } = auth_specs in
        names
        @ [
            Jg_types.Tstr "auth"; Jg_types.Tstr (String.uncapitalize user_model);
          ]
    | None -> names
  in
  let routes_index_template =
    getcwd () ^ "/templates/server/src/routes/index.jinja"
  in
  let routes_index_code =
    Jg_template.from_file routes_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let routes_index_file = getcwd () ^ "/.out/server/src/routes/index.ts" in
  write_file routes_index_file routes_index_code

let generate_validator name validators =
  let validator_template =
    getcwd () ^ "/templates/server/src/validators/template.jinja"
  in
  let validator_code =
    Jg_template.from_file validator_template
      ~models:
        [
          ("name", Jg_types.Tstr (String.uncapitalize name));
          ( "list",
            Jg_types.Tlist
              (List.map validators ~f:(fun validator ->
                   let { validator_id; fields } = validator in
                   let { where; search; data } = fields in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr validator_id);
                       ( "where",
                         match where with
                         | Some fields ->
                             Jg_types.Tlist
                               (List.map fields ~f:(fun field ->
                                    let id, types = field in
                                    Jg_types.Tobj
                                      [
                                        ("id", Jg_types.Tstr id);
                                        ( "type",
                                          Jg_types.Tlist
                                            (List.map types ~f:(fun typ ->
                                                 Jg_types.Tstr typ)) );
                                      ]))
                         | None -> Jg_types.Tnull );
                       ( "search",
                         match search with
                         | Some fields ->
                             Jg_types.Tlist
                               (List.map fields ~f:(fun field ->
                                    let id, types = field in
                                    Jg_types.Tobj
                                      [
                                        ("id", Jg_types.Tstr id);
                                        ( "type",
                                          Jg_types.Tlist
                                            (List.map types ~f:(fun typ ->
                                                 Jg_types.Tstr typ)) );
                                      ]))
                         | None -> Jg_types.Tnull );
                       ( "data",
                         match data with
                         | Some fields ->
                             Jg_types.Tlist
                               (List.map fields ~f:(fun field ->
                                    match field with
                                    | Field (id, types) ->
                                        Jg_types.Tobj
                                          [
                                            ( "field",
                                              Jg_types.Tobj
                                                [
                                                  ("id", Jg_types.Tstr id);
                                                  ( "type",
                                                    Jg_types.Tlist
                                                      (List.map types
                                                         ~f:(fun typ ->
                                                           Jg_types.Tstr typ))
                                                  );
                                                ] );
                                          ]
                                    | Object (id, Field (sub_id, types)) ->
                                        Jg_types.Tobj
                                          [
                                            ( "object",
                                              Jg_types.Tobj
                                                [
                                                  ("id", Jg_types.Tstr id);
                                                  ( "field",
                                                    Jg_types.Tobj
                                                      [
                                                        ( "id",
                                                          Jg_types.Tstr sub_id
                                                        );
                                                        ( "type",
                                                          Jg_types.Tlist
                                                            (List.map types
                                                               ~f:(fun typ ->
                                                                 Jg_types.Tstr
                                                                   typ)) );
                                                      ] );
                                                ] );
                                          ]
                                    | _ ->
                                        failwith
                                          "CompilationError: Something went \
                                           wrong"))
                         | None -> Jg_types.Tnull );
                     ])) );
        ]
  in
  let validator_file =
    getcwd () ^ "/.out/server/src/validators/" ^ String.uncapitalize name
    ^ ".ts"
  in
  write_file validator_file validator_code

let generate_validators validators_specs auth_specs =
  Hashtbl.iteri validators_specs ~f:(fun ~key ~data ->
      generate_validator key data);
  let names =
    let names =
      List.map (Hashtbl.keys validators_specs) ~f:(fun name ->
          Jg_types.Tstr (String.uncapitalize name))
    in
    match auth_specs with
    | Some auth_specs ->
        let { user_model; _ } = auth_specs in
        names
        @ [
            Jg_types.Tstr "auth"; Jg_types.Tstr (String.uncapitalize user_model);
          ]
    | None -> names
  in
  let validators_index_template =
    getcwd () ^ "/templates/server/src/validators/index.jinja"
  in
  let validators_index_code =
    Jg_template.from_file validators_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let validators_index_file =
    getcwd () ^ "/.out/server/src/validators/index.ts"
  in
  write_file validators_index_file validators_index_code

let generate_auth auth_specs =
  match auth_specs with
  | Some auth_specs ->
      let { user_model; id_field; username_field; password_field } =
        auth_specs
      in
      List.iter [ "controllers"; "routes"; "validators" ] ~f:(fun component ->
          List.iter
            [ "auth"; String.uncapitalize user_model ]
            ~f:(fun file ->
              let template =
                Fmt.str "%s/templates/server/src/%s/%s.jinja" (getcwd ())
                  component file
              in
              let code =
                Jg_template.from_file template
                  ~models:
                    [
                      ( "user_model",
                        Jg_types.Tstr (String.uncapitalize user_model) );
                      ("id_field", Jg_types.Tstr id_field);
                      ("username_field", Jg_types.Tstr username_field);
                      ("password_field", Jg_types.Tstr password_field);
                    ]
              in
              let output_file =
                Fmt.str "%s/.out/server/src/%s/%s.ts" (getcwd ()) component file
              in
              write_file output_file code))
  | None -> ()

let setup_server_folder =
  let destination = getcwd () ^ "/templates/server" in
  create_folder destination;
  system (Fmt.str "rm %s/.out/server/src/controllers/*" (getcwd ())) |> ignore;
  system (Fmt.str "rm %s/.out/server/src/routes/*" (getcwd ())) |> ignore;
  system (Fmt.str "rm %s/.out/server/src/validators/*" (getcwd ())) |> ignore

let generate_server server_specs =
  setup_server_folder;
  let { controllers_specs; routes_specs; validators_specs; auth_specs } =
    server_specs
  in
  generate_controllers controllers_specs auth_specs;
  generate_validators validators_specs auth_specs;
  generate_routes routes_specs auth_specs;
  generate_auth auth_specs
