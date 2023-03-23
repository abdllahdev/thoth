open Jingoo
open Core
open Core_unix
open Error_handler.Handler
open Specs.Server_specs
open File_generator
open Ast.Ast_types

let string_of_option = function Some str -> str | None -> ""
let list_of_option = function Some lst -> lst | None -> []

let generate_controller output_dir name imports controller_functions
    find_unique_requires_owns_entry find_many_requires_owns_entry =
  let controller_template =
    getcwd () ^ "/templates/server/src/controllers/template.jinja"
  in
  let controller_code =
    Jg_template.from_file controller_template
      ~models:
        [
          ("name", Jg_types.Tstr (String.uncapitalize name));
          ( "imports",
            Jg_types.Tlist
              (List.map imports ~f:(fun import -> Jg_types.Tstr import)) );
          ( "find_unique_requires_owns_entry",
            Jg_types.Tbool find_unique_requires_owns_entry );
          ( "find_many_requires_owns_entry",
            Jg_types.Tbool find_many_requires_owns_entry );
          ( "functions",
            Jg_types.Tlist
              (List.map controller_functions ~f:(fun controller_function ->
                   let {
                     function_id;
                     function_type;
                     required_args;
                     middlewares;
                     custom_fn;
                     includes;
                   } =
                     controller_function
                   in
                   match function_type with
                   | "custom" ->
                       Jg_types.Tobj
                         [
                           ("id", Jg_types.Tstr function_id);
                           ("type", Jg_types.Tstr function_type);
                           ( "custom_fn",
                             Jg_types.Tstr (Option.value_exn custom_fn) );
                         ]
                   | _ ->
                       let ownsEntry =
                         match
                           List.filter middlewares ~f:(fun middleware ->
                               if String.equal middleware "OwnsEntry" then true
                               else false)
                           |> List.hd
                         with
                         | Some _ -> true
                         | None -> false
                       in
                       let { requires_where; requires_search; requires_data } =
                         required_args
                       in
                       Jg_types.Tobj
                         [
                           ("id", Jg_types.Tstr function_id);
                           ("type", Jg_types.Tstr function_type);
                           ("owns_entry", Jg_types.Tbool ownsEntry);
                           ( "includes",
                             Jg_types.Tlist
                               (match includes with
                               | Some includes ->
                                   List.map includes ~f:(fun field ->
                                       Jg_types.Tstr field)
                               | None -> []) );
                           ("requires_where", Jg_types.Tbool requires_where);
                           ("requires_search", Jg_types.Tbool requires_search);
                           ("requires_data", Jg_types.Tbool requires_data);
                         ])) );
        ]
  in
  let controller_file =
    Fmt.str "%s/%s/server/src/controllers/%s.ts" (getcwd ()) output_dir
      (String.uncapitalize name)
  in
  write_file controller_file controller_code

let generate_controllers output_dir controllers_specs auth_specs =
  Hashtbl.iteri controllers_specs ~f:(fun ~key ~data ->
      let ( imports,
            find_unique_requires_owns_entry,
            find_many_requires_owns_entry,
            funcs ) =
        data
      in
      generate_controller output_dir key imports funcs
        find_unique_requires_owns_entry find_many_requires_owns_entry);
  let names =
    let names =
      List.map (Hashtbl.keys controllers_specs) ~f:(fun name ->
          Jg_types.Tstr (String.uncapitalize name))
    in
    match auth_specs with
    | Some _ -> names @ [ Jg_types.Tstr "auth" ]
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
    Fmt.str "%s/%s/server/src/controllers/index.ts" (getcwd ()) output_dir
  in
  write_file controller_index_file controllers_index_code

let generate_route output_dir name routes =
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
                   let {
                     route_id;
                     http_method;
                     custom_route;
                     route_param;
                     middlewares;
                     route_type;
                     _;
                   } =
                     route
                   in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr route_id);
                       ("http_method", Jg_types.Tstr http_method);
                       ( "custom_route",
                         match custom_route with
                         | Some custom_route -> Jg_types.Tstr custom_route
                         | None -> Jg_types.Tnull );
                       ("route_type", Jg_types.Tstr route_type);
                       ( "middlewares",
                         Jg_types.Tlist
                           (List.map middlewares ~f:(fun middleware ->
                                Jg_types.Tstr (String.uncapitalize middleware)))
                       );
                       ( "where",
                         match route_param with
                         | Some id -> Jg_types.Tstr id
                         | None -> Jg_types.Tnull );
                     ])) );
        ]
  in
  let route_file =
    Fmt.str "%s/%s/server/src/routes/%s.ts" (getcwd ()) output_dir
      (String.uncapitalize name)
  in
  write_file route_file route_code

let generate_routes output_dir routes_specs auth_specs =
  Hashtbl.iteri routes_specs ~f:(fun ~key ~data ->
      generate_route output_dir key data);
  let names =
    let names =
      List.map (Hashtbl.keys routes_specs) ~f:(fun name ->
          Jg_types.Tstr (String.uncapitalize name))
    in
    match auth_specs with
    | Some _ -> names @ [ Jg_types.Tstr "auth" ]
    | None -> names
  in
  let routes_index_template =
    getcwd () ^ "/templates/server/src/routes/index.jinja"
  in
  let routes_index_code =
    Jg_template.from_file routes_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let routes_index_file =
    Fmt.str "%s/%s/server/src/routes/index.ts" (getcwd ()) output_dir
  in
  write_file routes_index_file routes_index_code

let generate_validator output_dir name validators =
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
                   let { where; search; data; _ } = fields in
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
                                    | _ -> raise_compiler_error ()))
                         | None -> Jg_types.Tnull );
                     ])) );
        ]
  in
  let validator_file =
    Fmt.str "%s/%s/server/src/validators/%s.ts" (getcwd ()) output_dir
      (String.uncapitalize name)
  in
  write_file validator_file validator_code

let generate_validators output_dir validators_specs =
  Hashtbl.iteri validators_specs ~f:(fun ~key ~data ->
      generate_validator output_dir key data);
  let names =
    List.map (Hashtbl.keys validators_specs) ~f:(fun name ->
        Jg_types.Tstr (String.uncapitalize name))
  in
  let validators_index_template =
    getcwd () ^ "/templates/server/src/validators/index.jinja"
  in
  let validators_index_code =
    Jg_template.from_file validators_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let validators_index_file =
    Fmt.str "%s/%s/server/src/validators/index.ts" (getcwd ()) output_dir
  in
  write_file validators_index_file validators_index_code

let generate_auth output_dir auth_specs =
  match auth_specs with
  | Some
      {
        user_model;
        id_field;
        username_field;
        password_field;
        is_online_field;
        last_active_field;
        _;
      } ->
      List.iter [ "controllers"; "routes"; "validators"; "utils" ]
        ~f:(fun component ->
          if not (String.equal component "validators") then
            let template =
              Fmt.str "%s/templates/server/src/%s/auth.jinja" (getcwd ())
                component
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
                    ("is_online_field", Jg_types.Tstr is_online_field);
                    ("last_active_field", Jg_types.Tstr last_active_field);
                  ]
            in
            let output_file =
              Fmt.str "%s/%s/server/src/%s/auth.ts" (getcwd ()) output_dir
                component
            in
            write_file output_file code)
  | None -> ()

let generate_server_file output_dir auth_specs =
  let template = Fmt.str "%s/templates/server/src/app.jinja" (getcwd ()) in
  let code =
    match auth_specs with
    | Some _ ->
        Jg_template.from_file template
          ~models:[ ("requires_auth", Jg_types.Tbool true) ]
    | None ->
        Jg_template.from_file template
          ~models:[ ("requires_auth", Jg_types.Tbool false) ]
  in
  let output_file = Fmt.str "%s/%s/server/src/app.ts" (getcwd ()) output_dir in
  write_file output_file code

let generate_package_file output_dir client_deps =
  let app_index_template = getcwd () ^ "/templates/server/package.jinja" in
  let app_index_code =
    Jg_template.from_file app_index_template
      ~models:
        [
          ( "deps",
            match client_deps with
            | Some deps ->
                Jg_types.Tlist
                  (List.map deps ~f:(fun (id, version) ->
                       Jg_types.Tobj
                         [
                           ("id", Jg_types.Tstr id);
                           ("version", Jg_types.Tstr version);
                         ]))
            | None -> Jg_types.Tnull );
        ]
  in
  let app_index_file =
    Fmt.str "%s/%s/server/package.json" (getcwd ()) output_dir
  in
  write_file app_index_file app_index_code

let generate_env_file output_dir server_port db_name =
  let app_index_template = getcwd () ^ "/templates/server/.env.jinja" in
  let app_index_code =
    Jg_template.from_file app_index_template
      ~models:
        [
          ("server_port", Jg_types.Tint server_port);
          ("db_name", Jg_types.Tstr db_name);
        ]
  in
  let app_index_file = Fmt.str "%s/%s/server/.env" (getcwd ()) output_dir in
  write_file app_index_file app_index_code

let setup_server_folder output_dir =
  system (Fmt.str "rm %s/%s/server/src/controllers/*" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/server/src/routes/*" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/server/src/validators/*" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/server/src/utils/auth.jinja" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/server/src/app.jinja" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/server/.env.jinja" (getcwd ()) output_dir) |> ignore;
  system (Fmt.str "rm %s/%s/server/package.jinja" (getcwd ()) output_dir)
  |> ignore

let generate_server server_specs output_dir server_port db_name =
  setup_server_folder output_dir;
  let {
    controllers_specs;
    routes_specs;
    validators_specs;
    auth_specs;
    server_deps;
  } =
    server_specs
  in
  generate_package_file output_dir server_deps;
  generate_env_file output_dir server_port db_name;
  generate_server_file output_dir auth_specs;
  generate_controllers output_dir controllers_specs auth_specs;
  generate_validators output_dir validators_specs;
  generate_routes output_dir routes_specs auth_specs;
  generate_auth output_dir auth_specs
