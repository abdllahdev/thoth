open Core
open Core_unix
open Jingoo
open Specs.Client_specs
open File_generator

let generate_general_component output_dir general_component_specs =
  let general_component_template =
    getcwd ()
    ^ "/templates/client/src/components/general_component_template.jinja"
  in
  let { id; args; imported_types; imported_components; render_expression } =
    general_component_specs
  in
  let general_component_code =
    Jg_template.from_file general_component_template
      ~models:
        [
          ( "imported_components",
            Jg_types.Tlist
              (List.map imported_components ~f:(fun imported_component ->
                   Jg_types.Tstr imported_component)) );
          ( "imported_types",
            Jg_types.Tlist
              (List.map imported_types ~f:(fun imported_types ->
                   Jg_types.Tstr imported_types)) );
          ("id", Jg_types.Tstr id);
          ( "args",
            Jg_types.Tlist
              (List.map args ~f:(fun arg ->
                   let arg_id, arg_type = arg in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr arg_id);
                       ("type", Jg_types.Tstr arg_type);
                     ])) );
          ("render_expression", Jg_types.Tstr render_expression);
        ]
  in
  let general_component_file =
    Fmt.str "%s/%s/client/src/components/%s.tsx" (getcwd ()) output_dir id
  in
  write_file general_component_file general_component_code

let generate_find_component output_dir find_component_specs =
  let find_component_template =
    getcwd () ^ "/templates/client/src/components/find_component_template.jinja"
  in
  let {
    id;
    model;
    find_func;
    owns_entry;
    func_model;
    requires_auth;
    where_arg;
    search_arg;
    args;
    func_type;
    result_variable;
    result_type;
    result_scalar_type;
    imported_components;
    on_error;
    on_loading;
    render_expression;
  } =
    find_component_specs
  in
  let find_component_code =
    Jg_template.from_file find_component_template
      ~models:
        [
          ( "imported_components",
            Jg_types.Tlist
              (List.map imported_components ~f:(fun imported_component ->
                   Jg_types.Tstr imported_component)) );
          ("id", Jg_types.Tstr id);
          ( "where_arg",
            match where_arg with
            | Some where_arg -> Jg_types.Tstr where_arg
            | None -> Jg_types.Tnull );
          ( "search_arg",
            match search_arg with
            | Some search_arg ->
                Jg_types.Tlist
                  (List.map search_arg ~f:(fun (id, value) ->
                       Jg_types.Tobj
                         [
                           ("id", Jg_types.Tstr id);
                           ("value", Jg_types.Tstr value);
                         ]))
            | None -> Jg_types.Tnull );
          ("func_model", Jg_types.Tstr func_model);
          ("requires_auth", Jg_types.Tbool requires_auth);
          ("owns_entry", Jg_types.Tbool owns_entry);
          ( "args",
            Jg_types.Tlist
              (List.map args ~f:(fun arg ->
                   let arg_id, arg_type = arg in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr arg_id);
                       ("type", Jg_types.Tstr arg_type);
                     ])) );
          ( "model",
            match model with
            | Some model -> Jg_types.Tstr (String.uncapitalize model)
            | None -> Jg_types.Tnull );
          ("find_func", Jg_types.Tstr find_func);
          ("func_type", Jg_types.Tstr func_type);
          ( "variable",
            Jg_types.Tobj
              [
                ("id", Jg_types.Tstr result_variable);
                ("type", Jg_types.Tstr result_type);
                ("scalar_type", Jg_types.Tstr result_scalar_type);
              ] );
          ("on_error", Jg_types.Tstr on_error);
          ("on_loading", Jg_types.Tstr on_loading);
          ("render_expression", Jg_types.Tstr render_expression);
        ]
  in
  let find_component_file =
    Fmt.str "%s/%s/client/src/components/%s.tsx" (getcwd ()) output_dir id
  in
  write_file find_component_file find_component_code

let generate_action_form_component ?on_success_redirect_to ?on_fail_redirect_to
    output_dir action_form_component_specs =
  let generate_action_form_template =
    getcwd ()
    ^ "/templates/client/src/components/action_form_component_template.jinja"
  in
  let {
    id;
    args;
    requires_auth;
    func_type;
    where_arg;
    global_style;
    form_validation_scheme;
    form_inputs;
    form_button;
    action_func;
  } =
    action_form_component_specs
  in
  let action_form_component_code =
    Jg_template.from_file generate_action_form_template
      ~models:
        [
          ("id", Jg_types.Tstr id);
          ( "where_arg",
            match where_arg with
            | Some where_arg -> Jg_types.Tstr where_arg
            | None -> Jg_types.Tnull );
          ("requires_auth", Jg_types.Tbool requires_auth);
          ( "args",
            Jg_types.Tlist
              (List.map args ~f:(fun arg ->
                   let arg_id, arg_type = arg in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr arg_id);
                       ("type", Jg_types.Tstr arg_type);
                     ])) );
          ("type", Jg_types.Tstr func_type);
          ( "form_validation_scheme",
            Jg_types.Tlist
              (List.map form_validation_scheme ~f:(fun rule ->
                   match rule with
                   | Some (id, rule) ->
                       Jg_types.Tobj
                         [
                           ("id", Jg_types.Tstr id); ("rule", Jg_types.Tstr rule);
                         ]
                   | None -> Jg_types.Tnull)) );
          ( "global_style",
            Jg_types.Tlist
              (match global_style with
              | Some global_style ->
                  List.map global_style ~f:(fun style ->
                      let id, style = style in
                      Jg_types.Tobj
                        [
                          ("id", Jg_types.Tstr id);
                          ("value", Jg_types.Tstr style);
                        ])
              | None -> []) );
          ("action_func", Jg_types.Tstr action_func);
          ( "form_inputs",
            Jg_types.Tlist
              (List.map form_inputs ~f:(fun form_input ->
                   let { wrapper_style; label_attrs; input_attrs } =
                     form_input
                   in
                   Jg_types.Tobj
                     [
                       ( "wrapper_style",
                         Jg_types.Tstr
                           (match wrapper_style with
                           | Some wrapper_style ->
                               let _, wrapper_style = wrapper_style in
                               wrapper_style
                           | None -> "") );
                       ( "label_attrs",
                         Jg_types.Tlist
                           (match label_attrs with
                           | Some label_attrs ->
                               List.map label_attrs ~f:(fun label_attr ->
                                   let name, value = label_attr in
                                   Jg_types.Tobj
                                     [
                                       ("name", Jg_types.Tstr name);
                                       ("value", Jg_types.Tstr value);
                                     ])
                           | None -> []) );
                       ( "input_attrs",
                         Jg_types.Tlist
                           (List.map input_attrs ~f:(fun input_attr ->
                                let name, value = input_attr in
                                Jg_types.Tobj
                                  [
                                    ("name", Jg_types.Tstr name);
                                    ("value", Jg_types.Tstr value);
                                  ])) );
                     ])) );
          ( "form_button",
            Jg_types.Tlist
              (List.map form_button ~f:(fun (attr_name, attr_value) ->
                   Jg_types.Tobj
                     [
                       ("name", Jg_types.Tstr attr_name);
                       ("value", Jg_types.Tstr attr_value);
                     ])) );
          (match on_success_redirect_to with
          | Some on_success_redirect_to ->
              ("on_success_redirect_to", Jg_types.Tstr on_success_redirect_to)
          | None -> ("", Jg_types.Tstr ""));
          (match on_fail_redirect_to with
          | Some on_fail_redirect_to ->
              ("on_fail_redirect_to", Jg_types.Tstr on_fail_redirect_to)
          | None -> ("", Jg_types.Tstr ""));
        ]
  in
  let action_form_component_file =
    Fmt.str "%s/%s/client/src/components/%s.tsx" (getcwd ()) output_dir id
  in
  write_file action_form_component_file action_form_component_code

let generate_action_button_component ?on_success_redirect_to
    ?on_fail_redirect_to output_dir action_button_component_specs =
  let action_button_component_template =
    getcwd ()
    ^ "/templates/client/src/components/action_button_component_template.jinja"
  in
  let {
    id;
    args;
    requires_auth;
    func_type;
    where_arg;
    form_button;
    action_func;
  } =
    action_button_component_specs
  in
  let action_button_component_code =
    Jg_template.from_file action_button_component_template
      ~models:
        [
          ("id", Jg_types.Tstr id);
          ( "where_arg",
            match where_arg with
            | Some where_arg -> Jg_types.Tstr where_arg
            | None -> Jg_types.Tnull );
          ("requires_auth", Jg_types.Tbool requires_auth);
          ( "args",
            Jg_types.Tlist
              (List.map args ~f:(fun arg ->
                   let arg_id, arg_type = arg in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr arg_id);
                       ("type", Jg_types.Tstr arg_type);
                     ])) );
          ("action_func", Jg_types.Tstr action_func);
          ("type", Jg_types.Tstr func_type);
          ( "form_button",
            Jg_types.Tlist
              (List.map form_button ~f:(fun (attr_name, attr_value) ->
                   Jg_types.Tobj
                     [
                       ("name", Jg_types.Tstr attr_name);
                       ("value", Jg_types.Tstr attr_value);
                     ])) );
          (match on_success_redirect_to with
          | Some on_success_redirect_to ->
              ("on_success_redirect_to", Jg_types.Tstr on_success_redirect_to)
          | None -> ("", Jg_types.Tstr ""));
          (match on_fail_redirect_to with
          | Some on_fail_redirect_to ->
              ("on_fail_redirect_to", Jg_types.Tstr on_fail_redirect_to)
          | None -> ("", Jg_types.Tstr ""));
        ]
  in
  let action_button_component_file =
    Fmt.str "%s/%s/client/src/components/%s.tsx" (getcwd ()) output_dir id
  in
  write_file action_button_component_file action_button_component_code

let generate_page output_dir page_specs =
  let page_template =
    getcwd () ^ "/templates/client/src/pages/template.jinja"
  in
  let { id; route; imported_components; render_expression; requires_auth } =
    page_specs
  in
  let page_code =
    Jg_template.from_file page_template
      ~models:
        [
          ( "imported_components",
            Jg_types.Tlist
              (List.map imported_components ~f:(fun imported_component ->
                   Jg_types.Tstr imported_component)) );
          ("id", Jg_types.Tstr id);
          ( "on_fail_redirect_to",
            Jg_types.Tstr
              (match requires_auth with
              | Some requires_auth -> requires_auth
              | None -> "") );
          ("render_expression", Jg_types.Tstr render_expression);
        ]
  in
  let page_file =
    let splitted_route = String.split ~on:'/' route in
    let parameter_regex = Str.regexp "\\[[A-Za-z]+\\]" in
    let tail = List.nth_exn splitted_route (List.length splitted_route - 1) in
    let is_parameter = Str.string_match parameter_regex tail 0 in
    let path =
      String.concat ~sep:"/"
        ([ "/client/src/pages" ] @ List.slice splitted_route 1 0)
    in
    make_directory output_dir path;
    if is_parameter then
      Fmt.str "%s/%s/client/src/pages/%s.tsx" (getcwd ()) output_dir route
    else
      Fmt.str "%s/%s/client/src/pages/%s/index.tsx" (getcwd ()) output_dir route
  in
  write_file page_file page_code

let generate_type output_dir id type_specs =
  let type_template =
    getcwd () ^ "/templates/client/src/types/template.jinja"
  in
  let { imported_types; types } = type_specs in
  let types = Hashtbl.to_alist types in
  let type_code =
    Jg_template.from_file type_template
      ~models:
        [
          ( "imported_types",
            Jg_types.Tlist
              (List.map imported_types ~f:(fun import -> Jg_types.Tstr import))
          );
          ("id", Jg_types.Tstr id);
          ( "types",
            Jg_types.Tlist
              (List.map types ~f:(fun element ->
                   let id, typ = element in
                   Jg_types.Tobj
                     [ ("id", Jg_types.Tstr id); ("type", Jg_types.Tstr typ) ]))
          );
        ]
  in
  let type_file =
    Fmt.str "%s/%s/client/src/types/%s.ts" (getcwd ()) output_dir
      (String.uncapitalize id)
  in
  write_file type_file type_code

let generate_types output_dir types_specs auth_specs =
  let names =
    let names =
      List.map (Hashtbl.keys types_specs) ~f:(fun name ->
          Jg_types.Tstr (String.uncapitalize name))
    in
    match auth_specs with
    | Some auth_specs ->
        let { user_model; _ } = auth_specs in
        names @ [ Jg_types.Tstr (String.uncapitalize user_model) ]
    | None -> names
  in
  Hashtbl.iteri types_specs ~f:(fun ~key ~data ->
      generate_type output_dir key data);
  let types_index_template =
    getcwd () ^ "/templates/client/src/types/index.jinja"
  in
  let types_index_code =
    Jg_template.from_file types_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let types_index_file =
    Fmt.str "%s/%s/client/src/types/index.ts" (getcwd ()) output_dir
  in
  write_file types_index_file types_index_code

let generate_auth output_dir auth_specs =
  match auth_specs with
  | Some auth_specs ->
      let {
        signup_form;
        login_form;
        logout_button;
        user_model;
        id_field;
        username_field;
        on_success_redirect_to;
        on_fail_redirect_to;
      } =
        auth_specs
      in
      let generate_user_type () =
        let user_type_template =
          getcwd () ^ "/templates/client/src/types/user_type.jinja"
        in
        let types_user_type_code =
          Jg_template.from_file user_type_template
            ~models:
              [
                ("user_model", Jg_types.Tstr user_model);
                ("id_field", Jg_types.Tstr id_field);
                ("username_field", Jg_types.Tstr username_field);
              ]
        in
        let user_type_file =
          Fmt.str "%s/%s/client/src/types/user.ts" (getcwd ()) output_dir
        in
        write_file user_type_file types_user_type_code
      in
      let generate_auth_service () =
        let auth_service_template =
          getcwd () ^ "/templates/client/src/services/auth.jinja"
        in
        let auth_service_code = Jg_template.from_file auth_service_template in
        let auth_service_file =
          Fmt.str "%s/%s/client/src/services/auth.ts" (getcwd ()) output_dir
        in
        write_file auth_service_file auth_service_code
      in
      generate_action_form_component output_dir signup_form
        ~on_success_redirect_to ~on_fail_redirect_to;
      generate_action_form_component output_dir login_form
        ~on_success_redirect_to ~on_fail_redirect_to;
      generate_action_button_component output_dir logout_button
        ~on_success_redirect_to ~on_fail_redirect_to;
      generate_user_type ();
      generate_auth_service ()
  | None -> ()

let generate_custom_component output_dir custom_components_specs =
  let custom_component_template =
    getcwd ()
    ^ "/templates/client/src/components/custom_component_template.jinja"
  in
  let { id; args; imports; fn } = custom_components_specs in
  let custom_component_code =
    Jg_template.from_file custom_component_template
      ~models:
        [
          ("id", Jg_types.Tstr id);
          ( "args",
            Jg_types.Tlist
              (List.map args ~f:(fun arg ->
                   let arg_id, arg_type = arg in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr arg_id);
                       ("type", Jg_types.Tstr arg_type);
                     ])) );
          ( "imports",
            match imports with
            | Some imports -> Jg_types.Tstr imports
            | None -> Jg_types.Tnull );
          ("fn", Jg_types.Tstr fn);
        ]
  in
  let custom_component_file =
    Fmt.str "%s/%s/client/src/components/%s.tsx" (getcwd ()) output_dir id
  in
  write_file custom_component_file custom_component_code

let generate_service output_dir service_specs =
  let id, funcs = service_specs in
  let service_template =
    getcwd () ^ "/templates/client/src/services/template.jinja"
  in
  let service_component_code =
    Jg_template.from_file service_template
      ~models:
        [
          ( "functions",
            Jg_types.Tlist
              (List.map funcs ~f:(fun func ->
                   let {
                     func_id;
                     http_method;
                     route;
                     requires_where;
                     requires_search;
                     requires_auth;
                     func_type;
                   } =
                     func
                   in
                   Jg_types.Tobj
                     [
                       ("id", Jg_types.Tstr func_id);
                       ("requires_where", Jg_types.Tbool requires_where);
                       ("requires_search", Jg_types.Tbool requires_search);
                       ("http_method", Jg_types.Tstr http_method);
                       ("type", Jg_types.Tstr func_type);
                       ("route", Jg_types.Tstr route);
                       ("requires_auth", Jg_types.Tbool requires_auth);
                     ])) );
        ]
  in
  let service_component_file =
    Fmt.str "%s/%s/client/src/services/%s.ts" (getcwd ()) output_dir id
  in
  write_file service_component_file service_component_code

let generate_services output_dir services_specs =
  List.iter services_specs ~f:(fun service ->
      generate_service output_dir service);
  let names =
    List.map services_specs ~f:(fun (name, _) ->
        Jg_types.Tstr (String.uncapitalize name))
  in
  let services_index_template =
    getcwd () ^ "/templates/client/src/services/index.jinja"
  in
  let services_index_code =
    Jg_template.from_file services_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let services_index_file =
    Fmt.str "%s/%s/client/src/services/index.ts" (getcwd ()) output_dir
  in
  write_file services_index_file services_index_code

let generate_index_file output_dir app_title =
  let app_index_template = getcwd () ^ "/templates/client/index.jinja" in
  let app_index_code =
    Jg_template.from_file app_index_template
      ~models:[ ("app_title", Jg_types.Tstr app_title) ]
  in
  let app_index_file =
    Fmt.str "%s/%s/client/index.html" (getcwd ()) output_dir
  in
  write_file app_index_file app_index_code

let generate_env_file output_dir server_port =
  let app_index_template = getcwd () ^ "/templates/client/.env.jinja" in
  let app_index_code =
    Jg_template.from_file app_index_template
      ~models:[ ("server_port", Jg_types.Tint server_port) ]
  in
  let app_index_file = Fmt.str "%s/%s/client/.env" (getcwd ()) output_dir in
  write_file app_index_file app_index_code

let generate_package_file output_dir client_deps =
  let app_index_template = getcwd () ^ "/templates/client/package.jinja" in
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
    Fmt.str "%s/%s/client/package.json" (getcwd ()) output_dir
  in
  write_file app_index_file app_index_code

let generate_main_file output_dir auth_specs server_port =
  let main_template =
    Fmt.str "%s/templates/client/src/main.jinja" (getcwd ())
  in
  let code =
    match auth_specs with
    | Some _ ->
        Jg_template.from_file main_template
          ~models:
            [
              ("requires_auth", Jg_types.Tbool true);
              ("server_port", Jg_types.Tint server_port);
            ]
    | None ->
        Jg_template.from_file main_template
          ~models:[ ("requires_auth", Jg_types.Tbool false) ]
  in
  let output_file =
    Fmt.str "%s/%s/client/src/main.tsx" (getcwd ()) output_dir
  in
  write_file output_file code

let setup_client_folder output_dir =
  system (Fmt.str "rm %s/%s/client/index.jinja" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/client/src/main.jinja" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/client/src/components/*" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/client/src/pages/*" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/client/src/services/*" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/client/src/types/*" (getcwd ()) output_dir)
  |> ignore;
  system (Fmt.str "rm %s/%s/client/.env.jinja" (getcwd ()) output_dir) |> ignore;
  system (Fmt.str "rm %s/%s/client/package.jinja" (getcwd ()) output_dir)
  |> ignore

let generate_client client_specs output_dir server_port =
  setup_client_folder output_dir;
  let {
    app_title;
    general_components_specs;
    find_components_specs;
    action_form_components_specs;
    action_button_components_specs;
    custom_components_specs;
    pages_specs;
    types_specs;
    services_specs;
    auth_specs;
    client_deps;
  } =
    client_specs
  in
  List.iter general_components_specs ~f:(fun component ->
      generate_general_component output_dir component);
  List.iter find_components_specs ~f:(fun component ->
      generate_find_component output_dir component);
  List.iter action_form_components_specs ~f:(fun component ->
      generate_action_form_component output_dir component);
  List.iter action_button_components_specs ~f:(fun component ->
      generate_action_button_component output_dir component);
  List.iter custom_components_specs ~f:(fun component ->
      generate_custom_component output_dir component);
  List.iter pages_specs ~f:(fun page -> generate_page output_dir page);
  generate_package_file output_dir client_deps;
  generate_main_file output_dir auth_specs server_port;
  generate_env_file output_dir server_port;
  generate_index_file output_dir app_title;
  generate_services output_dir services_specs;
  generate_auth output_dir auth_specs;
  generate_types output_dir types_specs auth_specs
