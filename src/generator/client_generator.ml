open Core
open Core_unix
open Jingoo
open Specs.Client_specs
open File_generator

let generate_general_component general_component_specs =
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
    getcwd () ^ "/.out/client/src/components/" ^ id ^ ".tsx"
  in
  write_file general_component_file general_component_code

let generate_find_component find_component_specs =
  let find_component_template =
    getcwd () ^ "/templates/client/src/components/find_component_template.jinja"
  in
  let {
    id;
    find_from;
    result_variable;
    result_type;
    result_scalar_type;
    imported_components;
    requires_auth;
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
          ("requires_auth", Jg_types.Tbool requires_auth);
          ("find_from", Jg_types.Tstr find_from);
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
    getcwd () ^ "/.out/client/src/components/" ^ id ^ ".tsx"
  in
  write_file find_component_file find_component_code

let generate_action_form_component ?on_success_redirect_to ?on_fail_redirect_to
    action_form_component_specs =
  let generate_action_form_template =
    getcwd ()
    ^ "/templates/client/src/components/action_form_component_template.jinja"
  in
  let {
    id;
    args;
    typ;
    post_to;
    global_style;
    form_validation_scheme;
    form_inputs;
    form_button;
    requires_auth;
  } =
    action_form_component_specs
  in
  let action_form_component_code =
    Jg_template.from_file generate_action_form_template
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
          ("type", Jg_types.Tstr typ);
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
          ("post_to", Jg_types.Tstr post_to);
          ("requires_auth", Jg_types.Tbool requires_auth);
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
    getcwd () ^ "/.out/client/src/components/" ^ id ^ ".tsx"
  in
  write_file action_form_component_file action_form_component_code

let generate_action_button_component ?on_success_redirect_to
    ?on_fail_redirect_to action_button_component_specs =
  let action_button_component_template =
    getcwd ()
    ^ "/templates/client/src/components/action_button_component_template.jinja"
  in
  let { id; args; post_to; typ; form_button; requires_auth } =
    action_button_component_specs
  in
  let action_button_component_code =
    Jg_template.from_file action_button_component_template
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
          ("post_to", Jg_types.Tstr post_to);
          ("requires_auth", Jg_types.Tbool requires_auth);
          ("type", Jg_types.Tstr typ);
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
    getcwd () ^ "/.out/client/src/components/" ^ id ^ ".tsx"
  in
  write_file action_button_component_file action_button_component_code

let generate_page page_specs =
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
    make_directory path;
    if is_parameter then getcwd () ^ "/.out/client/src/pages" ^ route ^ ".tsx"
    else getcwd () ^ "/.out/client/src/pages" ^ route ^ "/index.tsx"
  in
  write_file page_file page_code

let generate_type type_id type_data =
  let type_template =
    getcwd () ^ "/templates/client/src/types/template.jinja"
  in
  let type_code =
    Jg_template.from_file type_template
      ~models:
        [
          ("id", Jg_types.Tstr type_id);
          ( "elements",
            Jg_types.Tlist
              (List.map type_data ~f:(fun element ->
                   let id, typ = element in
                   Jg_types.Tobj
                     [ ("id", Jg_types.Tstr id); ("type", Jg_types.Tstr typ) ]))
          );
        ]
  in
  let type_file =
    getcwd () ^ "/.out/client/src/types/" ^ String.uncapitalize type_id ^ ".ts"
  in
  write_file type_file type_code

let generate_types types_specs auth_specs =
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
      let data = Hashtbl.to_alist data in
      generate_type key data);
  let types_index_template =
    getcwd () ^ "/templates/client/src/types/index.jinja"
  in
  let types_index_code =
    Jg_template.from_file types_index_template
      ~models:[ ("names", Jg_types.Tlist names) ]
  in
  let types_index_file = getcwd () ^ "/.out/client/src/types/index.ts" in
  write_file types_index_file types_index_code

let generate_auth auth_specs =
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
        let user_type_file = getcwd () ^ "/.out/client/src/types/user.ts" in
        write_file user_type_file types_user_type_code
      in
      generate_action_form_component signup_form ~on_success_redirect_to
        ~on_fail_redirect_to;
      generate_action_form_component login_form ~on_success_redirect_to
        ~on_fail_redirect_to;
      generate_action_button_component logout_button ~on_success_redirect_to
        ~on_fail_redirect_to;
      generate_user_type ()
  | None -> ()

let generate_app_title app_title =
  let app_index_template = getcwd () ^ "/templates/client/index.jinja" in
  let app_index_code =
    Jg_template.from_file app_index_template
      ~models:[ ("app_title", Jg_types.Tstr app_title) ]
  in
  let app_index_file = getcwd () ^ "/.out/client/index.html" in
  write_file app_index_file app_index_code

let setup_client_folder =
  let destination = getcwd () ^ "/templates/client" in
  create_folder destination;
  system (Fmt.str "rm %s/.out/client/index.jinja" (getcwd ())) |> ignore;
  system (Fmt.str "rm %s/.out/client/src/components/*" (getcwd ())) |> ignore;
  system (Fmt.str "rm %s/.out/client/src/pages/*" (getcwd ())) |> ignore;
  system (Fmt.str "rm %s/.out/client/src/types/*" (getcwd ())) |> ignore

let generate_client client_specs =
  setup_client_folder;
  let {
    app_title;
    general_components_specs;
    find_components_specs;
    action_form_components_specs;
    action_button_components_specs;
    pages_specs;
    types_specs;
    auth_specs;
  } =
    client_specs
  in
  List.iter ~f:generate_general_component general_components_specs;
  List.iter ~f:generate_find_component find_components_specs;
  List.iter ~f:generate_action_form_component action_form_components_specs;
  List.iter ~f:generate_action_button_component action_button_components_specs;
  List.iter ~f:generate_page pages_specs;
  generate_app_title app_title;
  generate_auth auth_specs;
  generate_types types_specs auth_specs
