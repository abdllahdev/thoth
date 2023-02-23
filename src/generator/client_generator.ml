open Sys
open Core
open Jingoo
open Specs.Client_specs
open File_generator

let generate_general_component general_component_specs =
  let general_component_template =
    getcwd ()
    ^ "/templates/client/src/components/general_component_template.jinja"
  in

  let { id; args; imported_components; render_expression } =
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
    typ;
    find_from;
    result_variable;
    result_type;
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
          ("find_from", Jg_types.Tstr find_from);
          ( "variable",
            Jg_types.Tobj
              [
                ("id", Jg_types.Tstr result_variable);
                ("type", Jg_types.Tstr result_type);
              ] );
          ("on_error", Jg_types.Tstr on_error);
          ("on_loading", Jg_types.Tstr on_loading);
          ("render_expression", Jg_types.Tstr render_expression);
        ]
  in

  let find_component_file =
    getcwd () ^ "/.out/client/src/components/" ^ typ ^ id ^ ".tsx"
  in

  write_file find_component_file find_component_code

let generate_create_update_component create_update_component_specs =
  let page_template =
    getcwd ()
    ^ "/templates/client/src/components/create_update_component_template.jinja"
  in

  let { id; typ; post_to; form_fields; form_button } =
    create_update_component_specs
  in

  let create_update_component_code =
    Jg_template.from_file page_template
      ~models:
        [
          ("id", Jg_types.Tstr id);
          ("type", Jg_types.Tstr typ);
          ("post_to", Jg_types.Tstr post_to);
          ( "form_fields",
            Jg_types.Tlist
              (List.map form_fields ~f:(fun form_field ->
                   Jg_types.Tlist
                     (List.map form_field ~f:(fun (attr_name, attr_value) ->
                          Jg_types.Tobj
                            [
                              ("name", Jg_types.Tstr attr_name);
                              ("value", Jg_types.Tstr attr_value);
                            ])))) );
          ( "form_button",
            Jg_types.Tlist
              (List.map form_button ~f:(fun (attr_name, attr_value) ->
                   Jg_types.Tobj
                     [
                       ("name", Jg_types.Tstr attr_name);
                       ("value", Jg_types.Tstr attr_value);
                     ])) );
        ]
  in

  let create_update_component_file =
    getcwd () ^ "/.out/client/src/components/" ^ typ ^ id ^ ".tsx"
  in

  write_file create_update_component_file create_update_component_code

let generate_delete_component delete_component_specs =
  let delete_component_template =
    getcwd ()
    ^ "/templates/client/src/components/delete_component_template.jinja"
  in

  let { id; typ; post_to; form_button } = delete_component_specs in

  let delete_component_code =
    Jg_template.from_file delete_component_template
      ~models:
        [
          ("id", Jg_types.Tstr id);
          ("post_to", Jg_types.Tstr post_to);
          ( "form_button",
            Jg_types.Tlist
              (List.map form_button ~f:(fun (attr_name, attr_value) ->
                   Jg_types.Tobj
                     [
                       ("name", Jg_types.Tstr attr_name);
                       ("value", Jg_types.Tstr attr_value);
                     ])) );
        ]
  in

  let delete_component_file =
    getcwd () ^ "/.out/client/src/components/" ^ typ ^ id ^ ".tsx"
  in

  write_file delete_component_file delete_component_code

let generate_page page_specs =
  let page_template =
    getcwd () ^ "/templates/client/src/pages/template.jinja"
  in

  let { id; route; imported_components; render_expression; _ } = page_specs in

  let page_code =
    Jg_template.from_file page_template
      ~models:
        [
          ( "imported_components",
            Jg_types.Tlist
              (List.map imported_components ~f:(fun imported_component ->
                   Jg_types.Tstr imported_component)) );
          ("id", Jg_types.Tstr id);
          ("render_expression", Jg_types.Tstr render_expression);
        ]
  in

  let page_file =
    let splitted_route = String.split ~on:'/' route in
    let parameter_regex = Str.regexp "\\[[A-Za-z]+\\]" in
    let tail = List.nth_exn splitted_route (List.length splitted_route - 1) in
    print_string (tail ^ "\n");
    let is_parameter = Str.string_match parameter_regex tail 0 in
    let path =
      String.concat ~sep:"/"
        ([ "/client/src/pages" ]
        @ List.slice splitted_route 1 (List.length splitted_route - 1))
    in
    make_directory path;
    if is_parameter then getcwd () ^ "/.out/client/src/pages/" ^ route ^ ".tsx"
    else getcwd () ^ "/.out/client/src/pages/" ^ route ^ "/index.tsx"
  in

  write_file page_file page_code

let setup_client_folder =
  let destination = getcwd () ^ "/templates/client" in
  create_folder destination;
  delete_files "/client/src/components/general_component_template.jinja";
  delete_files "/client/src/components/find_component_template.jinja";
  delete_files "/client/src/components/create_update_component_template.jinja";
  delete_files "/client/src/components/delete_component_template.jinja";
  delete_files "/client/src/pages/template.jinja"

let generate_client client_specs =
  setup_client_folder;
  let {
    general_components_specs;
    find_components_specs;
    create_update_components_specs;
    delete_components_specs;
    pages_specs;
    _;
  } =
    client_specs
  in

  List.map ~f:generate_general_component general_components_specs |> ignore;
  List.map ~f:generate_find_component find_components_specs |> ignore;
  List.map ~f:generate_create_update_component create_update_components_specs
  |> ignore;
  List.map ~f:generate_delete_component delete_components_specs |> ignore;
  List.map ~f:generate_page pages_specs |> ignore
