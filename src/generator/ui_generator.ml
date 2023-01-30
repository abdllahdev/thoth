open Sys
open Core
open Jingoo
open Specs.Ui_specs
open File_generator

let generate_component (component_specs : component_specs) : unit =
  let component_template =
    getcwd () ^ "/templates/client/src/components/template.jsx"
  in
  let component_code =
    Jg_template.from_file component_template
      ~models:
        [
          ("type", Jg_types.Tstr "component");
          ( "component",
            Jg_types.Tobj
              [
                ("id", Jg_types.Tstr component_specs.id);
                ( "args",
                  Jg_types.Tstr
                    (match component_specs.args with
                    | Some args -> args
                    | None -> "") );
                ( "let_exprs",
                  Jg_types.Tlist
                    (match component_specs.let_expressions with
                    | Some let_exprs ->
                        List.map
                          ~f:(fun let_exp ->
                            Jg_types.Tobj
                              [
                                ("id", Jg_types.Tstr let_exp.id);
                                ( "query_id",
                                  Jg_types.Tstr let_exp.query_application.id );
                              ])
                          let_exprs
                    | None -> []) );
                ("jsx", Jg_types.Tstr component_specs.jsx);
              ] );
        ]
  in
  let component_file =
    getcwd () ^ "/.out/client/src/components/" ^ component_specs.id ^ ".jsx"
  in
  write_file component_file component_code

let generate_routes (components : component_specs list) : unit =
  ignore (List.map ~f:generate_component components);
  let ids = List.map ~f:(fun element -> Jg_types.Tstr element.id) components in
  let components_index_file =
    getcwd () ^ "/templates/client/src/components/index.js"
  in
  let components_index_code =
    Jg_template.from_file components_index_file
      ~models:[ ("names", Jg_types.Tlist ids) ]
  in
  let components_index_file =
    getcwd () ^ "/.out/client/src/components/index.js"
  in
  write_file components_index_file components_index_code

let generate_page (page_specs : page_specs) : unit =
  let page_template =
    getcwd () ^ "/templates/client/src/components/template.jsx"
  in
  let page_code =
    Jg_template.from_file page_template
      ~models:
        [
          ("type", Jg_types.Tstr "page");
          ( "component",
            Jg_types.Tobj
              [
                ("id", Jg_types.Tstr page_specs.id);
                ( "args",
                  Jg_types.Tstr
                    (match page_specs.args with
                    | Some args -> args
                    | None -> "") );
                ( "let_exprs",
                  Jg_types.Tlist
                    (match page_specs.let_expressions with
                    | Some let_exprs ->
                        List.map
                          ~f:(fun let_exp ->
                            Jg_types.Tobj
                              [
                                ("id", Jg_types.Tstr let_exp.id);
                                ( "query",
                                  Jg_types.Tobj
                                    [
                                      ( "model",
                                        Jg_types.Tstr
                                          let_exp.query_application.model );
                                    ] );
                              ])
                          let_exprs
                    | None -> []) );
                ("jsx", Jg_types.Tstr page_specs.jsx);
              ] );
        ]
  in
  let directories = String.split ~on:'/' page_specs.route in
  ignore
    (List.map ~f:make_directory
       (List.slice directories 0 (List.length directories - 1)));
  let page_file =
    getcwd () ^ "/.out/client/src/pages/" ^ page_specs.route
    ^ (if String.equal page_specs.route "/" then "index"
      else List.nth_exn directories (List.length directories - 1))
    ^ ".jsx"
  in
  write_file page_file page_code

let setup_client_folder =
  let destination = getcwd () ^ "/templates/client" in
  create_folder destination;
  delete_files "/client/src/components/template.jsx"

let generate_ui (ui_specs : ui_specs) : unit =
  setup_client_folder;
  let { pages; components } = ui_specs in
  ignore (List.map ~f:generate_page pages);
  generate_routes components
