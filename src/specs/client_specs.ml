open Core
open Ast.Ast_types
open Ast.Helper
open Ast.Formatter
open Type_checker.Environment

type general_component_specs = {
  id : string;
  args : (string * string) list;
  imported_components : string list;
  render_expression : string;
}

type find_component_specs = {
  id : string;
  find_from : string;
  result_variable : string;
  result_type : string;
  imported_components : string list;
  on_error : string;
  on_loading : string;
  render_expression : string;
}

type create_update_component_specs = {
  id : string;
  typ : string;
  post_to : string;
  form_fields : (string * string) list list;
  form_button : (string * string) list;
}

type delete_component_specs = {
  id : string;
  post_to : string;
  form_button : (string * string) list;
}

type type_specs = (string, string) Hashtbl.t

type page_specs = {
  id : string;
  route : string;
  permissions : permission list option;
  imported_components : string list;
  render_expression : string;
}

type client_specs = {
  general_components_specs : general_component_specs list;
  find_components_specs : find_component_specs list;
  create_update_components_specs : create_update_component_specs list;
  delete_components_specs : delete_component_specs list;
  pages_specs : page_specs list;
  types_specs : (string, type_specs) Hashtbl.t;
}

let convert_type typ =
  let convert_scalar_type scalar_type =
    match scalar_type with
    | Int -> ": number"
    | String -> ": string"
    | Boolean -> ": boolean"
    | DateTime -> ": string"
    | _ -> failwith "CompilationError: Something went wrong"
  in

  let convert_composite_type composite_type =
    match composite_type with
    | List scalar_type -> Fmt.str " %s[]" (convert_scalar_type scalar_type)
    | Optional scalar_type -> Fmt.str "? %s" (convert_scalar_type scalar_type)
    | OptionalList scalar_type ->
        Fmt.str "? %s[]" (convert_scalar_type scalar_type)
  in

  match typ with
  | Composite composite_type -> convert_composite_type composite_type
  | Scalar scalar_type -> convert_scalar_type scalar_type

let rec get_imported_components global_env xra_expression =
  match xra_expression with
  | XRA.Element (_, id, attributes, children) ->
      let attributes =
        match attributes with
        | Some attributes ->
            List.fold attributes ~init:[] ~f:(fun lst attribute ->
                lst @ get_imported_components global_env attribute)
        | None -> []
      in

      let children =
        match children with
        | Some children ->
            List.fold children ~init:[] ~f:(fun lst child ->
                lst @ get_imported_components global_env child)
        | None -> []
      in

      if GlobalEnvironment.contains global_env ~key:id then
        [ id ] @ attributes @ children
      else attributes @ children
  | Fragment (_, expressions) -> (
      match expressions with
      | Some expressions ->
          List.fold expressions ~init:[] ~f:(fun lst expression ->
              lst @ get_imported_components global_env expression)
      | None -> [])
  | Attribute (_, _, expression)
  | ForExpression (_, _, _, expression)
  | IfThenExpression (_, _, expression) ->
      get_imported_components global_env expression
  | IfThenElseExpression (_, _, then_block, else_block) ->
      get_imported_components global_env then_block
      @ get_imported_components global_env else_block
  | _ -> []

let rec generate_xra_specs xra_expression =
  match xra_expression with
  | XRA.Element (_, id, attributes, children) ->
      let attributes =
        match attributes with
        | Some attributes ->
            List.fold attributes ~init:"" ~f:(fun str attribute ->
                str ^ generate_xra_specs attribute)
        | None -> ""
      in

      let children =
        match children with
        | Some children ->
            List.fold children ~init:"" ~f:(fun str child ->
                match child with
                | XRA.Element _ | XRA.Fragment _ ->
                    str ^ generate_xra_specs child
                | _ -> str ^ Fmt.str "{ %s }" (generate_xra_specs child))
        | None -> ""
      in

      if String.is_empty children then Fmt.str "<%s %s/>" id attributes
      else Fmt.str "<%s %s>%s</%s>" id attributes children id
  | XRA.Fragment (_, children) -> (
      match children with
      | Some children ->
          List.fold children ~init:"" ~f:(fun str child ->
              match child with
              | XRA.Element _ | XRA.Fragment _ -> str ^ generate_xra_specs child
              | _ -> str ^ Fmt.str "{ %s }" (generate_xra_specs child))
      | None -> "")
  | XRA.Attribute (_, id, expression) ->
      Fmt.str "%s={ %s }" id (generate_xra_specs expression)
  | XRA.Literal literal -> (
      match literal with
      | StringLiteral (_, str) -> Fmt.str "'%s'" str
      | _ -> Fmt.str "'%s'" (string_of_literal literal))
  | XRA.VariableExpression (_, id) -> Fmt.str "%s" id
  | XRA.DotExpression (_, id, expanded_id) -> Fmt.str "%s.%s" id expanded_id
  | LiteralConditionalExpression (_, expression) ->
      generate_xra_specs expression
  | NotConditionalExpression (_, expression) ->
      Fmt.str "!(%s)" (generate_xra_specs expression)
  | EqConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "%s === %s"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
  | NotEqConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "%s !== %s"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
  | LtConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "%s < %s"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
  | GtConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "%s > %s"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
  | LtOrEqConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "%s <= %s"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
  | GtOrEqConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "%s >= %s"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
  | IfThenElseExpression (_, condition, then_block, else_block) ->
      Fmt.str "(%s) ? (%s) : (%s)"
        (generate_xra_specs condition)
        (generate_xra_specs then_block)
        (generate_xra_specs else_block)
  | IfThenExpression (_, condition, then_block) ->
      Fmt.str "(%s) && (%s)"
        (generate_xra_specs condition)
        (generate_xra_specs then_block)
  | ForExpression (_, var, lst, expression) ->
      Fmt.str "%s && %s.map((%s : any) => (%s))" (generate_xra_specs lst)
        (generate_xra_specs lst) var
        (generate_xra_specs expression)
  | _ -> ""

let generate_page_specs global_env page_declaration =
  let _, id, route, permissions, body = page_declaration in
  let let_expressions, render_expression = body in
  let imported_components =
    (match let_expressions with
    | Some let_expressions ->
        List.fold let_expressions ~init:[] ~f:(fun lst let_expression ->
            lst @ get_imported_components global_env let_expression)
    | None -> [])
    @ List.fold render_expression ~init:[] ~f:(fun lst expression ->
          lst @ get_imported_components global_env expression)
  in
  let render_expression =
    List.fold render_expression ~init:"" ~f:(fun str expression ->
        str ^ generate_xra_specs expression)
  in
  { id; route; permissions; imported_components; render_expression }

let generate_general_component_specs global_env id args body =
  let args =
    match args with
    | Some args ->
        List.map args ~f:(fun arg ->
            let _, arg_id, arg_type = arg in
            (arg_id, string_of_type arg_type))
    | None -> []
  in
  let imported_components, render_expression =
    match body with
    | Component.GeneralBody (_, render_expression) ->
        let imported_components =
          List.fold render_expression ~init:[] ~f:(fun lst expression ->
              lst @ get_imported_components global_env expression)
        in
        let render_expression =
          List.fold render_expression ~init:"" ~f:(fun str expression ->
              str ^ generate_xra_specs expression)
        in
        (imported_components, render_expression)
    | _ -> ([ "" ], "")
  in
  { id; args; imported_components; render_expression }

let generate_find_component_specs global_env id query_id variable_id body
    types_specs =
  let query =
    GlobalEnvironment.lookup global_env ~key:query_id
    |> GlobalEnvironment.get_query_value
  in

  let result_scalar_type =
    get_scalar_type query.return_type |> string_of_scalar_type
  in

  (* Model of the result returned by the query
     used to build a typescript type in the React client app *)
  if not (Hashtbl.mem types_specs result_scalar_type) then (
    let model_value =
      GlobalEnvironment.lookup global_env ~key:result_scalar_type
      |> GlobalEnvironment.get_model_value
    in

    let type_specs = Hashtbl.create ~size:17 (module String) in

    Hashtbl.iteri model_value ~f:(fun ~key ~data ->
        if not (is_custom_type data.typ) then
          let field_type = convert_type data.typ in
          Hashtbl.add_exn type_specs ~key ~data:field_type);

    Hashtbl.add_exn types_specs ~key:result_scalar_type ~data:type_specs);

  let imported_components, on_error, on_loading, render_expression =
    match body with
    | Component.FindBody (on_error, on_loading, render_expression) ->
        let imported_components =
          List.fold on_error ~init:[] ~f:(fun lst expression ->
              lst @ get_imported_components global_env expression)
          @ List.fold on_loading ~init:[] ~f:(fun lst expression ->
                lst @ get_imported_components global_env expression)
          @ List.fold render_expression ~init:[] ~f:(fun lst expression ->
                lst @ get_imported_components global_env expression)
        in
        let on_error =
          List.fold on_error ~init:"" ~f:(fun str expression ->
              str ^ generate_xra_specs expression)
        in
        let on_loading =
          List.fold on_loading ~init:"" ~f:(fun str expression ->
              str ^ generate_xra_specs expression)
        in
        let render_expression =
          List.fold render_expression ~init:"" ~f:(fun str expression ->
              str ^ generate_xra_specs expression)
        in
        (imported_components, on_error, on_loading, render_expression)
    | _ -> ([ "" ], "", "", "")
  in

  {
    id;
    find_from = result_scalar_type |> String.lowercase;
    result_variable = variable_id;
    result_type = string_of_type query.return_type;
    imported_components;
    on_error;
    on_loading;
    render_expression;
  }

let get_field_attr field_attr =
  match field_attr with
  | Component.FormFieldName name -> ("name", name)
  | Component.FormFieldVisibility literal ->
      ("visibility", string_of_literal literal)
  | Component.FormFieldDefaultValue value -> ("defaultValue", value)
  | Component.FormFieldStyle style -> ("style", style)
  | Component.FormFieldType field_type ->
      ("type", ComponentFormatter.string_form_field_type field_type)

let generate_create_update_components_specs global_env id query_id body =
  let query =
    GlobalEnvironment.lookup global_env ~key:query_id
    |> GlobalEnvironment.get_query_value
  in

  let result_scalar_type =
    get_scalar_type query.return_type |> string_of_scalar_type
  in

  let get_form_field field =
    let _, id, field_attrs = field in
    List.fold field_attrs
      ~init:[ ("name", id) ]
      ~f:(fun lst attr ->
        let key, data = get_field_attr attr in
        lst @ [ (key, data) ])
  in

  let form_fields, form_button =
    (match body with
    | Component.CreateBody (form_fields, form_button)
    | Component.UpdateBody (form_fields, form_button) ->
        let form_fields =
          List.map form_fields ~f:(fun field -> get_form_field field)
        in

        let form_button =
          List.fold form_button ~init:[] ~f:(fun lst attr ->
              let key, data = get_field_attr attr in
              lst @ [ (key, data) ])
        in

        Some (form_fields, form_button)
    | _ -> None)
    |> Option.value_exn
  in

  {
    id;
    typ = QueryFormatter.string_of_query_type query.typ;
    post_to = result_scalar_type |> String.lowercase;
    form_fields;
    form_button;
  }

let generate_delete_components_specs global_env id query_id body =
  let query =
    GlobalEnvironment.lookup global_env ~key:query_id
    |> GlobalEnvironment.get_query_value
  in

  let result_scalar_type =
    get_scalar_type query.return_type |> string_of_scalar_type
  in

  let form_button =
    (match body with
    | Component.DeleteBody form_button ->
        let form_button =
          List.fold form_button ~init:[] ~f:(fun lst attr ->
              let key, data = get_field_attr attr in
              lst @ [ (key, data) ])
        in

        Some form_button
    | _ -> None)
    |> Option.value_exn
  in

  { id; post_to = result_scalar_type |> String.lowercase; form_button }

let generate_client_specs global_env component_declarations page_declarations =
  let types_specs = Hashtbl.create ~size:17 (module String) in

  let general_components_specs =
    List.filter_map component_declarations ~f:(fun component_declaration ->
        let _, id, typ, args, body = component_declaration in
        match typ with
        | Component.General ->
            Some (generate_general_component_specs global_env id args body)
        | _ -> None)
  in

  let find_components_specs =
    List.filter_map component_declarations ~f:(fun component_declaration ->
        let _, id, typ, _, body = component_declaration in

        match typ with
        | Component.FindMany (_, query_id, variable_id)
        | Component.FindUnique (_, query_id, variable_id) ->
            Some
              (generate_find_component_specs global_env id query_id variable_id
                 body types_specs)
        | _ -> None)
  in

  let create_update_components_specs =
    List.filter_map component_declarations ~f:(fun component_declaration ->
        let _, id, typ, _, body = component_declaration in

        match typ with
        | Component.Create (_, query_id) | Component.Update (_, query_id) ->
            Some
              (generate_create_update_components_specs global_env id query_id
                 body)
        | _ -> None)
  in

  let delete_components_specs =
    List.filter_map component_declarations ~f:(fun component_declaration ->
        let _, id, typ, _, body = component_declaration in

        match typ with
        | Component.Delete (_, query_id) ->
            Some (generate_delete_components_specs global_env id query_id body)
        | _ -> None)
  in

  let pages_specs =
    List.map page_declarations ~f:(fun declaration_page ->
        generate_page_specs global_env declaration_page)
  in

  {
    general_components_specs;
    find_components_specs;
    create_update_components_specs;
    delete_components_specs;
    pages_specs;
    types_specs;
  }
