open Core
open Error_handler.Handler
open Ast.Ast_types
open Ast.Helper
open Ast.Formatter
open Type_checker.Environment
open Server_specs

type general_component_specs = {
  id : string;
  args : (string * string) list;
  imported_types : string list;
  imported_components : string list;
  render_expression : string;
}

type find_component_specs = {
  id : string;
  find_func : string;
  func_type : string;
  func_model : string;
  where_arg : string option;
  search_arg : (string * string) list option;
  owns_entry : bool;
  requires_auth : bool;
  args : (string * string) list;
  model : string option;
  result_variable : string;
  result_type : string;
  result_scalar_type : string;
  imported_components : string list;
  on_error : string;
  on_loading : string;
  render_expression : string;
}

type custom_component_specs = {
  id : string;
  args : (string * string) list;
  imports : string option;
  fn : string;
}

type form_input = {
  wrapper_style : (string * string) option;
  label_attrs : (string * string) list option;
  input_attrs : (string * string) list;
}

type action_form_component_specs = {
  id : string;
  args : (string * string) list;
  action_func : string;
  func_type : string;
  where_arg : string option;
  requires_auth : bool;
  form_validation_scheme : (string * string) option list;
  form_inputs : form_input list;
  form_button : (string * string) list;
  global_style : (string * string) list option;
}

type action_button_component_specs = {
  id : string;
  args : (string * string) list;
  action_func : string;
  where_arg : string option;
  requires_auth : bool;
  func_type : string;
  form_button : (string * string) list;
}

type type_specs = {
  imported_types : string list;
  types : (string, string) Hashtbl.t;
}

type page_specs = {
  id : string;
  route : string;
  requires_auth : string option;
  imported_components : string list;
  render_expression : string;
}

type auth_specs = {
  signup_form : action_form_component_specs;
  login_form : action_form_component_specs;
  logout_button : action_button_component_specs;
  user_model : string;
  id_field : string;
  username_field : string;
  on_success_redirect_to : string;
  on_fail_redirect_to : string;
}

type service_func_specs = {
  func_id : string;
  func_type : string;
  requires_where : bool;
  requires_search : bool;
  http_method : string;
  route : string;
  requires_auth : bool;
}

type services_specs = (string * service_func_specs list) list

type client_specs = {
  app_title : string;
  general_components_specs : general_component_specs list;
  find_components_specs : find_component_specs list;
  action_form_components_specs : action_form_component_specs list;
  action_button_components_specs : action_button_component_specs list;
  custom_components_specs : custom_component_specs list;
  pages_specs : page_specs list;
  types_specs : (string, type_specs) Hashtbl.t;
  auth_specs : auth_specs option;
  services_specs : services_specs;
  client_deps : (string * string) list option;
}

let convert_type typ =
  let convert_scalar_type scalar_type =
    match scalar_type with
    | Int -> ": number"
    | String -> ": string"
    | Boolean -> ": boolean"
    | DateTime -> ": string"
    | CustomType custom_type -> Fmt.str ": %s" custom_type
    | _ -> raise_compiler_error ()
  in
  let convert_composite_type composite_type =
    match composite_type with
    | List scalar_type -> Fmt.str " %s[]" (convert_scalar_type scalar_type)
    | Optional scalar_type -> Fmt.str "? %s" (convert_scalar_type scalar_type)
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

let get_component_args args =
  match args with
  | Some args ->
      List.map args ~f:(fun arg ->
          let _, arg_id, arg_type = arg in
          (arg_id, convert_type arg_type))
  | None -> []

let get_imported_types args =
  match args with
  | Some args ->
      List.fold args ~init:[] ~f:(fun lst arg ->
          let _, _, arg_type = arg in
          if is_custom_type arg_type then
            lst @ [ get_scalar_type arg_type |> string_of_scalar_type ]
          else lst)
  | None -> []

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
      | _ -> Fmt.str "%s" (string_of_literal literal))
  | XRA.VariableExpression (_, id) -> Fmt.str "%s" id
  | XRA.DotExpression (_, hd, tl) ->
      Fmt.str "%s.%s" (generate_xra_specs hd) (generate_xra_specs tl)
  | LiteralConditionalExpression (_, expression) ->
      generate_xra_specs expression
  | NotConditionalExpression (_, expression) ->
      Fmt.str "!(%s)" (generate_xra_specs expression)
  | AndConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "((%s) && %s)"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
  | OrConditionalExpression (_, left_expression, right_expression) ->
      Fmt.str "((%s) || %s)"
        (generate_xra_specs left_expression)
        (generate_xra_specs right_expression)
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
      Fmt.str "%s && %s.map((%s, idx) => (<div key={idx}>%s</div>))"
        (generate_xra_specs lst) (generate_xra_specs lst) var
        (generate_xra_specs expression)
  | _ -> ""

let generate_page_specs global_env page_declaration auth_specs =
  let _, id, route, permissions, body = page_declaration in
  let _, route = route in
  let requires_auth =
    match permissions with
    | Some _ ->
        Some
          (match auth_specs with
          | Some auth_specs ->
              let { on_fail_redirect_to; _ } = auth_specs in
              Some on_fail_redirect_to
          | None -> None)
        |> Option.value_exn
    | None -> None
  in
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
  { id; route; requires_auth; imported_components; render_expression }

let generate_general_component_specs global_env id args body =
  let imported_types = get_imported_types args in
  let args = get_component_args args in
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
  { id; args; imported_types; imported_components; render_expression }

let generate_query_application_specs query_app =
  let _, query_id, where_arg, search_arg = query_app in
  let where_arg =
    match where_arg with
    | Some (_, where_arg) -> Some (string_of_literal where_arg)
    | None -> None
  in
  let search_arg =
    match search_arg with
    | Some search_arg ->
        Some
          (List.map search_arg ~f:(fun (_, id, value) ->
               (id, string_of_literal value)))
    | None -> None
  in
  (query_id, where_arg, search_arg)

let generate_find_component_specs global_env id args body types_specs =
  let args = get_component_args args in
  let ( imported_components,
        query_id,
        where_arg,
        search_arg,
        variable_id,
        on_error,
        on_loading,
        render_expression ) =
    match body with
    | Component.FindBody
        ((query_app, (_, variable_id)), on_error, on_loading, render_expression)
      ->
        let query_id, where_arg, search_arg =
          generate_query_application_specs query_app
        in
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
        ( imported_components,
          query_id,
          where_arg,
          search_arg,
          variable_id,
          on_error,
          on_loading,
          render_expression )
    | _ -> ([ "" ], "", Some "", Some [ ("", "") ], "", "", "", "")
  in
  let query =
    GlobalEnvironment.lookup global_env ~key:query_id
    |> GlobalEnvironment.get_query_value
  in
  let result_scalar_type =
    get_scalar_type query.return_type |> string_of_scalar_type
  in
  let requires_auth, owns_entry =
    let permissions = query.permissions in
    match permissions with
    | Some permissions ->
        ( true,
          List.exists permissions ~f:(fun (_, permission) ->
              if String.equal permission "OwnsRecord" then true else false) )
    | None -> (false, false)
  in
  let model =
    match
      (GlobalEnvironment.lookup global_env ~key:query_id
      |> GlobalEnvironment.get_query_value)
        .model
    with
    | Some (_, model) -> Some model
    | None -> None
  in
  (if not (Hashtbl.mem types_specs result_scalar_type) then
   let model_value =
     GlobalEnvironment.lookup global_env ~key:result_scalar_type
     |> GlobalEnvironment.get_model_value
   in
   let type_specs = Hashtbl.create ~size:17 (module String) in
   let imported_types =
     Hashtbl.fold model_value ~init:[] ~f:(fun ~key ~data lst ->
         let field_type = convert_type data.typ in
         let field_attrs_table = data.field_attrs_table |> Option.value_exn in
         match
           LocalEnvironment.contains field_attrs_table ~key:"@relation"
           || not (is_custom_type data.typ)
         with
         | true ->
             Hashtbl.add_exn type_specs ~key ~data:field_type;
             if is_custom_type data.typ then
               lst @ [ string_of_scalar_type (get_scalar_type data.typ) ]
             else lst
         | false -> (
             match
               String.equal
                 (QueryFormatter.string_of_query_type query.typ)
                 "findMany"
             with
             | true -> lst
             | false ->
                 lst @ [ string_of_scalar_type (get_scalar_type data.typ) ]))
   in
   Hashtbl.add_exn types_specs ~key:result_scalar_type
     ~data:{ types = type_specs; imported_types });
  {
    id;
    find_func = query_id;
    func_type = QueryFormatter.string_of_query_type query.typ;
    func_model = String.uncapitalize (Option.value_exn model);
    args;
    owns_entry;
    where_arg;
    search_arg;
    model;
    requires_auth;
    result_variable = variable_id;
    result_type = string_of_type query.return_type;
    result_scalar_type =
      get_scalar_type query.return_type |> string_of_scalar_type;
    imported_components;
    on_error;
    on_loading;
    render_expression;
  }

let get_from_attr field_attr =
  match field_attr with
  | Component.FormAttrName name -> ("name", name)
  | Component.FormAttrVisibility boolean -> ("isVisible", string_of_bool boolean)
  | Component.FormAttrDefaultValue value ->
      ("defaultValue", string_of_obj_field value)
  | Component.FormAttrStyle style -> ("style", style)
  | Component.FormAttrType input_type ->
      ("type", ComponentFormatter.string_of_form_input_type input_type)
  | Component.FormAttrPlaceholder placeholder -> ("placeholder", placeholder)

let get_form_input input =
  let _, id, wrapper_style, label_attrs, input_attrs = input in
  let wrapper_style =
    match wrapper_style with
    | Some wrapper_style -> Some (get_from_attr wrapper_style)
    | None -> None
  in
  let label_attrs =
    match label_attrs with
    | Some label_attrs ->
        Some
          (List.fold label_attrs ~init:[] ~f:(fun lst attr ->
               let key, data = get_from_attr attr in
               lst @ [ (key, data) ]))
    | None -> None
  in
  let input_attrs =
    List.fold input_attrs
      ~init:[ ("name", id) ]
      ~f:(fun lst attr ->
        let key, data = get_from_attr attr in
        lst @ [ (key, data) ])
  in
  { wrapper_style; input_attrs; label_attrs }

let get_button_specs form_button =
  List.fold form_button ~init:[] ~f:(fun lst attr ->
      let key, data = get_from_attr attr in
      lst @ [ (key, data) ])

let get_form_specs form_inputs form_button =
  let form_inputs =
    List.map form_inputs ~f:(fun input -> get_form_input input)
  in
  (form_inputs, get_button_specs form_button)

let get_form_validation_scheme form_inputs =
  let is_visible input_attrs =
    List.fold input_attrs ~init:false ~f:(fun flag attr ->
        match attr with
        | Component.FormAttrVisibility boolean -> boolean
        | _ -> flag)
  in
  let get_input_validation input_attrs =
    let input_type =
      List.find_map input_attrs ~f:(fun attr ->
          match attr with
          | Component.FormAttrType form_input -> Some form_input
          | _ -> None)
      |> Option.value_exn
    in
    match input_type with
    | TextInput -> "z.string()"
    | PasswordInput -> "z.string()"
    | EmailInput -> "z.string().email()"
    | DateInput | DateTimeInput ->
        "z.preprocess((arg) => { if (typeof arg == 'string' || arg instanceof \
         Date) return new Date(arg);}, z.date()),"
    | NumberInput ->
        "z.string().regex(/^\\d+$/).transform((id: string) => parseInt(id, \
         10)),"
    | CheckboxInput -> "z.boolean()"
    | _ -> ""
  in
  List.map form_inputs ~f:(fun input ->
      let _, id, _, _, input_attrs = input in
      if is_visible input_attrs then
        let input_validation = get_input_validation input_attrs in
        Some (id, input_validation)
      else None)

let generate_action_form_components_specs ?global_env id args body =
  let get_global_style global_style =
    match global_style with
    | Some global_style ->
        Some
          (List.map global_style ~f:(fun style ->
               let _, id, style = style in
               let _, value = get_from_attr style in
               (id, value)))
    | None -> None
  in
  let args = get_component_args args in
  let ( action_func,
        where_arg,
        requires_auth,
        typ,
        global_style,
        form_validation_scheme,
        form_inputs,
        form_button ) =
    (match body with
    | Component.CreateBody (query_app, global_style, form_inputs, form_button)
    | Component.UpdateBody (query_app, global_style, form_inputs, form_button)
      ->
        let form_validation_scheme = get_form_validation_scheme form_inputs in
        let global_style = get_global_style global_style in
        let global_env = Option.value_exn global_env in
        let query_id, where_arg, _ =
          generate_query_application_specs query_app
        in
        let query =
          GlobalEnvironment.lookup global_env ~key:query_id
          |> GlobalEnvironment.get_query_value
        in
        let requires_auth =
          let permissions = query.permissions in
          match permissions with Some _ -> true | None -> false
        in
        let form_inputs, form_button = get_form_specs form_inputs form_button in
        Some
          ( query_id,
            where_arg,
            requires_auth,
            QueryFormatter.string_of_query_type query.typ,
            global_style,
            form_validation_scheme,
            form_inputs,
            form_button )
    | Component.SignupFormBody (global_style, form_inputs, form_button) ->
        let form_validation_scheme = get_form_validation_scheme form_inputs in
        let global_style = get_global_style global_style in
        let form_inputs, form_button = get_form_specs form_inputs form_button in
        Some
          ( "signup",
            None,
            false,
            "signup",
            global_style,
            form_validation_scheme,
            form_inputs,
            form_button )
    | Component.LoginFormBody (global_style, form_inputs, form_button) ->
        let form_validation_scheme = get_form_validation_scheme form_inputs in
        let global_style = get_global_style global_style in
        let form_inputs, form_button = get_form_specs form_inputs form_button in
        Some
          ( "login",
            None,
            false,
            "login",
            global_style,
            form_validation_scheme,
            form_inputs,
            form_button )
    | _ -> None)
    |> Option.value_exn
  in
  {
    id;
    args;
    where_arg;
    requires_auth;
    func_type = typ;
    action_func;
    form_validation_scheme;
    global_style;
    form_inputs;
    form_button;
  }

let generate_action_button_components_specs ?global_env id args body =
  let args = get_component_args args in
  let action_func, where_arg, requires_auth, typ, form_button =
    (match body with
    | Component.DeleteBody (query_app, form_button) ->
        let global_env = Option.value_exn global_env in
        let query_id, where_arg, _ =
          generate_query_application_specs query_app
        in
        let query =
          GlobalEnvironment.lookup global_env ~key:query_id
          |> GlobalEnvironment.get_query_value
        in
        let requires_auth =
          let permissions = query.permissions in
          match permissions with Some _ -> true | None -> false
        in
        Some
          ( query_id,
            where_arg,
            requires_auth,
            "delete",
            get_button_specs form_button )
    | Component.LogoutButtonBody form_button ->
        Some ("logout", None, false, "logout", get_button_specs form_button)
    | _ -> None)
    |> Option.value_exn
  in
  {
    id;
    args;
    requires_auth;
    action_func;
    where_arg;
    func_type = typ;
    form_button;
  }

let generate_service_specs server_specs =
  let { routes_specs; _ } = server_specs in
  Hashtbl.fold routes_specs ~init:[] ~f:(fun ~key ~data lst ->
      lst
      @ [
          ( String.uncapitalize key,
            List.map data ~f:(fun route ->
                let {
                  route_id;
                  http_method;
                  custom_route;
                  default_route;
                  middlewares;
                  route_type;
                  requires_where;
                  requires_search;
                  _;
                } =
                  route
                in
                let requires_auth =
                  List.exists middlewares ~f:(fun middleware ->
                      if String.equal middleware "IsAuth" then true else false)
                in
                let route =
                  match (default_route, custom_route) with
                  | Some default_route, None ->
                      String.uncapitalize default_route
                  | None, Some custom_route -> String.uncapitalize custom_route
                  | Some _, Some _ -> failwith "Something wrong happened"
                  | None, None -> failwith "Something wrong happened"
                in
                {
                  func_id = route_id;
                  http_method;
                  requires_where;
                  requires_search;
                  route;
                  requires_auth;
                  func_type = route_type;
                }) );
        ])

let generate_client_specs global_env app_declaration component_declarations
    page_declarations server_specs =
  let types_specs = Hashtbl.create ~size:17 (module String) in
  let services_specs = generate_service_specs server_specs in
  let _, app_title, _ = app_declaration in
  let auth_specs =
    match get_auth_config app_declaration with
    | Some
        {
          user_model;
          id_field;
          username_field;
          on_success_redirect_to;
          on_fail_redirect_to;
          _;
        } ->
        let signup_form =
          List.filter_map component_declarations
            ~f:(fun component_declaration ->
              let _, id, typ, args, body = component_declaration in

              match typ with
              | Component.SignupForm | Component.LoginForm ->
                  Some (generate_action_form_components_specs id args body)
              | _ -> None)
          |> List.hd_exn
        in
        let login_form =
          List.filter_map component_declarations
            ~f:(fun component_declaration ->
              let _, id, typ, args, body = component_declaration in

              match typ with
              | Component.LoginForm ->
                  Some (generate_action_form_components_specs id args body)
              | _ -> None)
          |> List.hd_exn
        in
        let logout_button =
          List.filter_map component_declarations
            ~f:(fun component_declaration ->
              let _, id, typ, args, body = component_declaration in

              match typ with
              | Component.LogoutButton ->
                  Some (generate_action_button_components_specs id args body)
              | _ -> None)
          |> List.hd_exn
        in
        Some
          {
            signup_form;
            login_form;
            logout_button;
            user_model;
            id_field;
            username_field;
            on_success_redirect_to;
            on_fail_redirect_to;
          }
    | None -> None
  in
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
        let _, id, typ, args, body = component_declaration in
        match typ with
        | Component.FindMany | Component.FindUnique ->
            Some
              (generate_find_component_specs global_env id args body types_specs)
        | _ -> None)
  in
  let action_form_components_specs =
    List.filter_map component_declarations ~f:(fun component_declaration ->
        let _, id, typ, args, body = component_declaration in
        match typ with
        | Component.Create | Component.Update ->
            Some
              (generate_action_form_components_specs ~global_env id args body)
        | _ -> None)
  in
  let action_button_components_specs =
    List.filter_map component_declarations ~f:(fun component_declaration ->
        let _, id, typ, args, body = component_declaration in
        match typ with
        | Component.Delete ->
            Some
              (generate_action_button_components_specs id args body ~global_env)
        | _ -> None)
  in
  let custom_components_specs =
    List.filter_map component_declarations ~f:(fun component_declaration ->
        let _, id, typ, args, body = component_declaration in
        match typ with
        | Component.Custom ->
            let args = get_component_args args in
            let imports, fn =
              (match body with
              | Component.CustomBody (imports, fn) -> Some (imports, fn)
              | _ -> None)
              |> Option.value_exn
            in
            Some { id; args; imports; fn }
        | _ -> None)
  in
  let pages_specs =
    List.map page_declarations ~f:(fun declaration_page ->
        generate_page_specs global_env declaration_page auth_specs)
  in
  let client_deps = get_client_deps app_declaration in
  {
    app_title;
    general_components_specs;
    find_components_specs;
    action_form_components_specs;
    action_button_components_specs;
    custom_components_specs;
    pages_specs;
    types_specs;
    auth_specs;
    services_specs;
    client_deps;
  }
