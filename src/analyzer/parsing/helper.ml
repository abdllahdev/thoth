open Core
open Ast.Ast_types
open Ast.Formatter
open Error_handler.Handler

let parse_id loc id =
  let keywords =
    [
      "true";
      "false";
      "model";
      "query";
      "component";
      "let";
      "render";
      "Now";
      "on";
      "permission";
      "delete";
    ]
  in
  if List.exists ~f:(fun x -> String.equal x id) keywords then
    raise_reserved_keyword_error loc id
  else id

let parse_declaration_id loc id declaration_type =
  let first_char = String.nget id 0 in
  if Char.is_uppercase first_char then parse_id loc id
  else raise_name_error loc declaration_type

let parse_type ?(list_modifier = false) ?(optional_modifier = false) typ =
  let scalar_type =
    match typ with
    | "String" -> String
    | "Int" -> Int
    | "Boolean" -> Boolean
    | "DateTime" -> DateTime
    | _ -> CustomType typ
  in
  if list_modifier then Composite (List scalar_type)
  else if optional_modifier then Composite (Optional scalar_type)
  else Scalar scalar_type

let parse_permissions loc permissions =
  let check_permission permission =
    match permission with
    | "IsAuth" -> (loc, "isAuth")
    | "OwnsRecord" -> (loc, "OwnsRecord")
    | _ -> raise_undefined_error loc "permission" permission
  in
  List.map permissions ~f:check_permission

let rec check_dup_exists loc keys =
  match keys with
  | [] -> ()
  | hd :: tl ->
      if List.exists ~f:(String.equal hd) tl then
        raise_multi_definitions_error loc hd
      else check_dup_exists loc tl

let rec parse_app_configs loc id body =
  let obj_keys = List.map body ~f:(fun (key, _) -> key) in
  check_dup_exists loc obj_keys;
  let get_deps id deps_list =
    match deps_list with
    | ListObjField (loc, deps) ->
        List.map deps ~f:(fun dep ->
            match dep with
            | TupleObjField (_, (value1, value2)) -> (value1, value2)
            | _ -> raise_type_error loc (Scalar Tuple) ~id)
    | _ -> raise_type_error loc (Scalar List) ~id
  in
  List.map body ~f:(fun (key, value) ->
      match key with
      | "title" -> (
          match value with
          | StringObjField (_, value) -> Title value
          | _ -> raise_type_error loc (Scalar String) ~id:key)
      | "auth" -> (
          match value with
          | AssocObjField (loc, auth_obj) ->
              let obj_keys = List.map auth_obj ~f:(fun (key, _) -> key) in
              check_dup_exists loc obj_keys;
              check_auth_config_entries loc id obj_keys;
              Auth
                (List.map auth_obj ~f:(fun (key, value) ->
                     match key with
                     | "userModel" | "idField" | "usernameField"
                     | "passwordField" | "isOnlineField" | "lastActiveField"
                       -> (
                         match value with
                         | ReferenceObjField (_, value) -> (key, value)
                         | _ -> raise_type_error loc (Scalar Reference) ~id:key)
                     | "onSuccessRedirectTo" | "onFailRedirectTo" -> (
                         match value with
                         | StringObjField (_, value) -> (key, value)
                         | _ -> raise_type_error loc (Scalar String) ~id:key)
                     | entry -> raise_unexpected_entry_error loc id entry))
          | _ -> raise_type_error loc (Scalar Assoc))
      | "clientDep" -> ClientDep (get_deps id value)
      | "serverDep" -> ServerDep (get_deps id value)
      | entry -> raise_unexpected_entry_error loc id entry)

and check_auth_config_entries loc id keys =
  let required_entries =
    [
      "userModel";
      "idField";
      "usernameField";
      "passwordField";
      "isOnlineField";
      "lastActiveField";
      "onSuccessRedirectTo";
      "onFailRedirectTo";
    ]
  in
  List.iter required_entries ~f:(fun entry ->
      if not (List.exists keys ~f:(fun key -> String.equal key entry)) then
        raise_required_entry_error loc id entry)

let parse_xra_element loc opening_id closing_id attributes children =
  if not (String.equal opening_id closing_id) then
    raise_syntax_error loc closing_id
  else XRA.Element (loc, opening_id, attributes, children)

let unexpected_keys_exists loc id keys expected_keys =
  List.iter keys ~f:(fun key ->
      if not (List.mem expected_keys key ~equal:String.equal) then
        raise_unexpected_entry_error loc id key)

let rec check_dup_exists loc keys =
  match keys with
  | [] -> ()
  | hd :: tl ->
      if List.exists ~f:(String.equal hd) tl then
        raise_multi_definitions_error loc hd
      else check_dup_exists loc tl

let get_fn loc id body =
  let fn = List.Assoc.find body "fn" ~equal:String.equal in
  (match fn with
  | Some fn -> (
      match fn with
      | TsObjField (_, fn) -> Some (loc, fn)
      | _ -> raise_type_error loc (Scalar String))
  | None -> raise_required_entry_error loc id "fn")
  |> Option.value_or_thunk ~default:raise_compiler_error

let get_imports loc body =
  let imports = List.Assoc.find body "imports" ~equal:String.equal in
  match imports with
  | Some imports -> (
      match imports with
      | TsObjField (loc, imports) -> Some (loc, imports)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None

let get_permissions attributes =
  let permissions =
    List.Assoc.find attributes "@permissions" ~equal:String.equal
  in
  match permissions with
  | Some (loc, args) ->
      Some
        (List.map args ~f:(fun arg ->
             match arg with
             | ReferenceLiteral (loc, id) -> (loc, id)
             | _ -> raise_type_error loc (Scalar Reference) ~id:"@permissions"))
  | None -> None

let get_model attributes =
  let models = List.Assoc.find attributes "@model" ~equal:String.equal in
  match models with
  | Some (loc, args) -> (
      if List.length args > 1 || List.length args < 1 then
        raise_argument_number_error loc 1 (List.length args) "@model";
      let arg = List.hd_exn args in
      match arg with
      | ReferenceLiteral (loc, id) -> Some (loc, id)
      | _ -> raise_type_error loc (Scalar Reference) ~id:"@model")
  | None -> None

let get_route attributes =
  let models = List.Assoc.find attributes "@route" ~equal:String.equal in
  match models with
  | Some (loc, args) -> (
      if List.length args > 1 || List.length args < 1 then
        raise_argument_number_error loc 1 (List.length args) "@route";
      let arg = List.hd_exn args in
      match arg with
      | StringLiteral (loc, route) -> Some (loc, route)
      | _ -> raise_type_error loc (Scalar String) ~id:"@route")
  | None -> None

let parse_page_attributes loc id attributes =
  match attributes with
  | Some attributes ->
      let attribute_ids = List.map attributes ~f:(fun (id, _) -> id) in
      check_dup_exists loc attribute_ids;
      unexpected_keys_exists loc id attribute_ids [ "@permissions"; "@route" ];
      let permissions = get_permissions attributes in
      let route =
        match get_route attributes with
        | Some route -> route
        | None -> raise_required_page_attribute loc id "@route"
      in
      (route, permissions)
  | None -> raise_required_page_attribute loc id "@route"

let rec parse_query loc id typ attributes return_type body =
  let obj_keys = List.map body ~f:(fun (key, _) -> key) in
  check_dup_exists loc obj_keys;
  check_query_unexpected_keys loc id obj_keys typ;
  let query_body =
    match typ with
    | Query.FindMany ->
        let search = get_search loc id body in
        [ search ]
    | Query.FindUnique | Query.Delete ->
        let where = get_where loc id body in
        [ where ]
    | Query.Create ->
        let data = get_data loc id body in
        [ data ]
    | Query.Update ->
        let where = get_where loc id body in
        let data = get_data loc id body in
        [ where; data ]
    | Query.Custom ->
        let imports =
          match get_imports loc body with
          | Some (loc, imports) -> [ Query.Imports (loc, imports) ]
          | None -> []
        in
        let fn =
          let loc, fn = get_fn loc id body in
          Query.Fn (loc, fn)
        in
        [ fn ] @ imports
  in
  let permissions, model, route =
    match attributes with
    | Some attributes ->
        let attribute_ids = List.map attributes ~f:(fun (id, _) -> id) in
        check_dup_exists loc attribute_ids;
        unexpected_keys_exists loc id attribute_ids
          [ "@model"; "@permissions"; "@route" ];
        let permissions = get_permissions attributes in
        let model = get_model attributes in
        let route = get_query_route attributes in
        (match typ with
        | Query.Custom -> (
            match route with
            | Some _ -> ()
            | None -> raise_required_query_attribute loc id typ "@route")
        | _ -> (
            match model with
            | Some _ -> ()
            | None -> raise_required_query_attribute loc id typ "@model"));
        (permissions, model, route)
    | None -> (
        match typ with
        | Query.Custom -> raise_required_query_attribute loc id typ "@route"
        | _ -> raise_required_query_attribute loc id typ "@model")
  in
  Query (loc, id, typ, return_type, query_body, model, permissions, route)

and get_query_route attributes =
  let models = List.Assoc.find attributes "@route" ~equal:String.equal in
  match models with
  | Some (loc, args) ->
      if List.length args > 2 || List.length args < 2 then
        raise_argument_number_error loc 2 (List.length args) "@route";
      let http_method =
        match List.nth_exn args 0 with
        | StringLiteral (loc, http_method) -> (
            match http_method with
            | "get" | "post" | "put" | "patch" | "delete" -> http_method
            | _ -> raise_undefined_http_method loc http_method)
        | _ -> raise_type_error loc (Scalar String) ~id:"@route"
      in
      let route =
        match List.nth_exn args 1 with
        | StringLiteral (_, route) -> route
        | _ -> raise_type_error loc (Scalar String) ~id:"@route"
      in
      Some (loc, http_method, route)
  | None -> None

and check_query_unexpected_keys loc id obj_keys typ =
  let expected_keys =
    match typ with
    | Query.FindMany -> [ "search" ]
    | Query.FindUnique -> [ "where" ]
    | Query.Delete -> [ "where" ]
    | Query.Create -> [ "data" ]
    | Query.Update -> [ "data"; "where" ]
    | Query.Custom -> [ "imports"; "fn" ]
  in
  unexpected_keys_exists loc id obj_keys expected_keys

and get_where loc id body =
  let where = List.Assoc.find body "where" ~equal:String.equal in
  (match where with
  | Some where -> (
      match where with
      | ListObjField (loc, where) ->
          Some
            (Query.Where
               ( loc,
                 List.map where ~f:(fun element ->
                     match element with
                     | ReferenceObjField (_, ref) -> ref
                     | _ -> raise_type_error loc (Scalar Reference)) ))
      | _ -> raise_type_error loc (Scalar List))
  | None -> raise_required_entry_error loc id "where")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_search loc id body =
  let search = List.Assoc.find body "search" ~equal:String.equal in
  (match search with
  | Some search -> (
      match search with
      | ListObjField (loc, search) ->
          Some
            (Query.Search
               ( loc,
                 List.map search ~f:(fun element ->
                     match element with
                     | ReferenceObjField (_, ref) -> ref
                     | _ -> raise_type_error loc (Scalar Reference)) ))
      | _ -> raise_type_error loc (Scalar List))
  | None -> raise_required_entry_error loc id "search")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_data loc id body =
  let data = List.Assoc.find body "data" ~equal:String.equal in
  (match data with
  | Some data -> (
      match data with
      | AssocObjField (_, value) ->
          let fields = get_data_fields loc id value in
          let relation_fields = get_data_relation_fields loc value in
          Some (Query.Data (loc, fields @ relation_fields))
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> raise_required_entry_error loc id "data")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_data_fields loc id body =
  let fields = List.Assoc.find body "fields" ~equal:String.equal in
  (match fields with
  | Some fields -> (
      match fields with
      | ListObjField (loc, fields) ->
          Some
            (List.map fields ~f:(fun element ->
                 match element with
                 | ReferenceObjField (_, ref) -> (ref, None)
                 | _ -> raise_type_error loc (Scalar Reference)))
      | _ -> raise_type_error loc (Scalar List))
  | None -> raise_required_entry_error loc id "fields")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_data_relation_fields loc body =
  let relation_fields =
    List.Assoc.find body "relationFields" ~equal:String.equal
  in
  match relation_fields with
  | Some relation_fields -> (
      match relation_fields with
      | AssocObjField (loc, relation_fields) ->
          List.map relation_fields ~f:(fun relation_field ->
              let id, value = relation_field in
              match value with
              | ConnectWithObjField (_, (value1, value2)) ->
                  (id, Some (value1, value2))
              | _ -> raise_type_error loc (Scalar ConnectWith))
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> []

let get_where_arg loc args =
  let where = List.Assoc.find args "where" ~equal:String.equal in
  match where with
  | Some where -> (
      match where with
      | ReferenceObjField (loc, ref) -> Some (loc, ReferenceLiteral (loc, ref))
      | IntObjField (loc, number) -> Some (loc, IntLiteral (loc, number))
      | _ ->
          failwith
            (Fmt.str "ArgumentError: (%s): Expected a reference or a literal"
               (string_of_loc loc)))
  | None -> None

let get_search_arg loc args =
  let search = List.Assoc.find args "search" ~equal:String.equal in
  match search with
  | Some search -> (
      match search with
      | AssocObjField (_, obj) ->
          Some
            (List.map obj ~f:(fun (id, value) ->
                 match value with
                 | StringObjField (loc, str) ->
                     (loc, id, StringLiteral (loc, str))
                 | BooleanObjField (loc, boolean) ->
                     (loc, id, BooleanLiteral (loc, boolean))
                 | IntObjField (loc, number) ->
                     (loc, id, IntLiteral (loc, number))
                 | ReferenceObjField (loc, ref) ->
                     (loc, id, ReferenceLiteral (loc, ref))
                 | _ ->
                     failwith
                       (Fmt.str
                          "ArgumentError: (%s): Expected a reference or a \
                           literal"
                          (string_of_loc loc))))
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> None

let rec parse_component loc id typ args body =
  let obj_keys = List.map body ~f:(fun (key, _) -> key) in
  check_dup_exists loc obj_keys;
  check_component_unexpected_keys loc id obj_keys typ;
  let component_body =
    match typ with
    | Component.FindMany | Component.FindUnique ->
        let find_query, variable = get_find_query_and_variable loc id body in
        let on_error = get_on_error loc id body in
        let on_loading = get_on_loading loc id body in
        let on_success = get_on_success loc id body in
        Component.FindBody
          ((find_query, variable), on_error, on_loading, on_success)
    | Component.Create ->
        let query : Component.query_application =
          get_action_query loc id body
        in
        let global_style = get_global_style loc body in
        let form_inputs = get_form_inputs loc id body in
        let form_button = get_form_button loc id body in
        Component.CreateBody (query, global_style, form_inputs, form_button)
    | Component.Update ->
        let query = get_action_query loc id body in
        let global_style = get_global_style loc body in
        let form_inputs = get_form_inputs loc id body in
        let form_button = get_form_button loc id body in
        Component.UpdateBody (query, global_style, form_inputs, form_button)
    | Component.Delete ->
        let query = get_action_query loc id body in
        let form_button = get_form_button loc id body in
        Component.DeleteBody (query, form_button)
    | Component.SignupForm ->
        let global_style = get_global_style loc body in
        let form_inputs = get_form_inputs loc id body in
        let form_button = get_form_button loc id body in
        Component.SignupFormBody (global_style, form_inputs, form_button)
    | Component.LoginForm ->
        let global_style = get_global_style loc body in
        let form_inputs = get_form_inputs loc id body in
        let form_button = get_form_button loc id body in
        Component.LoginFormBody (global_style, form_inputs, form_button)
    | Component.LogoutButton ->
        let form_button = get_form_button loc id body in
        Component.LogoutButtonBody form_button
    | Component.Custom ->
        let imports =
          match get_imports loc body with
          | Some (_, imports) -> Some imports
          | None -> None
        in
        let _, fn = get_fn loc id body in
        Component.CustomBody (imports, fn)
    | Component.General -> raise_compiler_error ()
  in
  Component (loc, id, typ, args, component_body)

and check_component_unexpected_keys loc id obj_keys typ =
  let expected_keys =
    match typ with
    | Component.FindMany | Component.FindUnique ->
        [ "findQuery"; "onError"; "onLoading"; "onSuccess" ]
    | Component.Create | Component.Update ->
        [ "actionQuery"; "globalStyle"; "formInputs"; "formButton" ]
    | Component.SignupForm | Component.LoginForm ->
        [ "globalStyle"; "formInputs"; "formButton" ]
    | Component.Delete -> [ "actionQuery"; "formButton" ]
    | Component.LogoutButton -> [ "formButton" ]
    | Component.Custom -> [ "fn"; "imports" ]
    | _ -> raise_compiler_error ()
  in
  unexpected_keys_exists loc id obj_keys expected_keys

and get_find_query_and_variable loc id body =
  let find_query = List.Assoc.find body "findQuery" ~equal:String.equal in
  (match find_query with
  | Some find_query -> (
      match find_query with
      | AsObjField (loc, query_application, var) -> (
          match query_application with
          | QueryAppObjField (loc, query_id, args) -> (
              match args with
              | Some args -> (
                  match args with
                  | AssocObjField (_, args) ->
                      let query_application =
                        ( loc,
                          query_id,
                          get_where_arg loc args,
                          get_search_arg loc args )
                      in
                      Some (query_application, (loc, var))
                  | _ -> raise_type_error loc (Scalar Assoc))
              | None -> Some ((loc, query_id, None, None), (loc, var)))
          | _ ->
              failwith
                (Fmt.str "@(%s): Expected query application" (string_of_loc loc))
          )
      | _ -> raise_type_error loc (Scalar As))
  | None -> raise_required_entry_error loc id "findQuery")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_action_query loc id body =
  let action_query = List.Assoc.find body "actionQuery" ~equal:String.equal in
  (match action_query with
  | Some action_query -> (
      match action_query with
      | QueryAppObjField (loc, query_id, args) -> (
          match args with
          | Some args -> (
              match args with
              | AssocObjField (_, args) ->
                  let query_application =
                    ( loc,
                      query_id,
                      get_where_arg loc args,
                      get_search_arg loc args )
                  in
                  Some query_application
              | _ -> raise_type_error loc (Scalar Assoc))
          | None -> Some (loc, query_id, None, None))
      | _ ->
          failwith
            (Fmt.str "@(%s): Expected query application" (string_of_loc loc)))
  | None -> raise_required_entry_error loc id "actionQuery")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_on_error loc id body =
  let on_error = List.Assoc.find body "onError" ~equal:String.equal in
  (match on_error with
  | Some on_error -> (
      match on_error with
      | RenderObjField (_, on_error) -> Some on_error
      | _ -> raise_type_error loc (Scalar String))
  | None -> raise_required_entry_error loc id "onError")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_on_loading loc id body =
  let on_loading = List.Assoc.find body "onLoading" ~equal:String.equal in
  (match on_loading with
  | Some on_loading -> (
      match on_loading with
      | RenderObjField (_, on_loading) -> Some on_loading
      | _ -> raise_type_error loc (Scalar String))
  | None -> raise_required_entry_error loc id "onLoading")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_on_success loc id body =
  let on_success = List.Assoc.find body "onSuccess" ~equal:String.equal in
  (match on_success with
  | Some on_success -> (
      match on_success with
      | RenderObjField (_, on_success) -> Some on_success
      | _ -> raise_type_error loc (Scalar String))
  | None -> raise_required_entry_error loc id "onSuccess")
  |> Option.value_or_thunk ~default:raise_compiler_error

and get_form_inputs loc id body =
  let form_inputs = List.Assoc.find body "formInputs" ~equal:String.equal in
  match form_inputs with
  | Some form_inputs -> (
      match form_inputs with
      | AssocObjField (loc, form_inputs) ->
          let obj_keys = List.map form_inputs ~f:(fun (key, _) -> key) in
          check_dup_exists loc obj_keys;
          List.map form_inputs ~f:(fun (id, input_obj) ->
              match input_obj with
              | AssocObjField (loc, form_input) ->
                  let style, label, input = get_from_input loc id form_input in
                  (loc, id, style, label, input)
              | _ -> raise_type_error loc (Scalar Assoc))
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> raise_required_entry_error loc id "formInputs"

and get_from_input loc id body =
  let obj_keys = List.map body ~f:(fun (key, _) -> key) in
  check_dup_exists loc obj_keys;
  let expected_keys = [ "style"; "label"; "input" ] in
  unexpected_keys_exists loc id obj_keys expected_keys;
  let style = get_style loc body in
  let label = get_input_label_attrs loc id body in
  let input = get_form_input_attrs loc id body in
  (style, label, input)

and get_form_input_attrs loc id body =
  let input = List.Assoc.find body "input" ~equal:String.equal in
  match input with
  | Some input -> (
      match input with
      | AssocObjField (loc, input_attrs) ->
          let obj_keys = List.map input_attrs ~f:(fun (key, _) -> key) in
          check_dup_exists loc obj_keys;
          let expected_keys =
            [ "type"; "isVisible"; "defaultValue"; "placeholder"; "style" ]
          in
          unexpected_keys_exists loc id obj_keys expected_keys;
          let input_type =
            match get_input_type loc id input_attrs with
            | Some input_type -> [ input_type ]
            | None -> raise_required_entry_error loc id "type"
          in
          let input_visibility =
            match get_input_visibility loc input_attrs with
            | Some input_visibility -> [ input_visibility ]
            | None -> [ Component.FormAttrVisibility true ]
          in
          let input_default_value =
            match get_input_default_value input_attrs with
            | Some input_default_value -> [ input_default_value ]
            | None -> []
          in
          let input_placeholder =
            match get_input_placeholder loc input_attrs with
            | Some input_placeholder -> [ input_placeholder ]
            | None -> []
          in
          let style =
            match get_style loc input_attrs with
            | Some style -> [ style ]
            | None -> []
          in
          input_type @ input_visibility @ input_default_value
          @ input_placeholder @ style
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> raise_required_entry_error loc id "input"

and get_input_label_attrs loc id body =
  let input_label = List.Assoc.find body "label" ~equal:String.equal in
  match input_label with
  | Some label -> (
      match label with
      | AssocObjField (loc, label_attrs) ->
          let obj_keys = List.map label_attrs ~f:(fun (key, _) -> key) in
          check_dup_exists loc obj_keys;
          let expected_keys = [ "name"; "style" ] in
          unexpected_keys_exists loc id obj_keys expected_keys;
          let style =
            match get_style loc label_attrs with
            | Some style -> [ style ]
            | None -> []
          in
          let name =
            match get_name loc label_attrs with
            | Some name -> [ name ]
            | None -> raise_required_entry_error loc id "name"
          in
          Some (name @ style)
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> None

and get_form_button loc id body =
  let button = List.Assoc.find body "formButton" ~equal:String.equal in
  match button with
  | Some button -> (
      match button with
      | AssocObjField (loc, button_attrs) ->
          let obj_keys = List.map button_attrs ~f:(fun (key, _) -> key) in
          check_dup_exists loc obj_keys;
          let expected_keys = [ "name"; "style" ] in
          unexpected_keys_exists loc id obj_keys expected_keys;
          let style =
            match get_style loc button_attrs with
            | Some style -> [ style ]
            | None -> []
          in
          let name =
            match get_name loc button_attrs with
            | Some name -> [ name ]
            | None -> raise_required_entry_error loc id "name"
          in
          name @ style
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> raise_required_entry_error loc id "formButton"

and get_global_style loc body =
  let global_style = List.Assoc.find body "globalStyle" ~equal:String.equal in
  match global_style with
  | Some global_style -> (
      match global_style with
      | AssocObjField (_, global_style) ->
          Some
            (List.map global_style ~f:(fun style ->
                 let id, style = style in
                 match style with
                 | StringObjField (loc, style) ->
                     (loc, id, Component.FormAttrStyle style)
                 | _ -> raise_type_error loc (Scalar String)))
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> None

and get_style loc body =
  let style = List.Assoc.find body "style" ~equal:String.equal in
  match style with
  | Some style -> (
      match style with
      | StringObjField (_, style) -> Some (Component.FormAttrStyle style)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None

and get_name loc body =
  let name = List.Assoc.find body "name" ~equal:String.equal in
  match name with
  | Some name -> (
      match name with
      | StringObjField (_, name) -> Some (Component.FormAttrName name)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None

and get_input_type loc id body =
  let input_type = List.Assoc.find body "type" ~equal:String.equal in
  match input_type with
  | Some input_type -> (
      match input_type with
      | ReferenceObjField (loc, input_type) -> (
          match input_type with
          | "TextInput" -> Some (Component.FormAttrType Component.TextInput)
          | "EmailInput" -> Some (Component.FormAttrType Component.EmailInput)
          | "PasswordInput" ->
              Some (Component.FormAttrType Component.PasswordInput)
          | "NumberInput" -> Some (Component.FormAttrType Component.NumberInput)
          | "RelationInput" ->
              Some (Component.FormAttrType Component.RelationInput)
          | _ -> raise_unexpected_entry_error loc id input_type)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None

and get_input_visibility loc body =
  let visibility = List.Assoc.find body "isVisible" ~equal:String.equal in
  match visibility with
  | Some visibility -> (
      match visibility with
      | BooleanObjField (_, visibility) ->
          Some (Component.FormAttrVisibility visibility)
      | _ -> raise_type_error loc (Scalar Boolean))
  | None -> None

and get_input_default_value body =
  let default_value = List.Assoc.find body "defaultValue" ~equal:String.equal in
  match default_value with
  | Some default_value -> (
      match default_value with
      | StringObjField (loc, default_value) ->
          Some
            (Component.FormAttrDefaultValue
               (StringObjField (loc, default_value)))
      | BooleanObjField (loc, default_value) ->
          Some
            (Component.FormAttrDefaultValue
               (BooleanObjField (loc, default_value)))
      | IntObjField (loc, default_value) ->
          Some
            (Component.FormAttrDefaultValue (IntObjField (loc, default_value)))
      | ReferenceObjField (loc, default_value) ->
          Some
            (Component.FormAttrDefaultValue
               (ReferenceObjField (loc, default_value)))
      | DotReferenceObjField (loc, (left, right)) ->
          Some
            (Component.FormAttrDefaultValue
               (DotReferenceObjField (loc, (left, right))))
      | AssocObjField (loc, default_value) ->
          Some
            (Component.FormAttrDefaultValue (AssocObjField (loc, default_value)))
      | ConnectWithObjField (loc, (value1, value2)) ->
          Some
            (Component.FormAttrDefaultValue
               (AssocObjField
                  ( loc,
                    [
                      ( "connect",
                        AssocObjField
                          (loc, [ (value1, ReferenceObjField (loc, value2)) ])
                      );
                    ] )))
      | _ -> raise_compiler_error ())
  | None -> None

and get_input_placeholder loc body =
  let placeholder = List.Assoc.find body "placeholder" ~equal:String.equal in
  match placeholder with
  | Some placeholder -> (
      match placeholder with
      | StringObjField (_, placeholder) ->
          Some (Component.FormAttrPlaceholder placeholder)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None
