open Core
open Ast.Formatter
open Ast.Ast_types
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
  if list_modifier && optional_modifier then
    Composite (OptionalList scalar_type)
  else if list_modifier then Composite (List scalar_type)
  else if optional_modifier then Composite (Optional scalar_type)
  else Scalar scalar_type

let parse_permissions loc permissions =
  let check_permission permission =
    match permission with
    | "IsAuth" -> (loc, "isAuth")
    | "OwnsEntry" -> (loc, "ownsEntry")
    | _ -> raise_undefined_error loc "permission" permission
  in
  List.map permissions ~f:check_permission

let parse_app_configs loc obj =
  List.fold obj ~init:[] ~f:(fun seen (key, _) ->
      if List.mem seen key ~equal:String.equal then
        raise_multi_definitions_error loc key
      else seen @ [ key ])
  |> ignore;
  List.map obj ~f:(fun (key, value) ->
      match key with
      | "title" -> (
          match value with
          | StringObjField (_, value) -> Title value
          | _ -> raise_type_error loc (Scalar String))
      | "auth" -> (
          match value with
          | AssocObjField (loc, auth_obj) ->
              Auth
                (List.fold auth_obj ~init:[] ~f:(fun seen (key, _) ->
                     if List.mem seen key ~equal:String.equal then
                       raise_multi_definitions_error loc key
                     else seen @ [ key ])
                 |> ignore;
                 List.map auth_obj ~f:(fun (key, value) ->
                     match key with
                     | "userModel" | "idField" | "usernameField"
                     | "passwordField" -> (
                         match value with
                         | ReferenceObjField (_, value) -> (key, value)
                         | _ -> raise_type_error loc (Scalar Reference))
                     | "onSuccessRedirectTo" | "onFailRedirectTo" -> (
                         match value with
                         | StringObjField (_, value) -> (key, value)
                         | _ -> raise_type_error loc (Scalar String))
                     | config -> raise_unexpected_config loc config))
          | _ -> raise_type_error loc (Scalar Assoc))
      | config -> raise_unexpected_config loc config)

let parse_xra_element loc opening_id closing_id attributes children =
  if not (String.equal opening_id closing_id) then
    raise_syntax_error loc closing_id
  else XRA.Element (loc, opening_id, attributes, children)

let rec dup_exist loc keys =
  match keys with
  | [] -> ()
  | hd :: tl ->
      if List.exists ~f:(String.equal hd) tl then
        raise_multi_definitions_error loc hd
      else dup_exist loc tl

let unexpected_keys_exists loc keys expected_keys =
  List.iter keys ~f:(fun key ->
      if not (List.mem expected_keys key ~equal:String.equal) then
        raise_unexpected_argument_error loc key)

let rec parse_component_body loc obj component_type =
  let obj_keys = List.map obj ~f:(fun (key, _) -> key) in
  dup_exist loc obj_keys;

  (match component_type with
  | "FIND_MANY" | "FIND_UNIQUE" ->
      let expected_keys = [ "onError"; "onLoading"; "onSuccess" ] in
      unexpected_keys_exists loc obj_keys expected_keys
  | "CREATE" | "UPDATE" | "SIGNUP_FORM" | "LOGIN_FORM" ->
      let expected_keys = [ "style"; "formInputs"; "formButton" ] in
      unexpected_keys_exists loc obj_keys expected_keys
  | "DELETE" | "LOGOUT_BUTTON" ->
      let expected_keys = [ "formButton" ] in
      unexpected_keys_exists loc obj_keys expected_keys
  | _ -> failwith "CompilationError: Something wrong happened");

  match component_type with
  | "FIND_MANY" | "FIND_UNIQUE" ->
      let on_error = get_on_error loc obj in
      let on_loading = get_on_loading loc obj in
      let on_success = get_on_success loc obj in
      Component.FindBody (on_error, on_loading, on_success)
  | "CREATE" ->
      let style = get_style loc obj in
      let form_inputs = get_form_inputs loc obj in
      let form_button = get_form_button loc obj in
      Component.CreateBody (style, form_inputs, form_button)
  | "UPDATE" ->
      let style = get_style loc obj in
      let form_inputs = get_form_inputs loc obj in
      let form_button = get_form_button loc obj in
      Component.UpdateBody (style, form_inputs, form_button)
  | "DELETE" ->
      let form_button = get_form_button loc obj in
      Component.DeleteBody form_button
  | "SIGNUP_FORM" ->
      let style = get_style loc obj in
      let form_inputs = get_form_inputs loc obj in
      let form_button = get_form_button loc obj in
      Component.SignupFormBody (style, form_inputs, form_button)
  | "LOGIN_FORM" ->
      let style = get_style loc obj in
      let form_inputs = get_form_inputs loc obj in
      let form_button = get_form_button loc obj in
      Component.LoginFormBody (style, form_inputs, form_button)
  | "LOGOUT_BUTTON" ->
      let form_button = get_form_button loc obj in
      Component.LogoutButtonBody form_button
  | _ -> failwith "CompilationError: Something wrong happened"

and get_on_error loc obj =
  let on_error = List.Assoc.find obj "onError" ~equal:String.equal in
  (match on_error with
  | Some on_error -> (
      match on_error with
      | RenderObjField (_, on_error) -> Some on_error
      | _ -> raise_type_error loc (Scalar String))
  | None -> failwith "Not found")
  |> Option.value_or_thunk ~default:(fun () -> failwith "Something went wrong")

and get_on_loading loc obj =
  let on_loading = List.Assoc.find obj "onLoading" ~equal:String.equal in
  (match on_loading with
  | Some on_loading -> (
      match on_loading with
      | RenderObjField (_, on_loading) -> Some on_loading
      | _ -> raise_type_error loc (Scalar String))
  | None -> failwith "Not found")
  |> Option.value_or_thunk ~default:(fun () -> failwith "Something went wrong")

and get_on_success loc obj =
  let on_success = List.Assoc.find obj "onSuccess" ~equal:String.equal in
  (match on_success with
  | Some on_success -> (
      match on_success with
      | RenderObjField (_, on_success) -> Some on_success
      | _ -> raise_type_error loc (Scalar String))
  | None -> failwith "Not found")
  |> Option.value_or_thunk ~default:(fun () -> failwith "Something went wrong")

and get_form_inputs loc obj =
  let form_inputs = List.Assoc.find obj "formInputs" ~equal:String.equal in
  match form_inputs with
  | Some form_inputs -> (
      match form_inputs with
      | AssocObjField (loc, form_inputs) ->
          let obj_keys = List.map form_inputs ~f:(fun (key, _) -> key) in
          dup_exist loc obj_keys;
          List.map form_inputs ~f:(fun (id, input_obj) ->
              match input_obj with
              | AssocObjField (loc, form_input) ->
                  let style, label, input = get_from_input loc form_input in
                  (loc, id, style, label, input)
              | _ -> raise_type_error loc (Scalar Assoc))
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> failwith (Fmt.str "@(%s): Expected formInputs" (string_of_loc loc))

and get_from_input loc obj =
  let obj_keys = List.map obj ~f:(fun (key, _) -> key) in
  dup_exist loc obj_keys;
  let expected_keys = [ "style"; "label"; "input" ] in
  unexpected_keys_exists loc obj_keys expected_keys;
  let style = get_style loc obj in
  let label = get_input_label_attrs loc obj in
  let input = get_form_input_attrs loc obj in
  (style, label, input)

and get_form_input_attrs loc obj =
  let input = List.Assoc.find obj "input" ~equal:String.equal in
  match input with
  | Some input -> (
      match input with
      | AssocObjField (loc, input_attrs) ->
          let obj_keys = List.map input_attrs ~f:(fun (key, _) -> key) in
          dup_exist loc obj_keys;
          let expected_keys =
            [ "type"; "isVisible"; "defaultValue"; "placeholder"; "style" ]
          in
          unexpected_keys_exists loc obj_keys expected_keys;
          let input_type =
            match get_input_type loc input_attrs with
            | Some input_type -> [ input_type ]
            | None -> failwith "Expected input_type attribute"
          in
          let input_visibility =
            match get_input_visibility loc input_attrs with
            | Some input_visibility -> [ input_visibility ]
            | None -> failwith "Expected visibility attribute"
          in
          let input_default_value =
            match get_input_default_value loc input_attrs with
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
  | None -> failwith "Must have an input attribute"

and get_input_label_attrs loc obj =
  let input_label = List.Assoc.find obj "label" ~equal:String.equal in
  match input_label with
  | Some label -> (
      match label with
      | AssocObjField (loc, label_attrs) ->
          let obj_keys = List.map label_attrs ~f:(fun (key, _) -> key) in
          dup_exist loc obj_keys;
          let expected_keys = [ "name"; "style" ] in
          unexpected_keys_exists loc obj_keys expected_keys;
          let style =
            match get_style loc label_attrs with
            | Some style -> [ style ]
            | None -> []
          in
          let name =
            match get_name loc label_attrs with
            | Some name -> [ name ]
            | None -> failwith "Expected name attribute"
          in
          Some (name @ style)
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> None

and get_form_button loc obj =
  let button = List.Assoc.find obj "formButton" ~equal:String.equal in
  match button with
  | Some button -> (
      match button with
      | AssocObjField (loc, button_attrs) ->
          let obj_keys = List.map button_attrs ~f:(fun (key, _) -> key) in
          dup_exist loc obj_keys;
          let expected_keys = [ "name"; "style" ] in
          unexpected_keys_exists loc obj_keys expected_keys;
          let style =
            match get_style loc button_attrs with
            | Some style -> [ style ]
            | None -> []
          in
          let name =
            match get_name loc button_attrs with
            | Some name -> [ name ]
            | None -> failwith "Expected name attribute"
          in
          name @ style
      | _ -> raise_type_error loc (Scalar Assoc))
  | None -> failwith "Must have a button attribute"

and get_style loc obj =
  let style = List.Assoc.find obj "style" ~equal:String.equal in
  match style with
  | Some style -> (
      match style with
      | StringObjField (_, style) -> Some (Component.FormAttrStyle style)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None

and get_name loc obj =
  let name = List.Assoc.find obj "name" ~equal:String.equal in
  match name with
  | Some name -> (
      match name with
      | StringObjField (_, name) -> Some (Component.FormAttrName name)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None

and get_input_type loc obj =
  let input_type = List.Assoc.find obj "type" ~equal:String.equal in
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
          | _ -> raise_unexpected_config loc input_type)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None

and get_input_visibility loc obj =
  let visibility = List.Assoc.find obj "isVisible" ~equal:String.equal in
  match visibility with
  | Some visibility -> (
      match visibility with
      | BooleanObjField (_, visibility) ->
          Some (Component.FormAttrVisibility visibility)
      | _ -> raise_type_error loc (Scalar Boolean))
  | None -> None

and get_input_default_value _ obj =
  let default_value = List.Assoc.find obj "defaultValue" ~equal:String.equal in
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
      | _ -> failwith "unimplemented")
  | None -> None

and get_input_placeholder loc obj =
  let placeholder = List.Assoc.find obj "placeholder" ~equal:String.equal in
  match placeholder with
  | Some placeholder -> (
      match placeholder with
      | StringObjField (_, placeholder) ->
          Some (Component.FormAttrPlaceholder placeholder)
      | _ -> raise_type_error loc (Scalar String))
  | None -> None
