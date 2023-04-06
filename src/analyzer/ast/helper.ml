open Core
open Ast_types

let get_scalar_type typ =
  match typ with
  | Scalar scalar_type -> scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> scalar_type
      | Optional scalar_type -> scalar_type)

let get_custom_type typ =
  let get_custom_scalar_type scalar_type =
    match scalar_type with CustomType str -> Some str | _ -> None
  in
  match typ with
  | Scalar scalar_type -> get_custom_scalar_type scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> get_custom_scalar_type scalar_type
      | Optional scalar_type -> get_custom_scalar_type scalar_type)

let is_custom_type typ =
  let get_custom_scalar_type scalar_type =
    match scalar_type with CustomType _ -> true | _ -> false
  in
  match typ with
  | Scalar scalar_type -> get_custom_scalar_type scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> get_custom_scalar_type scalar_type
      | Optional scalar_type -> get_custom_scalar_type scalar_type)

let is_list_type typ =
  match typ with
  | Scalar _ -> false
  | Composite composite_type -> (
      match composite_type with List _ -> true | Optional _ -> false)

let get_auth_config app_declaration =
  let _, _, app_configs = app_declaration in
  List.map app_configs ~f:(fun config ->
      match config with
      | Auth auth_configs ->
          let get_value field_name =
            List.Assoc.find auth_configs field_name ~equal:String.equal
            |> Option.value_exn
          in
          Some
            {
              user_model = get_value "userModel";
              id_field = get_value "idField";
              username_field = get_value "usernameField";
              password_field = get_value "passwordField";
              is_online_field = get_value "isOnlineField";
              last_active_field = get_value "lastActiveField";
              on_success_redirect_to = get_value "onSuccessRedirectTo";
              on_fail_redirect_to = get_value "onFailRedirectTo";
            }
      | _ -> None)
  |> List.find ~f:(function Some _ -> true | None -> false)
  |> Option.value_or_thunk ~default:(fun () -> None)

let get_client_deps app_declaration =
  let _, _, app_configs = app_declaration in
  List.map app_configs ~f:(fun config ->
      match config with ClientDep client_deps -> Some client_deps | _ -> None)
  |> List.find ~f:(function Some _ -> true | None -> false)
  |> Option.value_or_thunk ~default:(fun () -> None)

let get_server_deps app_declaration =
  let _, _, app_configs = app_declaration in
  List.map app_configs ~f:(fun config ->
      match config with ServerDep server_deps -> Some server_deps | _ -> None)
  |> List.find ~f:(function Some _ -> true | None -> false)
  |> Option.value_or_thunk ~default:(fun () -> None)
