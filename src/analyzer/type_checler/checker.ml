open Core
open Ast.Ast_types
open Ast.Formatter
open Ast.Helper
open Error_handler.Handler
open Environment
open Model_checker
open Query_checker
open Xra_checker

let check_app_declaration global_env app_declaration =
  let loc, _, _ = app_declaration in
  match get_auth_config app_declaration with
  | Some { user_model; id_field; username_field; password_field; _ } ->
      if GlobalEnvironment.contains global_env ~key:user_model then (
        let user_model_table =
          GlobalEnvironment.lookup global_env ~key:user_model
          |> GlobalEnvironment.get_model_value
        in

        if LocalEnvironment.contains user_model_table ~key:id_field then (
          let id_field_attrs =
            (LocalEnvironment.lookup user_model_table ~key:id_field)
              .field_attrs_table
          in
          if
            not
              (LocalEnvironment.contains id_field_attrs ~key:"@unique"
              || LocalEnvironment.contains id_field_attrs ~key:"@id")
          then
            raise_unique_field_error loc Model.UniqueField id_field
              Model.NonUniqueField)
        else
          raise_undefined_error loc "Field" id_field ~declaration_id:user_model
            ~declaration_type:ModelDeclaration;

        if LocalEnvironment.contains user_model_table ~key:username_field then (
          let username_field_table =
            LocalEnvironment.lookup user_model_table ~key:username_field
          in
          let username_field_attrs = username_field_table.field_attrs_table in
          let username_field_type = username_field_table.typ in
          if
            not
              (String.equal
                 (string_of_type username_field_type)
                 (string_of_scalar_type String))
          then
            raise_type_error loc ~received_value:username_field
              ~received_type:username_field_type (Scalar String);
          if not (LocalEnvironment.contains username_field_attrs ~key:"@unique")
          then
            raise_unique_field_error loc Model.UniqueField username_field
              Model.NonUniqueField)
        else
          raise_undefined_error loc "Field" username_field
            ~declaration_id:user_model ~declaration_type:ModelDeclaration;

        if LocalEnvironment.contains user_model_table ~key:password_field then (
          let password_field_type =
            (LocalEnvironment.lookup user_model_table ~key:password_field).typ
          in
          if
            not
              (String.equal
                 (string_of_type password_field_type)
                 (string_of_scalar_type String))
          then
            raise_type_error loc ~received_value:password_field
              ~received_type:password_field_type (Scalar String))
        else
          raise_undefined_error loc "Field" password_field
            ~declaration_id:user_model ~declaration_type:ModelDeclaration)
      else raise_undefined_error loc "Model" user_model
  | None -> ()

let check_declarations global_env app_declaration declarations =
  let check_declaration declaration =
    match declaration with
    | Model model ->
        let _, id, _ = model in
        let model_value =
          GlobalEnvironment.lookup global_env ~key:id
          |> GlobalEnvironment.get_model_value
        in
        check_model global_env model_value model
    | Query query -> check_query global_env app_declaration query
    | Component (loc, id, typ, args, body) ->
        let xra_env = XRAEnvironment.create_env () in
        check_component global_env xra_env app_declaration loc id typ args body
    | Page (_, _, _, _, body) ->
        let xra_env = XRAEnvironment.create_env () in
        check_page global_env xra_env body
  in
  List.iter ~f:(fun declaration -> check_declaration declaration) declarations

let run global_env ast : unit =
  EnvironmentManager.populate global_env ast;
  let app_declaration, declarations = ast in
  check_app_declaration global_env app_declaration;
  check_declarations global_env app_declaration declarations
