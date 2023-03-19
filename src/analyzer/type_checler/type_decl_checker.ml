open Core
open Ast.Helper
open Ast.Formatter
open Error_handler.Handler
open Environment

let check_type_decl global_env type_decl_id fields =
  List.iter fields ~f:(fun (loc, _, typ) ->
      if is_custom_type typ then
        let scalar_type = string_of_scalar_type (get_scalar_type typ) in
        if not (GlobalEnvironment.contains global_env ~key:scalar_type) then
          raise_undefined_error loc "Type" scalar_type
            ~declaration_id:type_decl_id ~declaration_type:TypeDeclaration)
