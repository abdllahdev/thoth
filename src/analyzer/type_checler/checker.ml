open Core
open Ast.Ast_types
open Environment
open Model_checker
open Query_checker
open Xra_checker

(* TODO: check app declaration *)
let check_app_declaration _ = ()

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
  check_app_declaration app_declaration;
  check_declarations global_env app_declaration declarations
