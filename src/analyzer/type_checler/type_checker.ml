open Core
open Ast.Ast_types
open Environment
open Model_checker
open Query_checker
open Xra_checker

let check_declaration global_env declarations =
  let check_declaration declaration =
    match declaration with
    | Model model ->
        let _, id, _ = model in
        let model_value =
          GlobalEnvironment.lookup global_env ~key:id
          |> GlobalEnvironment.get_model_value
        in
        check_model global_env model_value model
    | Query query -> check_query global_env query
    | Component (_, _, typ, args, body) ->
        let xra_env = XRAEnvironment.create_env () in
        check_component global_env xra_env typ args body
    | Page (_, _, _, _, body) ->
        let xra_env = XRAEnvironment.create_env () in
        check_general_body global_env xra_env body
  in
  List.iter ~f:(fun declaration -> check_declaration declaration) declarations

let run_type_checker (Ast declarations) : unit =
  let global_env = GlobalEnvironment.create () in
  EnvironmentManager.populate global_env (Ast declarations);
  check_declaration global_env declarations
