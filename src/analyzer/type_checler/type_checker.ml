open Core
open Ast.Ast_types
open Environment
open Model_checker
open Query_checker

let check_declaration global_env declarations =
  let check_declaration declaration =
    match declaration with
    | Model (_, id, body) ->
        let model_value =
          GlobalEnvironment.get_value global_env ~key:id
          |> GlobalEnvironment.get_model_value
        in
        check_model global_env model_value id body
    | Query query -> check_query global_env query
    | Component _ -> ()
    | Page _ -> ()
  in
  List.iter ~f:(fun declaration -> check_declaration declaration) declarations

let run_type_checker (Ast declarations) : unit =
  let global_env = GlobalEnvironment.create () in
  EnvironmentManager.populate global_env (Ast declarations);
  check_declaration global_env declarations
