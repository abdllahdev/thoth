open Core
open Ast.Ast_types
open Environment
open Model_checker
open Query_checker

let check_declaration global_env declarations =
  let check_declaration declaration =
    match declaration with
    | Model (_, id, body) ->
        let model_env =
          GlobalEnv.get_value global_env ~key:id |> Option.value_exn
        in
        check_model global_env model_env id body
    | Query query -> check_query global_env query
    | Component _ -> ()
    | Page _ -> ()
  in
  List.iter ~f:(fun declaration -> check_declaration declaration) declarations

let run_type_checker (Ast declarations) : unit =
  let global_env = GlobalEnv.create () in
  EnvironmentManager.populate global_env (Ast declarations);
  check_declaration global_env declarations
