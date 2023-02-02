open Core
open Ast.Ast_types
open Environment
open Model_checker
open Query_checker

let check_declaration (global_env : 'a GlobalEnv.t)
    (declarations : declaration list) : unit =
  let check_declaration declaration =
    match declaration with
    | Model (_, id, body) ->
        let model_table =
          GlobalEnv.get_table global_env ~key:id |> Option.value_exn
        in
        check_model global_env model_table id body
    | Query query -> check_query global_env query
    | Component _ -> ()
    | Page _ -> ()
  in
  List.iter ~f:(fun declaration -> check_declaration declaration) declarations

let run_type_checker (Ast declarations) : unit =
  let global_env = GlobalEnv.create () in
  SymbolTableManager.populate global_env (Ast declarations);
  check_declaration global_env declarations
