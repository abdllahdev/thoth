open Core
open Ast.Ast_types
open Symbol_table
open Model_checker
open Query_checker

let check_declaration (global_table : 'a GlobalSymbolTable.t)
    (declarations : declaration list) : unit =
  let check_declaration declaration =
    match declaration with
    | Model (_, id, body) ->
        let model_table = GlobalSymbolTable.get_table global_table ~key:id in
        check_model global_table model_table id body
    | Query (loc, id, body) -> check_query global_table loc id body
    | Component _ -> ()
    | Page _ -> ()
  in
  List.iter ~f:(fun declaration -> check_declaration declaration) declarations

let run_type_checker (Ast declarations) : unit =
  let global_table = GlobalSymbolTable.create () in
  SymbolTableManager.populate global_table (Ast declarations);
  check_declaration global_table declarations
