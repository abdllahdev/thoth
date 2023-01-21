open Ast.Ast_types
open Symbol_table
open Model_checker
open Query_checker

let check_declaration (global_table : 'a GlobalSymbolTable.t)
    (declaration : declaration) : unit =
  match declaration with
  | Model (_, id, body) ->
      let model_table =
        Option.get (GlobalSymbolTable.get_table global_table id)
      in
      check_model global_table model_table body
  | Query (loc, _, body) -> check_query_body global_table loc body

let rec semantic_check (global_table : 'a GlobalSymbolTable.t)
    (Ast declarations) : unit =
  match declarations with
  | [] -> ()
  | declaration :: declarations ->
      check_declaration global_table declaration;
      semantic_check global_table (Ast declarations)

let run_type_checker (Ast declarations) : unit =
  let global_table = GlobalSymbolTable.create () in
  SymbolTableManager.populate global_table (Ast declarations);
  semantic_check global_table (Ast declarations)
