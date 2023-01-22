open Core
open Ast_types

let get_model_declaration = function Model model -> Some model | _ -> None
let get_query_declaration = function Query query -> Some query | _ -> None

let get_filtered_ast (declarations : declaration list) : filtered_ast =
  let model_declarations =
    List.filter_map ~f:get_model_declaration declarations
  in
  let query_declarations =
    List.filter_map ~f:get_query_declaration declarations
  in
  { model_declarations; query_declarations }
