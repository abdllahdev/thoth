open Core
open Ast_types

let get_model_declaration = function Model model -> Some model | _ -> None
let get_query_declaration = function Query query -> Some query | _ -> None

let get_component_declaration = function
  | Component component -> Some component
  | _ -> None

let get_page_declaration = function Page page -> Some page | _ -> None

let get_filtered_ast (declarations : declaration list) : filtered_ast =
  let model_declarations =
    List.filter_map ~f:get_model_declaration declarations
  in
  let query_declarations =
    List.filter_map ~f:get_query_declaration declarations
  in
  let component_declarations =
    List.filter_map ~f:get_component_declaration declarations
  in
  let page_declarations =
    List.filter_map ~f:get_page_declaration declarations
  in
  {
    model_declarations;
    query_declarations;
    component_declarations;
    page_declarations;
  }
