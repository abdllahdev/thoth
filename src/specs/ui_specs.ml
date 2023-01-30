open Core
open Ast.Ast_types

type query_application_specs = {
  id : string;
  args : string list;
  query_type : string;
  model : string;
}

type let_specs = { id : string; query_application : query_application_specs }

type component_specs = {
  id : string;
  args : string option;
  let_expressions : let_specs list option;
  jsx : string;
}

type page_specs = {
  id : string;
  args : string option;
  route : string;
  let_expressions : let_specs list option;
  jsx : string;
}

type ui_specs = { pages : page_specs list; components : component_specs list }

let generate_let_specs (lets : Component.lex_expression list option) =
  match lets with
  | Some exprs ->
      Some
        (List.map
           ~f:(fun expr ->
             let _, let_id, query_application = expr in
             let _, query_id, args = query_application in
             let query_type = "findUnique" in
             let model = "task" in
             let query_application =
               { id = query_id; args; query_type; model }
             in
             { id = let_id; query_application })
           exprs)
  | None -> None

let generate_component_specs (component : component_declaration) :
    component_specs =
  let _, id, args, body = component in
  let args =
    match args with
    | Some args -> Some (String.concat ~sep:", " args)
    | None -> None
  in
  let lets, jsx = body in
  let let_expressions = generate_let_specs lets in
  let jsx = match jsx with Some jsx -> jsx | None -> "" in
  { id; args; let_expressions; jsx }

let generate_page_specs (page : page_declaration) : page_specs =
  let _, id, args, route, body = page in
  let args =
    match args with
    | Some args -> Some (String.concat ~sep:", " args)
    | None -> None
  in
  let lets, jsx = body in
  let let_expressions = generate_let_specs lets in
  let jsx = match jsx with Some jsx -> jsx | None -> "" in
  { id; args; route; let_expressions; jsx }

let generate_ui_specs (pages : page_declaration list)
    (components : component_declaration list) : ui_specs =
  let components = List.map ~f:generate_component_specs components in
  let pages = List.map ~f:generate_page_specs pages in
  { pages; components }
