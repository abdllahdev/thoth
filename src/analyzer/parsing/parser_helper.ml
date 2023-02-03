open Core
open Ast.Ast_types
open Error_handler.Handler

let parse_id loc id =
  let keywords =
    [
      "true";
      "false";
      "model";
      "query";
      "component";
      "let";
      "render";
      "now";
      "on";
      "permission";
      "delete";
    ]
  in
  if List.exists ~f:(fun x -> String.equal x (String.lowercase id)) keywords
  then raise_reserved_keyword_error loc id
  else id

let parse_declaration_id loc id declaration_type =
  let first_char = String.nget id 0 in
  if Char.is_uppercase first_char then parse_id loc id
  else raise_name_error loc declaration_type

let parse_field_type field_type =
  match field_type with
  | "String" -> String
  | "Int" -> Int
  | "Boolean" -> Boolean
  | "Bytes" -> Bytes
  | "Json" -> Json
  | "DateTime" -> DateTime
  | _ -> CustomType field_type

let parse_query_arg loc arg fields =
  match arg with
  | "filter" -> Query.Filter (loc, fields)
  | "where" -> Query.Where (loc, List.hd_exn fields)
  | "data" -> Query.Data (loc, fields)
  | _ -> raise_unbound_value_error loc "query argument" arg

let parse_query_type loc typ =
  match typ with
  | "findMany" -> Query.FindMany
  | "findUnique" -> Query.FindUnique
  | "create" -> Query.Create
  | "update" -> Query.Update
  | "delete" -> Query.Delete
  | _ -> raise_unbound_value_error loc "query type" typ

let parse_permissions loc permissions =
  let check_permission permission =
    match permission with
    | "isAuth" -> (loc, "isAuth")
    | "owns" -> (loc, "owns")
    | _ -> raise_unbound_value_error loc "query permission" permission
  in
  List.map ~f:check_permission permissions

let parse_xra_element loc opening_id closing_id attributes children =
  if not (String.equal opening_id closing_id) then
    raise_syntax_error loc closing_id
  else XRA.Element (loc, opening_id, attributes, children)
