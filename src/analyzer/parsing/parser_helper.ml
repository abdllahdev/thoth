open Core
open Ast
open Ast.Ast_types
open Error_handler.Handler

let parse_id (loc : loc) (id : id) : id =
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
  if List.exists ~f:(fun x -> String.equal x id) keywords then
    raise_reserved_keyword_error id (Pprinter.string_of_loc loc)
  else id

let parse_field_type (field_type : string) : scalar_type =
  match field_type with
  | "String" -> String
  | "Int" -> Int
  | "Boolean" -> Boolean
  | "Bytes" -> Bytes
  | "Json" -> Json
  | "DateTime" -> DateTime
  | _ -> CustomType field_type

let parse_query_arg (loc : loc) (arg : string) (fields : string list) :
    Query.arg =
  match arg with
  | "filter" -> Query.Filter (loc, fields)
  | "where" -> Query.Where (loc, List.hd_exn fields)
  | "data" -> Query.Data (loc, fields)
  | _ -> raise_name_error (Pprinter.string_of_loc loc) "query argument" arg

let parse_query_type (loc : loc) (typ : string) : Query.typ =
  match typ with
  | "findMany" -> Query.FindMany
  | "findUnique" -> Query.FindUnique
  | "create" -> Query.Create
  | "update" -> Query.Update
  | "delete" -> Query.Delete
  | _ -> raise_name_error (Pprinter.string_of_loc loc) "query type" typ

let parse_query_permissions (loc : loc) (permissions : string list) :
    Query.permission list =
  let check_permission permission =
    match permission with
    | "isAuth" -> (loc, "isAuth")
    | "owns" -> (loc, "owns")
    | _ ->
        raise_name_error
          (Pprinter.string_of_loc loc)
          "query permission" permission
  in
  List.map ~f:check_permission permissions

let parse_jsx_element (loc : loc) (opening_id : id) (closing_id : id)
    (attributes : Component.jsx_element_attribute list option)
    (children : Component.jsx list option) : Component.jsx =
  if not (String.equal opening_id closing_id) then
    raise_syntax_error (Pprinter.string_of_loc loc) closing_id
  else Component.Element (loc, opening_id, attributes, children)
