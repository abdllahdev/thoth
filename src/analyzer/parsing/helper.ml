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
  if List.exists ~f:(fun x -> String.equal x id) keywords then
    raise_reserved_keyword_error loc id
  else id

let parse_declaration_id loc id declaration_type =
  let first_char = String.nget id 0 in
  if Char.is_uppercase first_char then parse_id loc id
  else raise_name_error loc declaration_type

let parse_type ?(list_modifier = false) ?(optional_modifier = false) typ =
  let scalar_type =
    match typ with
    | "String" -> String
    | "Int" -> Int
    | "Boolean" -> Boolean
    | "DateTime" -> DateTime
    | _ -> CustomType typ
  in
  if list_modifier && optional_modifier then
    Composite (OptionalList scalar_type)
  else if list_modifier then Composite (List scalar_type)
  else if optional_modifier then Composite (Optional scalar_type)
  else Scalar scalar_type

let parse_permissions loc permissions =
  let check_permission permission =
    match permission with
    | "isAuth" -> (loc, "isAuth")
    | "owns" -> (loc, "owns")
    | _ -> raise_undefined_error loc "permission" permission
  in
  List.map permissions ~f:check_permission

let parse_xra_element loc opening_id closing_id attributes children =
  if not (String.equal opening_id closing_id) then
    raise_syntax_error loc closing_id
  else XRA.Element (loc, opening_id, attributes, children)
