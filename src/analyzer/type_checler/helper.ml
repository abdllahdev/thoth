open Ast.Ast_types

let get_custom_scalar_type (scalar_type : scalar_type) : string option =
  match scalar_type with CustomType str -> Some str | _ -> None

let get_custom_type (typ : typ) : string option =
  match typ with
  | Scalar scalar_type -> get_custom_scalar_type scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> get_custom_scalar_type scalar_type
      | Optional scalar_type -> get_custom_scalar_type scalar_type
      | OptionalList scalar_type -> get_custom_scalar_type scalar_type)

let extract_scalar_type (typ : typ) : scalar_type =
  match typ with
  | Scalar scalar_type -> scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> scalar_type
      | Optional scalar_type -> scalar_type
      | OptionalList scalar_type -> scalar_type)
