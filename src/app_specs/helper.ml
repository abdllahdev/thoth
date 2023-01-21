open Ast.Ast_types

let generate_literals (literal : literal) : string =
  match literal with
  | StringLiteral (_, str) -> str
  | IntLiteral (_, num) -> Fmt.str "%d" num
  | BooleanLiteral (_, boolean) -> Fmt.str "%b" boolean

let generate_scalar_type (scalar_type : scalar_type) : string =
  match scalar_type with
  | String -> "String"
  | Int -> "Int"
  | Json -> "Json"
  | Boolean -> "Boolean"
  | DateTime -> "DateTime"
  | Bytes -> "Bytes"
  | CustomType custom_type -> custom_type

let generate_composite_type (composite_type : composite_type) : string =
  match composite_type with
  | List scalar_type -> Fmt.str "%s[]" (generate_scalar_type scalar_type)
  | Optional scalar_type -> Fmt.str "%s?" (generate_scalar_type scalar_type)
  | OptionalList scalar_type ->
      Fmt.str "%s[]?" (generate_scalar_type scalar_type)
