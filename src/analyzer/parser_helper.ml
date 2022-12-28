open Ast

let parse_field_type (field_type : string) (startpos : loc) =
  match field_type with
  | "String" -> Model.String startpos
  | "Int" -> Model.Int startpos
  | "Json" -> Model.Json startpos
  | "Boolean" -> Model.Boolean startpos
  | "Float" -> Model.Float startpos
  | "Decimal" -> Model.Decimal startpos
  | "DateTime" -> Model.DateTime startpos
  | "BigInt" -> Model.BigInt startpos
  | "Bytes" -> Model.Bytes startpos
  | _ -> Model.Custom (startpos, field_type)
