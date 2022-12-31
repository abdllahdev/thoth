open Ast

let parse_field_type (field_type : string) (startpos : loc) =
  match field_type with
  | "String" -> Model.FieldTypeString
  | "Int" -> Model.FieldTypeInt
  | "Json" -> Model.FieldTypeJson
  | "Boolean" -> Model.FieldTypeBoolean
  | "Float" -> Model.FieldTypeFloat
  | "Decimal" -> Model.FieldTypeDecimal
  | "DateTime" -> Model.FieldTypeDateTime
  | "BigInt" -> Model.FieldTypeBigInt
  | "Bytes" -> Model.FieldTypeBytes
  | _ -> Model.FieldTypeCustom (startpos, field_type)
