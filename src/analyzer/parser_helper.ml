open Ast

let parse_field_type (field_type : string) (startpos : loc) =
  match field_type with
  | "String" -> Model.FieldTypeString startpos
  | "Int" -> Model.FieldTypeInt startpos
  | "Json" -> Model.FieldTypeJson startpos
  | "Boolean" -> Model.FieldTypeBoolean startpos
  | "Float" -> Model.FieldTypeFloat startpos
  | "Decimal" -> Model.FieldTypeDecimal startpos
  | "DateTime" -> Model.FieldTypeDateTime startpos
  | "BigInt" -> Model.FieldTypeBigInt startpos
  | "Bytes" -> Model.FieldTypeBytes startpos
  | _ -> Model.FieldTypeCustom (startpos, field_type)
