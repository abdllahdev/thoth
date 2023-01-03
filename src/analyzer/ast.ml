(* Identifier *)
type id = string
type loc = Lexing.position

(* Model definition *)
module Model = struct
  type field_type =
    | FieldTypeString
    | FieldTypeInt
    | FieldTypeJson
    | FieldTypeBoolean
    | FieldTypeFloat
    | FieldTypeDecimal
    | FieldTypeDateTime
    | FieldTypeBigInt
    | FieldTypeBytes
    | FieldTypeCustom of loc * string

  type field_type_modifier = NoModifier | List | Optional

  type field_attr_arg =
    | AttrArgString of loc * string
    | AttrArgFunc of loc * string
    | AttrArgRef of loc * id
    | AttrArgBoolean of loc * bool
    | AttrArgNumber of loc * int

  type field_attr = Attribute of loc * id * field_attr_arg list

  type field =
    | Field of loc * id * field_type * field_type_modifier * field_attr list
end

(* The different types of declarations in the language *)
type declaration = Model of loc * id * Model.field list

(* Ast type *)
type ast = Ast of declaration list
