(* Identifier *)
type id = string

(* Model definition *)
module Model = struct
  type field_type =
    | String
    | Int
    | Json
    | Boolean
    | Float
    | Decimal
    | DateTime
    | BigInt
    | Bytes
    | Custom of string

  type field_type_modifier = List | Optional

  type field_attr_arg =
    | AttrArgString of string
    | AttrArgFunc of string
    | AttrArgRefList of string list
    | AttrArgBoolean of bool
    | AttrArgNumber of int

  type field_attr =
    | AttributeNoArgs of id
    | AttributeWithArgs of id * field_attr_arg list

  type field =
    | FieldNoModiferNoAttrs of id * field_type
    | FieldWithModifierNoAttrs of id * field_type * field_type_modifier
    | FieldNoModiferWithAttrs of id * field_type * field_attr list
    | FieldWithModiferWithAttrs of
        id * field_type * field_type_modifier * field_attr list

  type fields = Fields of field list
  type declaration = Declaration of id * fields
end

type declaration = Model of Model.declaration

(* Program type *)
type program = Program of declaration list
