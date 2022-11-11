(* Identifier *)
type id = string

(* Model definition *)
module Model = struct
  type field_type = string
  type field_type_modifier = List | Optional
  type attr_arg_func = Autoincrement | Now

  type attr_arg_value =
    | AttrArgString of string
    | AttrArgFunc of attr_arg_func
    | AttrArgFieldRefList of string list
    | AttrArgBoolean of bool
    | AttrArgNumber of int

  type field_attr_id = Id | Unique | Ignore | UpdatedAt | Relation | Default
  type field_attr_arg = Argument of attr_arg_value

  type field_attr =
    | AttributeNoArgs of field_attr_id
    | AttributeWithArgs of field_attr_id * field_attr_arg

  type field =
    | FieldNoModiferNoAttrs of id * field_type
    | FieldWithModifierNoAttrs of id * field_type * field_type_modifier
    | FieldNoModiferWithAttrs of id * field_type * field_attr list
    | FieldWithModiferWithAttrs of
        id * field_type * field_type_modifier * field_attr list

  type body = Body of field list
  type definition = Definition of id * body
end

type declaration = Model of Model.definition

(* Program type *)
type program = Program of declaration list
