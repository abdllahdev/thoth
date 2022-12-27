(* Identifier *)
type id = string
type loc = Lexing.position

(* Model definition *)
module Model = struct
  type field_type =
    | String of loc
    | Int of loc
    | Json of loc
    | Boolean of loc
    | Float of loc
    | Decimal of loc
    | DateTime of loc
    | BigInt of loc
    | Bytes of loc
    | Custom of loc * string

  type field_type_modifier = List | Optional

  type field_attr_arg =
    | AttrArgString of loc * string
    | AttrArgFunc of loc * string
    | AttrArgRefList of loc * string list
    | AttrArgBoolean of loc * bool
    | AttrArgNumber of loc * int

  type field_attr =
    | AttributeNoArgs of loc * id
    | AttributeWithArgs of loc * id * field_attr_arg list

  type field =
    | FieldNoModiferNoAttrs of loc * id * field_type
    | FieldWithModifierNoAttrs of loc * id * field_type * field_type_modifier
    | FieldNoModiferWithAttrs of loc * id * field_type * field_attr list
    | FieldWithModiferWithAttrs of
        loc * id * field_type * field_type_modifier * field_attr list
end

type declaration = Model of loc * id * Model.field list

(* Program type *)
type program = Program of declaration list
