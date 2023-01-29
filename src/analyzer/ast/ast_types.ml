(* Identifier *)
type id = string
type loc = Lexing.position

(* TODO: add auth config, app config, and db config *)

type scalar_type =
  | String
  | Int
  | Boolean
  | Bytes
  | Json
  | DateTime
  | CustomType of string

type composite_type =
  | List of scalar_type
  | Optional of scalar_type
  | OptionalList of scalar_type

type typ = Scalar of scalar_type | Composite of composite_type

type literal =
  | StringLiteral of scalar_type * string
  | IntLiteral of scalar_type * int
  | BooleanLiteral of scalar_type * bool

(* TODO: add enums to data models *)
module Model = struct
  type attr_arg =
    | AttrArgString of loc * literal
    | AttrArgBoolean of loc * literal
    | AttrArgInt of loc * literal
    | AttrArgNow of loc
    | AttrArgRef of loc * id

  type attribute = Attribute of loc * id * attr_arg list
  type field = Field of loc * id * typ * attribute list
  type body = field list
end

module Query = struct
  type typ = FindUnique | FindMany | Create | Update | Delete

  type arg =
    | Where of loc * string
    | Filter of loc * string list
    | Data of loc * string list

  type model = loc * id
  type permission = loc * id
  type body = typ * arg list * model list * permission list
end

module Component = struct
  type query_application = loc * id * id list
  type lex_expression = loc * id * query_application
  type jsx_element_attribute = loc * id * string

  type jsx =
    | Element of loc * id * jsx_element_attribute list option * jsx list option
    | Text of loc * string
    | Expression of loc * string

  type route = string option
  type args = string list option
  type body = lex_expression list option * string option
end

type declaration_type = ModelType | QueryType | ComponentType
type model_declaration = loc * id * Model.body
type query_declaration = loc * id * Query.body

type component_declaration =
  loc * id * Component.args * Component.route * Component.body

(* The different types of declarations in the language *)
type declaration =
  | Model of model_declaration
  | Query of query_declaration
  | Component of component_declaration

type filtered_ast = {
  model_declarations : model_declaration list;
  query_declarations : query_declaration list;
  component_declarations : component_declaration list;
}

(* Ast type *)
type ast = Ast of declaration list
