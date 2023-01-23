type id = string
type loc = Lexing.position

type scalar_type =
  | String
  | Int
  | Boolean
  | Bytes
  | Json
  | DateTime
  | CustomType of id

type composite_type =
  | List of scalar_type
  | Optional of scalar_type
  | OptionalList of scalar_type

type typ = Scalar of scalar_type | Composite of composite_type

type literal =
  | StringLiteral of scalar_type * id
  | IntLiteral of scalar_type * int
  | BooleanLiteral of scalar_type * bool

module Model : sig
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

module Query : sig
  type typ = FindUnique | FindAll | Create | Update | Delete

  type arg =
    | Where of loc * id
    | Filter of loc * id list
    | Data of loc * id list

  type model = loc * id
  type permission = loc * id
  type body = typ * arg list * model list * permission list
end

type declaration_type = ModelType | QueryType
type model_declaration = loc * id * Model.body
type query_declaration = loc * id * Query.body
type declaration = Model of model_declaration | Query of query_declaration

type filtered_ast = {
  model_declarations : model_declaration list;
  query_declarations : query_declaration list;
}

type ast = Ast of declaration list
