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
end

module Component = struct
  type permission = loc * id
  type query_application = loc * id * id list
  type variable_expression = Variable of id | Dot of id * variable_expression
  type let_expression = loc * id * query_application

  type jsx =
    | JSXElement of loc * id * jsx list option * jsx list option
    | JSXAttribute of loc * id * jsx
    | JSXLiteral of loc * literal
    | JSXQueryApplication of loc * id * id list
    | JSXVariableExpression of loc * variable_expression
    | JSXNotConditionalExpression of loc * jsx
    | JSXEqConditionalExpression of loc * jsx * jsx
    | JSXNotEqConditionalExpression of loc * jsx * jsx
    | JSXLtConditionalExpression of loc * jsx * jsx
    | JSXGtConditionalExpression of loc * jsx * jsx
    | JSXLtOrEqConditionalExpression of loc * jsx * jsx
    | JSXGtOrEqConditionalExpression of loc * jsx * jsx
    | JSXIfElseStatement of loc * jsx * jsx * jsx
    | JSXThenStatement of loc * jsx * jsx
    | JSXLoopStatement of loc * jsx * jsx * jsx

  type route = string
  type args = (string * string) list option
  type body = let_expression list option * jsx list
end

type declaration_type = ModelType | QueryType | ComponentType | PageType
type model_declaration = loc * id * Model.body

type query_declaration =
  loc
  * id
  * Query.typ
  * Query.arg list
  * Query.model list
  * Query.permission list option

type component_declaration = loc * id * Component.args * Component.body

type page_declaration =
  loc
  * id
  * Component.args
  * Component.route
  * Component.permission list option
  * Component.body

(* The different types of declarations in the language *)
type declaration =
  | Model of model_declaration
  | Query of query_declaration
  | Component of component_declaration
  | Page of page_declaration

type filtered_ast = {
  model_declarations : model_declaration list;
  query_declarations : query_declaration list;
  component_declarations : component_declaration list;
  page_declarations : page_declaration list;
}

(* Ast type *)
type ast = Ast of declaration list
