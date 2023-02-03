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

type permission = loc * id

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
end

module XRA = struct
  type query_application = loc * id * id list

  type basic_expression =
    | Literal of literal
    | Variable of id
    | Dot of id * basic_expression

  type conditional_expression =
    | LiteralConditionalExpression of loc * basic_expression
    | NotConditionalExpression of loc * basic_expression
    | EqConditionalExpression of loc * basic_expression * basic_expression
    | NotEqConditionalExpression of loc * basic_expression * basic_expression
    | LtConditionalExpression of loc * basic_expression * basic_expression
    | GtConditionalExpression of loc * basic_expression * basic_expression
    | LtOrEqConditionalExpression of loc * basic_expression * basic_expression
    | GtOrEqConditionalExpression of loc * basic_expression * basic_expression

  type expression =
    | Element of loc * id * expression list option * expression list option
    | Attribute of loc * id * expression
    | QueryApplication of loc * id * id list
    | BasicExpression of loc * basic_expression
    | IfElseStatement of loc * conditional_expression * expression * expression
    | IfThenStatement of loc * conditional_expression * expression
    | ForLoopStatement of loc * id * basic_expression * expression
    | LetExpression of loc * id * expression

  type body = expression list option * expression list
end

module Component = struct
  type arg = string * string
end

module Page = struct
  type route = string
end

type declaration_type = ModelType | QueryType | ComponentType | PageType
type model_declaration = loc * id * Model.body

type query_declaration =
  loc
  * id
  * Query.typ
  * Query.arg list
  * Query.model list
  * permission list option

type component_declaration = loc * id * Component.arg list option * XRA.body

type page_declaration =
  loc * id * Page.route * permission list option * XRA.body

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
