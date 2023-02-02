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

  type xra_basic_expression =
    | Literal of literal
    | Variable of id
    | Dot of id * xra_basic_expression

  type xra_conditional_expression =
    | LiteralConditionalExpression of loc * xra_basic_expression
    | NotConditionalExpression of loc * xra_basic_expression
    | EqConditionalExpression of
        loc * xra_basic_expression * xra_basic_expression
    | NotEqConditionalExpression of
        loc * xra_basic_expression * xra_basic_expression
    | LtConditionalExpression of
        loc * xra_basic_expression * xra_basic_expression
    | GtConditionalExpression of
        loc * xra_basic_expression * xra_basic_expression
    | LtOrEqConditionalExpression of
        loc * xra_basic_expression * xra_basic_expression
    | GtOrEqConditionalExpression of
        loc * xra_basic_expression * xra_basic_expression

  type xra_expression =
    | Element of
        loc * id * xra_expression list option * xra_expression list option
    | Attribute of loc * id * xra_expression
    | QueryApplication of loc * id * id list
    | BasicExpression of loc * xra_basic_expression
    | IfElseStatement of
        loc * xra_conditional_expression * xra_expression * xra_expression
    | IfThenStatement of loc * xra_conditional_expression * xra_expression
    | ForLoopStatement of loc * id * xra_basic_expression * xra_expression
    | LetExpression of loc * id * xra_expression

  type body = xra_expression list option * xra_expression list
end

module Component = struct
  type args = (string * string) list option
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

type component_declaration = loc * id * Component.args * XRA.body

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
