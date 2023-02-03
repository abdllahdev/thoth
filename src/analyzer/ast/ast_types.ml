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
  | Reference
  | CustomType of string

type composite_type =
  | List of scalar_type
  | Optional of scalar_type
  | OptionalList of scalar_type

type typ = Scalar of scalar_type | Composite of composite_type

type literal =
  | StringLiteral of loc * string
  | IntLiteral of loc * int
  | BooleanLiteral of loc * bool

type permission = loc * id

(* TODO: add enums to data models *)
module Model = struct
  type attr_arg =
    | AttrArgLiteral of literal
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
  type dot_expression = Dot of loc * id * dot_expression option

  type expression =
    | Literal of literal
    | Variable of loc * id
    | DotExpression of dot_expression
    | LetExpression of loc * id * expression
    | LiteralConditionalExpression of loc * expression
    | NotConditionalExpression of loc * expression
    | EqConditionalExpression of loc * expression * expression
    | NotEqConditionalExpression of loc * expression * expression
    | LtConditionalExpression of loc * expression * expression
    | GtConditionalExpression of loc * expression * expression
    | LtOrEqConditionalExpression of loc * expression * expression
    | GtOrEqConditionalExpression of loc * expression * expression
    | IfThenElseStatement of loc * expression * expression * expression
    | IfThenStatement of loc * expression * expression
    | ForLoopStatement of loc * id * expression * expression
    | Element of loc * id * expression list option * expression list option
    | Fragment of loc * expression list option
    | Attribute of loc * id * expression

  type body = expression list option * expression list
end

module Component = struct
  type arg = string * string
  type query_id = id

  type component_type =
    | General
    | Fetch of loc * query_id * string
    | Create of loc * query_id
    | Update of loc * query_id
    | Delete of loc * query_id

  type component_body =
    | GeneralBody of XRA.body
    | FetchBody of
        XRA.expression list * XRA.expression list * XRA.expression list
    | CreateBody of (string * XRA.expression) list * XRA.expression
    | UpdateBody of (string * XRA.expression) list * XRA.expression
    | DeleteBody of XRA.expression
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

type component_declaration =
  loc
  * id
  * Component.component_type
  * Component.arg list option
  * Component.component_body

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
