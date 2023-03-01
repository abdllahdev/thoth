(* Identifier *)
type id = string
type loc = Lexing.position

type scalar_type =
  | String
  | Int
  | Boolean
  | DateTime
  | Reference
  | Nil
  | Assoc
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

module Model = struct
  type unique_field_type = UniqueField | NonUniqueField

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
  type argument_type = WhereArgument | DataArgument | SearchArgument
  type data_args = (string * (string * string) option) list

  type body_arg =
    | Where of loc * id list
    | Search of loc * id list
    | Data of loc * data_args

  type model = loc * id
  type body = body_arg list
end

module XRA = struct
  type query_application = loc * id * id list

  type expression =
    | Literal of literal
    | VariableExpression of loc * id
    | DotExpression of loc * id * id
    | LetExpression of loc * id * expression
    | LiteralConditionalExpression of loc * expression
    | NotConditionalExpression of loc * expression
    | EqConditionalExpression of loc * expression * expression
    | NotEqConditionalExpression of loc * expression * expression
    | LtConditionalExpression of loc * expression * expression
    | GtConditionalExpression of loc * expression * expression
    | LtOrEqConditionalExpression of loc * expression * expression
    | GtOrEqConditionalExpression of loc * expression * expression
    | IfThenElseExpression of loc * expression * expression * expression
    | IfThenExpression of loc * expression * expression
    | ForExpression of loc * id * expression * expression
    | Element of loc * id * expression list option * expression list option
    | Fragment of loc * expression list option
    | Attribute of loc * id * expression

  type body = expression list option * expression list
end

module Component = struct
  type arg = loc * id * typ
  type query_id = id

  type typ =
    | General
    | FindMany of loc * query_id * string
    | FindUnique of loc * query_id * string
    | Create of loc * query_id
    | Update of loc * query_id
    | Delete of loc * query_id

  type form_field_type = TextField | EmailField | PasswordField | NumberField

  type form_field_attrs =
    | FormFieldName of string
    | FormFieldType of form_field_type
    | FormFieldVisibility of literal
    | FormFieldStyle of string
    | FormFieldDefaultValue of string

  type form_field = loc * id * form_field_attrs list
  type form_button = form_field_attrs list

  type body =
    | GeneralBody of XRA.body
    | FindBody of
        XRA.expression list * XRA.expression list * XRA.expression list
    | CreateBody of form_field list * form_button
    | UpdateBody of form_field list * form_button
    | DeleteBody of form_button
end

module Page = struct
  type route = string
end

type model_declaration = loc * id * Model.body

type query_declaration =
  loc
  * id
  * Query.typ
  * typ option
  * Query.body
  * Query.model list
  * permission list option

type component_declaration =
  loc * id * Component.typ * Component.arg list option * Component.body

type page_declaration =
  loc * id * Page.route * permission list option * XRA.body

(* The different types of declarations in the language *)
type declaration_type =
  | ModelDeclaration
  | QueryDeclaration
  | ComponentDeclaration
  | PageDeclaration

type declaration =
  | Model of model_declaration
  | Query of query_declaration
  | Component of component_declaration
  | Page of page_declaration

type obj_field =
  | AssocObjField of (string * obj_field) list
  | ReferenceObjField of string
  | StringObjField of string

type app_config = Title of string | Auth of (string * string) list
type app_declaration = string * app_config list

type filtered_ast = {
  model_declarations : model_declaration list;
  query_declarations : query_declaration list;
  component_declarations : component_declaration list;
  page_declarations : page_declaration list;
}

(* Ast type *)
type ast = app_declaration * declaration list
