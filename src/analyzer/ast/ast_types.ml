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
    | AndConditionalExpression of loc * expression * expression
    | OrConditionalExpression of loc * expression * expression
    | IfThenElseExpression of loc * expression * expression * expression
    | IfThenExpression of loc * expression * expression
    | ForExpression of loc * id * expression * expression
    | Element of loc * id * expression list option * expression list option
    | Fragment of loc * expression list option
    | Attribute of loc * id * expression

  type body = expression list option * expression list
end

type obj_field =
  | AssocObjField of loc * (string * obj_field) list
  | ReferenceObjField of loc * string
  | DotReferenceObjField of loc * (string * string)
  | StringObjField of loc * string
  | BooleanObjField of loc * bool
  | IntObjField of loc * int
  | RenderObjField of loc * XRA.expression list
  | ListObjField of loc * obj_field list
  | AsObjField of loc * (string * string)

module Component = struct
  type arg = loc * id * typ
  type query_id = loc * id
  type variable = loc * id

  type typ =
    | General
    | FindMany
    | FindUnique
    | Create
    | Update
    | Delete
    | SignupForm
    | LoginForm
    | LogoutButton

  type form_field_type =
    | TextInput
    | EmailInput
    | PasswordInput
    | NumberInput
    | DefaultInput

  type form_attr =
    | FormAttrName of string
    | FormAttrType of form_field_type
    | FormAttrVisibility of bool
    | FormAttrStyle of string
    | FormAttrDefaultValue of obj_field
    | FormAttrPlaceholder of string

  type style = form_attr
  type form_input = form_attr list
  type form_input_label = form_attr list

  type form_element =
    loc * id * style option * form_input_label option * form_input

  type form_button = form_attr list

  type body =
    | GeneralBody of XRA.body
    | FindBody of
        (query_id * variable)
        * XRA.expression list
        * XRA.expression list
        * XRA.expression list
    | CreateBody of query_id * style option * form_element list * form_button
    | UpdateBody of query_id * style option * form_element list * form_button
    | DeleteBody of query_id * form_button
    | SignupFormBody of style option * form_element list * form_button
    | LoginFormBody of style option * form_element list * form_button
    | LogoutButtonBody of form_button
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
  | AppDeclaration

type declaration =
  | Model of model_declaration
  | Query of query_declaration
  | Component of component_declaration
  | Page of page_declaration

type app_config =
  | Title of string
  | NotFound of string
  | Auth of (string * string) list

type auth_config = {
  user_model : string;
  id_field : string;
  username_field : string;
  password_field : string;
  on_success_redirect_to : string;
  on_fail_redirect_to : string;
}

type app_declaration = loc * string * app_config list

type filtered_ast = {
  model_declarations : model_declaration list;
  query_declarations : query_declaration list;
  component_declarations : component_declaration list;
  page_declarations : page_declaration list;
}

(* Ast type *)
type ast = app_declaration * declaration list
