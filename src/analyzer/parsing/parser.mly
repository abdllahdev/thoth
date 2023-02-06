%{
  open Ast.Ast_types
  open Helper
%}

%token <int>    INT
%token <string> ID
%token <string> STRING
%token <string> ATTRIBUTE
%token          TRUE
%token          FALSE
%token          ARROW
%token          NOT
%token          EQ
%token          NOT_EQ
%token          LT_OR_EQ
%token          GT_OR_EQ
%token          IF
%token          THEN
%token          ELSE
%token          FOR
%token          IN
%token          LEFT_BRACE
%token          RIGHT_BRACE
%token          LEFT_BRACKET
%token          RIGHT_BRACKET
%token          LEFT_PARAN
%token          RIGHT_PARAN
%token          LIST_MODIFIER
%token          LT
%token          GT
%token          SLASH
%token          CLOSING_TAG
%token          QUESTION_MARK
%token          EQUAL
%token          COMMA
%token          DOT
%token          NOW
%token          MODEL
%token          QUERY
%token          WHERE
%token          DATA
%token          FILTER
%token          COMPONENT
%token          PAGE
%token          LET
%token          RENDER
%token          FIND_MANY
%token          FIND_UNIQUE
%token          CREATE
%token          UPDATE
%token          DELETE
%token          FETCH_MANY
%token          FETCH_ONE
%token          ON_ERROR
%token          ON_LOADING
%token          ON_SUCCESS
%token          FORM_FIELDS
%token          FORM_BUTTON
%token          AS
%token          FRAGMENT_OPENING
%token          FRAGMENT_CLOSING
%token          ON
%token          AT
%token          PERMISSION
%token          COLON
%token          SEMICOLON
%token          EOF

%start <ast> ast

%type <Model.field> model_field
%type <declaration> declaration

%%

boolean:
  | TRUE
    { BooleanLiteral($startpos, true) }
  | FALSE
    { BooleanLiteral($startpos, false) }
  ;

number:
  | number = INT
    { IntLiteral($startpos, number) }
  ;

str:
  | str = STRING
    { StringLiteral($startpos, str) }
  ;

literal:
  | boolean = boolean
    { boolean }
  | number = number
    { number }
  | str = str
    { str }
  ;

(* Model rules *)
model_field_attr_args:
  | literal = literal
   { Model.AttrArgLiteral(literal) }
  | NOW
    { Model.AttrArgNow($startpos) }
  | ref = ID
    { Model.AttrArgRef($startpos, ref) }
  ;

model_field_attr:
  | attr = ATTRIBUTE
    { Model.Attribute($startpos, attr, []) }
  | attr = ATTRIBUTE; LEFT_PARAN; args = separated_list(COMMA, model_field_attr_args); RIGHT_PARAN
    { Model.Attribute($startpos, attr, args) }
  ;

typ:
  | typ = ID; QUESTION_MARK
    { parse_type typ ~optional_modifier:true }
  | typ = ID; LIST_MODIFIER
    { parse_type typ ~list_modifier:true }
  | typ = ID; LIST_MODIFIER; QUESTION_MARK
    { parse_type typ ~list_modifier:true ~optional_modifier:true }
  | typ = ID
    { parse_type typ }
  ;

model_field:
  | id = ID; typ = typ; attrs = list(model_field_attr); option(SEMICOLON)
    { Model.Field($startpos, id, typ, attrs) }
  ;

model_body:
  | LEFT_BRACE; model_fields = list(model_field); RIGHT_BRACE
    { model_fields }
  ;

(* Query rules *)
query_attributes:
  | permissions = option(permissions); models = query_models
    { (permissions, models) }
  | models = query_models; permissions = option(permissions)
    { (permissions, models) }
  ;

query_type:
  | LT; FIND_MANY; GT
    { Query.FindMany }
  | LT; FIND_UNIQUE; GT
    { Query.FindUnique }
  | LT; CREATE; GT
    { Query.Create }
  | LT; UPDATE; GT
    { Query.Update }
  | LT; DELETE; GT
    { Query.Delete }
  ;

query_arg:
  | WHERE; COLON; field = ID
    { Query.Where($startpos, field) }
  | DATA; COLON; LEFT_BRACE; fields = separated_list(COMMA, ID); RIGHT_BRACE
    { Query.Data($startpos, fields) }
  | FILTER; COLON; LEFT_BRACE; fields = separated_list(COMMA, ID); RIGHT_BRACE
    { Query.Filter($startpos, fields) }
  ;

query_args:
  | LEFT_PARAN; args = separated_nonempty_list(COMMA, query_arg); RIGHT_PARAN
    { args }
  ;

query_models:
  | ON; LEFT_PARAN; models = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { List.map (fun model -> ($startpos, model)) models }
  ;

permissions:
  | PERMISSION; LEFT_PARAN; permissions = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { parse_permissions $startpos permissions }

(* xra rules *)
(* TODO: solve this problem, this rule never reduces to ID *)
xra_variable:
  | dot = xra_variable; DOT; id = ID
    { XRA.Dot($startpos, id, dot) }
  | id = ID;
    { XRA.Variable($startpos, id) }
  ;

xra_variable_expression:
  | xra_variable = xra_variable
    { XRA.VariableExpression(xra_variable) }

xra_basic_expression:
  | literal = literal
    { XRA.Literal(literal) }
  | xra_variable_expression = xra_variable_expression
    { xra_variable_expression }
  ;

xra_attribute:
  | id = ID; EQUAL; xra_expression_declaration = xra_expression_declaration
    { XRA.Attribute($startpos, id, xra_expression_declaration) }
  | id = ID; EQUAL; str = STRING;
    { XRA.Attribute($startpos, id, XRA.Literal(StringLiteral($startpos, str))) }
  ;

xra_opening_element:
  | LT; id = ID; attributes = option(list(xra_attribute)); GT
    { (id, attributes) }
  ;

xra_closing_element:
  | CLOSING_TAG; id = ID; GT
    { id }
  ;

xra_self_closing_element:
  | LT; id = ID; attributes = option(list(xra_attribute)); SLASH; GT
    { (id, attributes) }
  ;

xra_element:
  | xra_opening_element = xra_opening_element;
    children = option(list(xra_children));
    closing_id = xra_closing_element
    { let (opening_id, attributes) = xra_opening_element in
      parse_xra_element $startpos opening_id closing_id attributes children }
  | xra_self_closing_element = xra_self_closing_element
    { let (id, attributes) = xra_self_closing_element
      in XRA.Element ($startpos, id, attributes, None) }
  ;

xra_element_or_fragment:
  | xra_opening_element = xra_opening_element;
    children = option(list(xra_children));
    closing_id = xra_closing_element
    { let (opening_id, attributes) = xra_opening_element in
      parse_xra_element $startpos opening_id closing_id attributes children }
  | xra_self_closing_element = xra_self_closing_element
    { let (id, attributes) = xra_self_closing_element
      in XRA.Element ($startpos, id, attributes, None) }
  | FRAGMENT_OPENING; children = option(list(xra_children)); FRAGMENT_CLOSING
    { XRA.Fragment($startpos, children) }
  ;

xra_children:
  | xra_element = xra_element
    { xra_element }
  | xra_expression_declaration = xra_expression_declaration
    { xra_expression_declaration }
  ;

xra_conditional_expression:
  | left_expression = xra_basic_expression; EQ; right_expression = xra_basic_expression
    { XRA.EqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_basic_expression; NOT_EQ; right_expression = xra_basic_expression
    { XRA.NotEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_basic_expression; LT; right_expression = xra_basic_expression
    { XRA.LtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_basic_expression; GT; right_expression = xra_basic_expression
    { XRA.GtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_basic_expression; LT_OR_EQ; right_expression = xra_basic_expression
    { XRA.LtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_basic_expression; GT_OR_EQ; right_expression = xra_basic_expression
    { XRA.GtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | NOT; xra_variable_expression = xra_variable_expression
    { XRA.NotConditionalExpression($startpos, xra_variable_expression) }
  | NOT; boolean = boolean
    { XRA.NotConditionalExpression($startpos, XRA.Literal(boolean)) }
  | xra_variable_expression = xra_variable_expression
    { XRA.LiteralConditionalExpression($startpos, xra_variable_expression) }
  | boolean = boolean
    { XRA.LiteralConditionalExpression($startpos, XRA.Literal(boolean)) }
  ;

xra_expression:
  | xra_basic_expression = xra_basic_expression
    { xra_basic_expression }
  | xra_element_or_fragment = xra_element_or_fragment
    { xra_element_or_fragment }
  | IF; conditional_expression = xra_conditional_expression;
    THEN; then_block = xra_expression;
    ELSE; else_block = xra_expression;
    { XRA.IfThenElseStatement($startpos, conditional_expression, then_block, else_block) }
  | IF; conditional_expression = xra_conditional_expression
    THEN; then_block = xra_expression;
    { XRA.IfThenStatement($startpos, conditional_expression, then_block) }
  | FOR; var = ID; IN; lst = xra_variable_expression; ARROW; output = xra_expression;
    { XRA.ForLoopStatement ($startpos, var, lst, output) }
  | LEFT_PARAN; xra_expression = xra_expression; RIGHT_PARAN
    { xra_expression }
  ;

xra_expression_declaration:
  | LEFT_BRACE; xra_expression = xra_expression; RIGHT_BRACE
    { xra_expression }
  ;

xra_let_expression:
  | LET; id = ID; EQUAL; xra_expression = xra_expression
    { XRA.LetExpression($startpos, (parse_id $startpos id), xra_expression) }
  ;

xra_render_expression:
  | RENDER; LEFT_PARAN; xra = list(xra_element_or_fragment); RIGHT_PARAN
    { xra }
  ;

xra_general_body:
  | LEFT_BRACE;
    xra_let_expression = option(list(xra_let_expression));
    xra_render_expression = xra_render_expression;
    RIGHT_BRACE
    { (xra_let_expression, xra_render_expression) }
  ;

xra_component_arg:
  | arg = ID; COLON; typ = typ
    { ($startpos, parse_id $startpos arg, typ) }
  ;

xra_component_args:
  | LEFT_PARAN; args = separated_list(COMMA, xra_component_arg); RIGHT_PARAN
    { args }
  ;

xra_fetch_component_declarations:
  | ON_ERROR; ARROW; xra_render_expression = xra_render_expression
    { (xra_render_expression) }
  | ON_LOADING; ARROW; xra_render_expression = xra_render_expression
    { (xra_render_expression) }
  | ON_SUCCESS; ARROW; xra_render_expression = xra_render_expression
    { (xra_render_expression) }
  ;

xra_action_component_form_field:
  | id = ID; COLON; xra_element = xra_element
    { ($startpos, id, xra_element) }
  ;

xra_action_component_form_fields:
  | FORM_FIELDS;
    ARROW;
    LEFT_BRACE;
    fields = separated_nonempty_list(COMMA, xra_action_component_form_field);
    RIGHT_BRACE
    { fields }
  ;

xra_action_component_submit_button:
  | FORM_BUTTON; ARROW; xra_element = xra_element
    { xra_element }
  ;

xra_component:
  | COMPONENT;
    component_id = ID;
    args = option(xra_component_args);
    xra_general_body = xra_general_body
    { Component(
        $startpos,
        component_id,
        Component.General,
        args,
        Component.GeneralBody(xra_general_body)) }
  | COMPONENT;
    LT; FETCH_MANY; COLON; query_id = ID; AS; variable = ID; GT;
    component_id = ID;
    LEFT_BRACE;
    on_error = xra_fetch_component_declarations;
    on_loading = xra_fetch_component_declarations;
    on_success = xra_fetch_component_declarations;
    RIGHT_BRACE
    { Component(
        $startpos,
        component_id,
        Component.FetchMany($startpos, query_id, variable),
        None,
        Component.FetchBody(on_error, on_loading, on_success)) }
  | COMPONENT;
    LT; FETCH_ONE; COLON; query_id = ID; AS; variable = ID; GT;
    component_id = ID;
    LEFT_BRACE;
    on_error = xra_fetch_component_declarations;
    on_loading = xra_fetch_component_declarations;
    on_success = xra_fetch_component_declarations;
    RIGHT_BRACE
    { Component(
        $startpos,
        component_id,
        Component.FetchOne($startpos, query_id, variable),
        None,
        Component.FetchBody(on_error, on_loading, on_success)) }
  | COMPONENT;
    LT; CREATE; COLON; query_id = ID; GT;
    component_id = ID;
    LEFT_BRACE;
    fromFields = xra_action_component_form_fields;
    submitButton = xra_action_component_submit_button;
    RIGHT_BRACE
    { Component(
        $startpos,
        component_id,
        Component.Create($startpos, query_id),
        None,
        Component.CreateBody(fromFields, submitButton)) }
  | COMPONENT;
    LT; UPDATE; COLON; query_id = ID; GT;
    component_id = ID;
    LEFT_BRACE;
    fromFields = xra_action_component_form_fields;
    submitButton = xra_action_component_submit_button;
    RIGHT_BRACE
    { Component(
        $startpos,
        component_id,
        Component.Update($startpos, query_id),
        None,
        Component.UpdateBody(fromFields, submitButton)) }
  | COMPONENT;
    LT; DELETE; COLON; query_id = ID; GT;
    component_id = ID;
    LEFT_BRACE;
    submitButton = xra_action_component_submit_button;
    RIGHT_BRACE
    { Component(
        $startpos,
        component_id,
        Component.Delete($startpos, query_id),
        None,
        Component.DeleteBody(submitButton)) }
  ;

xra_page_route:
  | AT; LEFT_PARAN; route = STRING; RIGHT_PARAN
    { route }
  ;

xra_page_attributes:
  | route = xra_page_route; permissions = option(permissions)
    { (permissions, route) }
  | permissions = option(permissions); route = xra_page_route
    { (permissions, route) }
  ;

declaration:
  | MODEL; model_id = ID; model_body = model_body
    { Model($startpos, (parse_declaration_id $startpos model_id ModelDeclaration), model_body) }
  | query_attributes = query_attributes;
    QUERY; typ = query_type; query_id = ID; args = query_args;
    option(COLON); return_type = option(typ); option(SEMICOLON)
    { let (permissions, models) = query_attributes in
      Query($startpos, (parse_id $startpos query_id), typ, return_type, args, models, permissions) }
  | xra_component = xra_component
    { xra_component }
  | xra_page_attributes = xra_page_attributes; PAGE; page_id = ID; xra_general_body = xra_general_body
    { let (permissions, route) = xra_page_attributes in
      Page(
        $startpos,
        (parse_declaration_id $startpos page_id PageDeclaration),
        route,
        permissions,
        xra_general_body) }
  ;

ast:
  | declarations = list(declaration); EOF
    { Ast(declarations) }
  ;
