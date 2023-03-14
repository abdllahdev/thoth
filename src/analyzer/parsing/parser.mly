%{
  open Ast.Ast_types
  open Helper
%}

%token <int>    INT
%token <string> ID
%token <string> STRING
%token <string> TYPESCRIPT
%token <string> ATTRIBUTE
%token          TRUE
%token          FALSE
%token          NOT
%token          AND
%token          OR
%token          EQ
%token          NOT_EQ
%token          LT_OR_EQ
%token          GT_OR_EQ
%token          EXPRESSION_OPENING
%token          EXPRESSION_CLOSING
%token          IF
%token          ENDIF
%token          ELSE
%token          FOR
%token          ENDFOR
%token          IN
%token          LEFT_BRACE
%token          RIGHT_BRACE
%token          LEFT_PARAN
%token          RIGHT_PARAN
%token          LEFT_BRACKET
%token          RIGHT_BRACKET
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
%token          CONNECT
%token          WITH
%token          COMPONENT
%token          PAGE
%token          APP
%token          LET
%token          RENDER
%token          FIND_MANY
%token          FIND_UNIQUE
%token          CREATE
%token          UPDATE
%token          DELETE
%token          SIGNUP_FORM
%token          LOGIN_FORM
%token          LOGOUT_BUTTON
%token          AS
%token          FRAGMENT_OPENING
%token          FRAGMENT_CLOSING
%token          ON
%token          AT
%token          PERMISSION
%token          COLON
%token          SEMICOLON
%token          EOF

%left NOT
%right AND
%right OR

%start <ast> ast

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

obj_field_value:
  | value = ID
    { ReferenceObjField ($startpos, value) }
  | left = ID; DOT; right = ID
    { DotReferenceObjField($startpos, (left, right)) }
  | TRUE
    { BooleanObjField ($startpos, true) }
  | FALSE
    { BooleanObjField ($startpos, false) }
  | value = INT
    { IntObjField ($startpos, value) }
  | value = STRING
    { StringObjField ($startpos, value) }
  | value = xra_render_expression
    { RenderObjField ($startpos, value) }
  | LEFT_BRACE; value = separated_list(COMMA, obj_field); RIGHT_BRACE
    { AssocObjField ($startpos, value) }
  | LEFT_BRACKET; value = separated_list(COMMA, obj_field_value); RIGHT_BRACKET
    { ListObjField ($startpos, value) }
  | value1 = ID; AS; value2 = ID
    { AsObjField ($startpos, (value1, value2)) }
  | CONNECT; value1 = ID; WITH; value2 = ID
    { ConnectWithObjField ($startpos, (value1, value2)) }
  | CONNECT; value1 = ID; WITH; left = ID; DOT; right = ID
    { ConnectWithObjField ($startpos, (value1, left ^ right)) }
  | value = TYPESCRIPT
    { TsObjField ($startpos, value) }
  ;

obj_field:
  | key = ID; COLON; value = obj_field_value
    { (key, value) }
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

query_body:
  | LEFT_BRACE; body = separated_nonempty_list(COMMA, obj_field); RIGHT_BRACE
    { body }
  ;

query_models:
  | ON; LEFT_PARAN; models = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { List.map (fun model -> ($startpos, model)) models }
  ;

permissions:
  | PERMISSION; LEFT_PARAN; permissions = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { parse_permissions $startpos permissions }

(* xra rules *)
xra_variable_expression:
  | id = ID
    { XRA.VariableExpression($startpos, id) }
  | id = ID; DOT; expanded_id = ID
    { XRA.DotExpression($startpos, id, expanded_id) }
  ;

xra_literal_expression:
  | literal = literal
    { XRA.Literal(literal) }
  | xra_variable_expression = xra_variable_expression
    { xra_variable_expression }
  ;

xra_attribute:
  | id = ID; EQUAL; xra_expression = xra_expression
    { XRA.Attribute($startpos, id, xra_expression) }
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
  | xra_expression = xra_expression
    { xra_expression }
  ;

xra_conditional_expression:
  | xra_literal_expression = xra_literal_expression
    { XRA.LiteralConditionalExpression($startpos, xra_literal_expression) }
  | LEFT_PARAN; xra_conditional_expression = xra_conditional_expression; RIGHT_PARAN
    { xra_conditional_expression }
  | left_expression = xra_literal_expression; EQ; right_expression = xra_literal_expression
    { XRA.EqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_literal_expression; NOT_EQ; right_expression = xra_literal_expression
    { XRA.NotEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_literal_expression; LT; right_expression = xra_literal_expression
    { XRA.LtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_literal_expression; GT; right_expression = xra_literal_expression
    { XRA.GtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_literal_expression; LT_OR_EQ; right_expression = xra_literal_expression
    { XRA.LtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_literal_expression; GT_OR_EQ; right_expression = xra_literal_expression
    { XRA.GtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | NOT; xra_conditional_expression = xra_conditional_expression
    { XRA.NotConditionalExpression($startpos, xra_conditional_expression) }
  | left_expression = xra_conditional_expression; AND; right_expression = xra_conditional_expression
    { XRA.AndConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = xra_conditional_expression; OR; right_expression = xra_conditional_expression
    { XRA.OrConditionalExpression($startpos, left_expression, right_expression) }
  ;

xra_basic_expressions:
  | xra_literal_expression = xra_literal_expression
    { xra_literal_expression }
  | xra_conditional_expression = xra_conditional_expression
    { xra_conditional_expression }
  | LEFT_PARAN; xra_expression = xra_expression; RIGHT_PARAN
    { xra_expression }
  ;

xra_expression:
  | EXPRESSION_OPENING; IF; conditional_expression = xra_conditional_expression; EXPRESSION_CLOSING;
    then_block = xra_expression;
    EXPRESSION_OPENING; ELSE; EXPRESSION_CLOSING;
    else_block = xra_expression;
    EXPRESSION_OPENING; ENDIF; EXPRESSION_CLOSING
    { XRA.IfThenElseExpression($startpos, conditional_expression, then_block, else_block) }
  | EXPRESSION_OPENING; IF; conditional_expression = xra_conditional_expression; EXPRESSION_CLOSING;
    then_block = xra_expression;
    EXPRESSION_OPENING; ENDIF; EXPRESSION_CLOSING
    { XRA.IfThenExpression($startpos, conditional_expression, then_block) }
  | EXPRESSION_OPENING; FOR; var = ID; IN;
    lst = xra_variable_expression; EXPRESSION_CLOSING
    output = xra_expression;
    EXPRESSION_OPENING; ENDFOR; EXPRESSION_CLOSING
    { XRA.ForExpression ($startpos, var, lst, output) }
  | xra_element_or_fragment = xra_element_or_fragment
    { xra_element_or_fragment }
  | LEFT_BRACE; xra_basic_expressions = xra_basic_expressions; RIGHT_BRACE
    { xra_basic_expressions }
  ;

xra_let_expression:
  | LET; id = ID; EQUAL; xra_expression = xra_expression
    { XRA.LetExpression($startpos, (parse_id $startpos id), xra_expression) }
  ;

xra_render_expression:
  | RENDER; LEFT_PARAN; xra = list(xra_expression); RIGHT_PARAN
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

xra_component_type:
  | LT; FIND_MANY; GT;
    { Component.FindMany }
  | LT; FIND_UNIQUE; GT;
    { Component.FindUnique }
  | LT; CREATE; GT;
    { Component.Create }
  | LT; UPDATE; GT;
    { Component.Update }
  | LT; DELETE; GT;
    { Component.Delete }
  | LT; SIGNUP_FORM; GT;
    { Component.SignupForm }
  | LT; LOGIN_FORM; GT;
    { Component.LoginForm }
  | LT; LOGOUT_BUTTON; GT;
    { Component.LogoutButton }
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
    typ = xra_component_type;
    id = ID;
    args = option(xra_component_args);
    LEFT_BRACE;
    body = separated_list(COMMA, obj_field);
    RIGHT_BRACE
    { parse_component $startpos id typ args body }
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
    QUERY; typ = query_type; query_id = ID; option(COLON);
    return_type = option(typ); body = query_body; option(SEMICOLON)
    { let (permissions, models) = query_attributes in
      parse_query $startpos (parse_id $startpos query_id) typ models permissions return_type body }
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

app_declaration:
  | APP; app_id = ID; LEFT_BRACE; app_configs = separated_list(COMMA, obj_field); RIGHT_BRACE
    { ($startpos, app_id, parse_app_configs $startpos app_id app_configs) }
  ;

ast:
  | app_declaration = app_declaration; declarations = list(declaration); EOF
    { (app_declaration, declarations) }
  ;
