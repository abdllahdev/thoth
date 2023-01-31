%{
  open Ast.Ast_types
  open Parser_helper
%}

%token <int>    INT
%token <string> ID
%token <string> STRING
%token <string> ATTRIBUTE
%token          TRUE
%token          FALSE
%token          AND
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
%token          COMPONENT
%token          PAGE
%token          LET
%token          RENDER
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

(* Model rules *)
model_field_attr_args:
  | str = STRING
    { Model.AttrArgString($startpos, (StringLiteral(String, str))) }
  | NOW
    { Model.AttrArgNow($startpos) }
  | ref = ID
    { Model.AttrArgRef($startpos, ref) }
  | TRUE
    { Model.AttrArgBoolean($startpos, (BooleanLiteral(Boolean, true))) }
  | FALSE
    { Model.AttrArgBoolean($startpos, (BooleanLiteral(Boolean, false))) }
  | number = INT
    { Model.AttrArgInt($startpos, (IntLiteral(Int, number))) }
  ;

model_field_attr:
  | attr = ATTRIBUTE
    { Model.Attribute($startpos, attr, []) }
  | attr = ATTRIBUTE; LEFT_PARAN; args = separated_list(COMMA, model_field_attr_args); RIGHT_PARAN
    { Model.Attribute($startpos, attr, args) }
  ;

model_field_type:
  | field_type = ID; QUESTION_MARK
    { Composite (Optional (parse_field_type field_type)) }
  | field_type = ID; LEFT_BRACKET; RIGHT_BRACKET
    { Composite (List (parse_field_type field_type)) }
  | field_type = ID; LEFT_BRACKET; RIGHT_BRACKET; QUESTION_MARK
    { Composite (OptionalList (parse_field_type field_type)) }
  | field_type = ID
    { Scalar (parse_field_type field_type) }
  ;

model_field:
  | id = ID; field_type = model_field_type; attrs = list(model_field_attr); option(SEMICOLON)
    { Model.Field($startpos, id, field_type, attrs) }
  ;

model_body:
  | LEFT_BRACE; model_fields = list(model_field); RIGHT_BRACE
    { model_fields }
  ;

(* Query rules *)
(* TODO: change the way querier are parsed` *)
query_arg:
  | arg = ID; COLON; field = ID;
    { parse_query_arg $startpos arg [field] }
  | arg = ID; COLON; LEFT_BRACE; fields = separated_list(COMMA, ID); RIGHT_BRACE
    { parse_query_arg $startpos arg fields }
  ;

query_args:
  | LEFT_PARAN; args = separated_nonempty_list(COMMA, query_arg); RIGHT_PARAN
    { args }
  ;

query_models:
  | ON; LEFT_PARAN; models = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { List.map (fun model -> ($startpos, model)) models }
  ;

query_permissions:
  | PERMISSION; LEFT_PARAN; permissions = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { parse_query_permissions $startpos permissions }

query_body:
  | args = query_args; COLON; typ = ID; models = query_models;
    { (parse_query_type $startpos typ, args, models, []) }
  | args = query_args; COLON; typ = ID; models = query_models; permissions = query_permissions
    { (parse_query_type $startpos typ, args, models, permissions) }
  ;

query_application:
  | id = ID; LEFT_PARAN; args = separated_list(COMMA, ID); RIGHT_PARAN; option(SEMICOLON)
    { ($startpos, id, args) }
  ;

(* Component rules *)
let_expression:
  | LET; id = ID; EQUAL; query_application = query_application;
    { ($startpos, (parse_id $startpos id), query_application) }
  ;

jsx_attribute:
  | id = ID; EQUAL; jsx_expression_declaration = jsx_expression_declaration
    { Component.JSXAttribute($startpos, id, jsx_expression_declaration) }
  | id = ID; EQUAL; str = STRING;
    { Component.JSXAttribute($startpos, id, Component.JSXLiteral($startpos, StringLiteral(String, str))) }
  ;

jsx_opening_element:
  | LT; id = ID; attributes = option(list(jsx_attribute)); GT
    { (id, attributes) }
  ;

jsx_closing_element:
  | CLOSING_TAG; id = ID; GT
    { id }
  ;

jsx_self_closing_element:
  | LT; id = ID; attributes = option(list(jsx_attribute)); SLASH; GT
    { (id, attributes) }
  ;

jsx_element:
  | jsx_opening_element = jsx_opening_element; children = option(list(jsx_children)); closing_id = jsx_closing_element
    { let (opening_id, attributes) = jsx_opening_element in parse_jsx_element $startpos opening_id closing_id attributes children }
  | jsx_self_closing_element = jsx_self_closing_element
    { let (id, attributes) = jsx_self_closing_element in Component.JSXElement ($startpos, id, attributes, None) }
  ;

jsx_children:
  | jsx_element = jsx_element
    { jsx_element }
  | jsx_expression_declaration = jsx_expression_declaration
    { jsx_expression_declaration }
  ;

jsx_expression:
  | str = STRING
    { Component.JSXLiteral($startpos, StringLiteral(String, str)) }
  | number = INT
    { Component.JSXLiteral($startpos, IntLiteral(Int, number)) }
  | TRUE
    { Component.JSXLiteral($startpos, BooleanLiteral(Boolean, true)) }
  | FALSE
    { Component.JSXLiteral($startpos, BooleanLiteral(Boolean, false)) }
  | var = ID
    { Component.JSXVariableExpression($startpos, var) }
  | jsx_element = jsx_element
    { jsx_element }
  | left_expression = jsx_expression; EQ; right_expression = jsx_expression
    { Component.JSXEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = jsx_expression; NOT_EQ; right_expression = jsx_expression
    { Component.JSXNotEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = jsx_expression; LT; right_expression = jsx_expression
    { Component.JSXLtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = jsx_expression; GT; right_expression = jsx_expression
    { Component.JSXGtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = jsx_expression; LT_OR_EQ; right_expression = jsx_expression
    { Component.JSXLtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = jsx_expression; GT_OR_EQ; right_expression = jsx_expression
    { Component.JSXGtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | NOT; jsx_expression = jsx_expression
    { Component.JSXNotConditionalExpression($startpos, jsx_expression) }
  | IF; conditional_expression = jsx_expression; THEN; then_block = jsx_expression; ELSE; else_block = jsx_expression;
    { Component.JSXIfElseStatement($startpos, conditional_expression, then_block, else_block) }
  | IF; conditional_expression = jsx_expression THEN; then_block = jsx_expression;
    { Component.JSXThenStatement($startpos, conditional_expression, then_block) }
  | FOR; var = jsx_expression; IN; lst = jsx_expression; ARROW; output = jsx_expression;
    { Component.JSXLoopStatement ($startpos, var, lst, output) }
  | LEFT_PARAN; jsx_expression = jsx_expression; RIGHT_PARAN
    { jsx_expression }
  ;

jsx_expression_declaration:
  | LEFT_BRACE; jsx_expression = jsx_expression; RIGHT_BRACE
    { jsx_expression }
  ;

render:
  | RENDER; LEFT_PARAN; jsx = list(jsx_element); RIGHT_PARAN
    { jsx }
  ;

page_route:
  | AT; LEFT_PARAN; route = STRING; RIGHT_PARAN
    { route }
  ;

component_body:
  | LEFT_BRACE; let_expressions = option(list(let_expression)); render = render; RIGHT_BRACE
    { (let_expressions, render) }
  ;

component_args:
  | LEFT_PARAN; args = separated_list(COMMA, ID); RIGHT_PARAN
    { args }
  ;

declaration:
  | MODEL; model_id = ID; model_body = model_body
    { Model($startpos, (parse_id $startpos model_id), model_body) }
  | QUERY; query_id = ID; query_body = query_body; option(SEMICOLON)
    { Query($startpos, (parse_id $startpos query_id), query_body) }
  | COMPONENT; component_id = ID; args = option(component_args); component_body = component_body
    { Component($startpos, (parse_id $startpos component_id), args, component_body) }
  | PAGE; component_id = ID; args = option(component_args); route = page_route; component_body = component_body
    { Page($startpos, (parse_id $startpos component_id), args, route, component_body) }
  ;

ast:
  | declarations = list(declaration); EOF
    { Ast(declarations) }
  ;
