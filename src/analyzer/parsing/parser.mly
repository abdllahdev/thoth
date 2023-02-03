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

permissions:
  | PERMISSION; LEFT_PARAN; permissions = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { parse_permissions $startpos permissions }

query_application:
  | id = ID; LEFT_PARAN; args = separated_list(COMMA, ID); RIGHT_PARAN; option(SEMICOLON)
    { ($startpos, id, args) }
  ;

(* Component rules *)
variable_expression:
  | id = ID; DOT; dot = variable_expression
    { XRA.Dot($startpos, id, dot) }
  | id = ID
    { XRA.Variable($startpos, id) }
  ;

basic_expression:
  | literal = literal
    { XRA.Literal(literal) }
  | variable_expression = variable_expression
    { variable_expression }

xra_attribute:
  | id = ID; EQUAL; xra_expression_declaration = xra_expression_declaration
    { XRA.Attribute($startpos, id, xra_expression_declaration) }
  | id = ID; EQUAL; str = STRING;
    { XRA.Attribute($startpos, id, XRA.BasicExpression(XRA.Literal(StringLiteral($startpos, str)))) }
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
  | xra_opening_element = xra_opening_element; children = option(list(xra_children)); closing_id = xra_closing_element
    { let (opening_id, attributes) = xra_opening_element in parse_xra_element $startpos opening_id closing_id attributes children }
  | xra_self_closing_element = xra_self_closing_element
    { let (id, attributes) = xra_self_closing_element in XRA.Element ($startpos, id, attributes, None) }
  ;

xra_children:
  | xra_element = xra_element
    { xra_element }
  | xra_expression_declaration = xra_expression_declaration
    { xra_expression_declaration }
  ;

xra_conditional_expression:
  | left_expression = basic_expression; EQ; right_expression = basic_expression
    { XRA.EqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = basic_expression; NOT_EQ; right_expression = basic_expression
    { XRA.NotEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = basic_expression; LT; right_expression = basic_expression
    { XRA.LtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = basic_expression; GT; right_expression = basic_expression
    { XRA.GtConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = basic_expression; LT_OR_EQ; right_expression = basic_expression
    { XRA.LtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | left_expression = basic_expression; GT_OR_EQ; right_expression = basic_expression
    { XRA.GtOrEqConditionalExpression($startpos, left_expression, right_expression) }
  | NOT; basic_expression = basic_expression
    { XRA.NotConditionalExpression($startpos, basic_expression) }
  | basic_expression = basic_expression
    { XRA.LiteralConditionalExpression($startpos, basic_expression) }
  ;

xra_expression:
  | basic_expression = basic_expression
    { XRA.BasicExpression(basic_expression) }
  | query_application =  query_application
    { let (loc, id, args) = query_application in XRA.QueryApplication(loc, id, args) }
  | xra_element = xra_element
    { xra_element }
  | IF; conditional_expression = xra_conditional_expression; THEN; then_block = xra_expression; ELSE; else_block = xra_expression;
    { XRA.IfThenElseStatement($startpos, conditional_expression, then_block, else_block) }
  | IF; conditional_expression = xra_conditional_expression THEN; then_block = xra_expression;
    { XRA.IfThenStatement($startpos, conditional_expression, then_block) }
  | FOR; var = ID; IN; lst = variable_expression; ARROW; output = xra_expression;
    { XRA.ForLoopStatement ($startpos, var, XRA.BasicExpression(lst), output) }
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
  | RENDER; LEFT_PARAN; xra = list(xra_element); RIGHT_PARAN
    { xra }
  ;

page_route:
  | AT; LEFT_PARAN; route = STRING; RIGHT_PARAN
    { route }
  ;

xra_body:
  | LEFT_BRACE; xra_let_expression = option(list(xra_let_expression)) xra_render_expression = xra_render_expression; RIGHT_BRACE
    { (xra_let_expression, xra_render_expression) }
  ;

component_arg:
  | arg = ID; COLON; typ = ID
    { (parse_id $startpos arg, typ) }

component_args:
  | LEFT_PARAN; args = separated_list(COMMA, component_arg); RIGHT_PARAN
    { args }
  ;

declaration:
  | MODEL; model_id = ID; model_body = model_body
    { Model($startpos, (parse_declaration_id $startpos model_id "Model"), model_body) }
  | models = query_models; permissions = option(permissions); QUERY; LT; typ = ID; GT; query_id = ID; args = query_args; option(SEMICOLON)
    { Query($startpos, (parse_id $startpos query_id), parse_query_type $startpos typ, args, models, permissions) }
  | COMPONENT; component_id = ID; args = option(component_args); xra_body = xra_body
    { Component($startpos, (parse_declaration_id $startpos component_id "Component"), args, xra_body) }
  | route = page_route; permissions = option(permissions); PAGE; component_id = ID; xra_body = xra_body
    { Page($startpos, (parse_declaration_id $startpos component_id "Page"), route, permissions, xra_body) }
  ;

ast:
  | declarations = list(declaration); EOF
    { Ast(declarations) }
  ;
