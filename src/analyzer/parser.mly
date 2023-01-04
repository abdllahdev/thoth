%{
  open Ast
  open Parser_helper
%}

%token <int>    INT
%token <string> ID
%token <string> STRING
%token <string> MODEL_FIELD_ATTR
%token          TRUE
%token          FALSE
%token          LEFT_BRACE
%token          RIGHT_BRACE
%token          LEFT_BRACKET
%token          RIGHT_BRACKET
%token          LEFT_PARAN
%token          RIGHT_PARAN
%token          QUESTION_MARK
%token          COMMA
%token          MODEL
%token          SEMICOLON
%token          EOF

%start <ast> ast

%type <Model.field> model_field
%type <declaration> declaration

%%

model_field_attr_args:
  | str = STRING
    { Model.AttrArgString($startpos, (StringLiteral(String, str))) }
  | func = ID; LEFT_PARAN; RIGHT_PARAN
    { Model.AttrArgFunc($startpos, func) }
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
  | attr = MODEL_FIELD_ATTR
    { Model.Attribute($startpos, attr, []) }
  | attr = MODEL_FIELD_ATTR; LEFT_PARAN; args = separated_list(COMMA, model_field_attr_args); RIGHT_PARAN
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
  | id = ID; field_type = model_field_type; attrs = list(model_field_attr); SEMICOLON
    { Model.Field($startpos, id, field_type, attrs) }
  | id = ID; field_type = model_field_type; attrs = list(model_field_attr); SEMICOLON
    { Model.Field($startpos, id, field_type, attrs) }
  ;

model_fields:
  | LEFT_BRACE; model_fields = list(model_field); RIGHT_BRACE
    { model_fields }
  ;

declaration:
  | MODEL; model_id = ID; model_fields = model_fields
    { Model($startpos, model_id, model_fields) }
  ;

ast:
  | declarations = list(declaration); EOF
    { Ast(declarations) }
  ;
