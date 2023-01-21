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
%token          LEFT_BRACE
%token          RIGHT_BRACE
%token          LEFT_BRACKET
%token          RIGHT_BRACKET
%token          LEFT_PARAN
%token          RIGHT_PARAN
%token          QUESTION_MARK
%token          COMMA
%token          MODEL
%token          QUERY
%token          ON
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
  | id = ID; field_type = model_field_type; attrs = list(model_field_attr); SEMICOLON
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

query_permissions:
  | PERMISSION; LEFT_PARAN; permissions = separated_nonempty_list(COMMA, ID); RIGHT_PARAN
    { parse_query_permissions $startpos permissions }

query_body:
  | args = query_args; COLON; typ = ID; models = query_models;
    { (parse_query_type $startpos typ, args, models, []) }
  | args = query_args; COLON; typ = ID; models = query_models; permissions = query_permissions
    { (parse_query_type $startpos typ, args, models, permissions) }
  ;

declaration:
  | MODEL; model_id = ID; model_body = model_body
    { Model($startpos, model_id, model_body) }
  | QUERY; query_id = ID; query_body = query_body; SEMICOLON
    { Query($startpos, query_id, query_body) }
  ;

ast:
  | declarations = list(declaration); EOF
    { Ast(declarations) }
  ;
