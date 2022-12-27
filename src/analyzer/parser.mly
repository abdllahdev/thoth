%{
  open Ast

  let parse_field_type field_type =
    match field_type with
    | "String" -> Model.String
    | "Int" -> Model.Int
    | "Json" -> Model.Json
    | "Boolean" -> Model.Boolean
    | "Float" -> Model.Float
    | "Decimal" -> Model.Decimal
    | "DateTime" -> Model.DateTime
    | "BigInt" -> Model.BigInt
    | "Bytes" -> Model.Bytes
    | _ -> Model.Custom field_type
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

%start <program> program

%type <Model.field> model_field
%type <Model.fields> model_fields
%type <Model.declaration> model_declaration
%type <declaration> declaration

%%

model_field_modifier:
  | QUESTION_MARK
    { Model.Optional }
  | LEFT_BRACKET; RIGHT_BRACKET
    { Model.List }
  ;

model_field_attr_args:
  | str = STRING
    { Model.AttrArgString(str) }
  | func = ID; LEFT_PARAN; RIGHT_PARAN
    { Model.AttrArgFunc(func) }
  | LEFT_BRACKET; refs = separated_nonempty_list(COMMA, ID); RIGHT_BRACKET
    { Model.AttrArgRefList(refs) }
  | TRUE
    { Model.AttrArgBoolean(true) }
  | FALSE
    { Model.AttrArgBoolean(false) }
  | number = INT
    { Model.AttrArgNumber(number) }
  ;

model_field_attr:
  | attr = MODEL_FIELD_ATTR;
    { Model.AttributeNoArgs(attr) }
  | attr = MODEL_FIELD_ATTR; LEFT_PARAN; args = separated_list(COMMA, model_field_attr_args); RIGHT_PARAN
    { Model.AttributeWithArgs(attr, args) }
  ;

model_field_type:
  | field_type = ID
    { parse_field_type field_type }

model_field:
  | id = ID; field_type = model_field_type; SEMICOLON
    { Model.FieldNoModiferNoAttrs(id, field_type) }
  | id = ID; field_type = model_field_type; modifier = model_field_modifier; SEMICOLON
    { Model.FieldWithModifierNoAttrs(id, field_type, modifier) }
  | id = ID; field_type = model_field_type; attrs = list(model_field_attr); SEMICOLON
    { Model.FieldNoModiferWithAttrs(id, field_type, attrs) }
  | id = ID; field_type = model_field_type; modifier = model_field_modifier; attrs = list(model_field_attr); SEMICOLON
    { Model.FieldWithModiferWithAttrs(id, field_type, modifier, attrs) }
  ;

model_fields:
  | LEFT_BRACE; model_fields = list(model_field); RIGHT_BRACE
    { Model.Fields(model_fields) }
  ;

model_declaration:
  | MODEL; model_id = ID; model_fields = model_fields
    { Model.Declaration(model_id, model_fields) }
  ;

declaration:
  | model_declaration = model_declaration
    { Model(model_declaration) }
  ;

program:
  | declarations = list(declaration); EOF
    { Program(declarations) }
  ;
