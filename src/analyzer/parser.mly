%{
  open Ast

  let parse_field_type field_type startpos =
    match field_type with
    | "String" -> Model.String(startpos)
    | "Int" -> Model.Int(startpos)
    | "Json" -> Model.Json(startpos)
    | "Boolean" -> Model.Boolean(startpos)
    | "Float" -> Model.Float(startpos)
    | "Decimal" -> Model.Decimal(startpos)
    | "DateTime" -> Model.DateTime(startpos)
    | "BigInt" -> Model.BigInt(startpos)
    | "Bytes" -> Model.Bytes(startpos)
    | _ -> Model.Custom(startpos, field_type)
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
    { Model.AttrArgString($startpos, str) }
  | func = ID; LEFT_PARAN; RIGHT_PARAN
    { Model.AttrArgFunc($startpos, func) }
  | LEFT_BRACKET; refs = separated_nonempty_list(COMMA, ID); RIGHT_BRACKET
    { Model.AttrArgRefList($startpos, refs) }
  | TRUE
    { Model.AttrArgBoolean($startpos, true) }
  | FALSE
    { Model.AttrArgBoolean($startpos, false) }
  | number = INT
    { Model.AttrArgNumber($startpos, number) }
  ;

model_field_attr:
  | attr = MODEL_FIELD_ATTR;
    { Model.AttributeNoArgs($startpos, attr) }
  | attr = MODEL_FIELD_ATTR; LEFT_PARAN; args = separated_list(COMMA, model_field_attr_args); RIGHT_PARAN
    { Model.AttributeWithArgs($startpos, attr, args) }
  ;

model_field_type:
  | field_type = ID
    { parse_field_type field_type $startpos }

model_field:
  | id = ID; field_type = model_field_type; SEMICOLON
    { Model.FieldNoModifierNoAttrs($startpos, id, field_type) }
  | id = ID; field_type = model_field_type; modifier = model_field_modifier; SEMICOLON
    { Model.FieldWithModifierNoAttrs($startpos, id, field_type, modifier) }
  | id = ID; field_type = model_field_type; attrs = list(model_field_attr); SEMICOLON
    { Model.FieldNoModifierWithAttrs($startpos, id, field_type, attrs) }
  | id = ID; field_type = model_field_type; modifier = model_field_modifier; attrs = list(model_field_attr); SEMICOLON
    { Model.FieldWithModifierWithAttrs($startpos, id, field_type, modifier, attrs) }
  ;

model_fields:
  | LEFT_BRACE; model_fields = list(model_field); RIGHT_BRACE
    { model_fields }
  ;

declaration:
  | MODEL; model_id = ID; model_fields = model_fields
    { Model($startpos, model_id, model_fields) }
  ;

program:
  | declarations = list(declaration); EOF
    { Program(declarations) }
  ;
