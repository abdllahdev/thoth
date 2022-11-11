%{
  open Syntax
%}

%token <int>    INT
%token <string> ID
%token <string> STRING
%token          MODEL_FIELD_MODIFIER_OPTIONAL
%token          MODEL_FIELD_MODIFIER_LIST
%token          MODEL_FIELD_ATTR_ID
%token          MODEL_FIELD_ATTR_DEFAULT
%token          MODEL_FIELD_ATTR_UNIQUE
%token          MODEL_FIELD_ATTR_RELATION
%token          MODEL_FIELD_ATTR_UPDATED_AT
%token          MODEL_FIELD_ATTR_IGNORE
%token          MODEL_FIELD_ATTR_FUNC_AUTOINCREMENT
%token          MODEL_FIELD_ATTR_FUNC_NOW
%token          TRUE
%token          FALSE
%token          LEFT_BRACE
%token          RIGHT_BRACE
%token          LEFT_BRACKET
%token          RIGHT_BRACKET
%token          LEFT_PARAN
%token          RIGHT_PARAN
%token          EQUAL
%token          QUESTION_MARK
%token          SEMICOLON
%token          MODEL
%token          EOF

%start <program> program

%type <Model.field> model_field
%type <Model.body> model_body
%type <Model.definition> model_definition
%type <declaration> declaration

%%

model_field_modifier:
  | MODEL_FIELD_MODIFIER_OPTIONAL
    { Model.Optional }
  | MODEL_FIELD_MODIFIER_LIST
    { Model.List }
  ;

model_field_attr_no_args:
  | MODEL_FIELD_ATTR_ID
    { Model.AttributeNoArgs(Model.Id) }
  | MODEL_FIELD_ATTR_UNIQUE
    { Model.AttributeNoArgs(Model.Unique) }
  | MODEL_FIELD_ATTR_IGNORE
    { Model.AttributeNoArgs(Model.Ignore) }
  ;

model_field_attr_default:
  | MODEL_FIELD_ATTR_DEFAULT; LEFT_PARAN; MODEL_FIELD_ATTR_FUNC_AUTOINCREMENT; RIGHT_PARAN
    {
      let func = Model.AttrArgFunc(Model.Autoincrement) in
      let arg = Model.Argument(func) in
      Model.AttributeWithArgs(Model.Default, arg)
    }
  | MODEL_FIELD_ATTR_DEFAULT; LEFT_PARAN; MODEL_FIELD_ATTR_FUNC_NOW; RIGHT_PARAN
    {
      let func = Model.AttrArgFunc(Model.Now) in
      let arg = Model.Argument(func) in
      Model.AttributeWithArgs(Model.Default, arg)
    }
  | MODEL_FIELD_ATTR_DEFAULT; LEFT_PARAN; TRUE; RIGHT_PARAN
    {
      let boolean = Model.AttrArgBoolean(true) in
      let arg = Model.Argument(boolean) in
      Model.AttributeWithArgs(Model.Default, arg)
    }
  | MODEL_FIELD_ATTR_DEFAULT; LEFT_PARAN; FALSE; RIGHT_PARAN
    {
      let boolean = Model.AttrArgBoolean(false) in
      let arg = Model.Argument(boolean) in
      Model.AttributeWithArgs(Model.Default, arg)
    }
  | MODEL_FIELD_ATTR_DEFAULT; LEFT_PARAN; str = STRING; RIGHT_PARAN
    {
      let str = Model.AttrArgString(str) in
      let arg = Model.Argument(str) in
      Model.AttributeWithArgs(Model.Default, arg)
    }
  | MODEL_FIELD_ATTR_DEFAULT; LEFT_PARAN; num = INT; RIGHT_PARAN
    {
      let num = Model.AttrArgNumber(num) in
      let arg = Model.Argument(num) in
      Model.AttributeWithArgs(Model.Default, arg)
    }
  ;

model_field_attr_with_arg:
  | default = model_field_attr_default
    { default }

model_field_attr:
  | attr_no_args = model_field_attr_no_args;
    { attr_no_args }
  | attr_with_args = model_field_attr_with_arg;
    { attr_with_args }
  ;

// Add the extra options here
model_field:
  | id = ID; typ = ID; SEMICOLON
    {Model.FieldNoModiferNoAttrs(id, typ)}
  | id = ID; typ = ID; modifier = model_field_modifier; SEMICOLON
    {Model.FieldWithModifierNoAttrs(id, typ, modifier)}
  | id = ID; typ = ID; attrs = list(model_field_attr); SEMICOLON
    {Model.FieldNoModiferWithAttrs(id, typ, attrs)}
  | id = ID; typ = ID; modifier = model_field_modifier; attrs = list(model_field_attr); SEMICOLON
    {Model.FieldWithModiferWithAttrs(id, typ, modifier, attrs)}
  ;

model_body:
  | LEFT_BRACE; model_fields = list(model_field); RIGHT_BRACE
    {Model.Body(model_fields)}
  ;

model_definition:
  | MODEL; model_id = ID; EQUAL; model_body = model_body
    {Model.Definition(model_id, model_body)}
  ;

declaration:
  | model_definition = model_definition
    { Model(model_definition) }
  ;

program:
  | declarations = list(declaration); EOF
    {(Program declarations)}
  ;
