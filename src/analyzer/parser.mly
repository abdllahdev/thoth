%{
  open Ast
%}

%token    <string>    ID
%token    <string>    PSL_BLOCK
%token                LEFT_BRACE
%token                RIGHT_BRACE
%token                EQUAL
%token                EOF
%token                MODEL

%start program

%type <program> program
%type <model_definition> model_definition

%%

program:
  | model_definitions=list(model_definition); EOF {Prog(model_definitions)}
  ;

model_definition:
  | MODEL; model_name = ID; EQUAL; LEFT_BRACE; psl_block = PSL_BLOCK RIGHT_BRACE
  {TModel(ModelName.of_string model_name, PslBlock.of_string psl_block)}
