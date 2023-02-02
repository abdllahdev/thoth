type id = string
type field_specs = { id : id; field_type : id; field_attrs : id }
type model_specs = { id : id; body : field_specs list }
type db_config = { user : id; password : id; host : id; name : id }
type db_specs = { models : model_specs list }

val generate_attr_arg_specs : Ast.Ast_types.Model.attr_arg -> string
val generate_attr_specs : Ast.Ast_types.Model.attribute -> string
val generate_attrs_specs : Ast.Ast_types.Model.attribute list -> string
val generate_field_type_specs : Ast.Ast_types.typ -> string
val generate_field_specs : Ast.Ast_types.Model.field -> field_specs
val generate_model_specs : Ast.Ast_types.model_declaration -> model_specs
val generate_db_specs : Ast.Ast_types.model_declaration list -> db_specs
