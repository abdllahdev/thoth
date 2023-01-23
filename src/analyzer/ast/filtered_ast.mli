val get_model_declaration :
  Ast_types.declaration -> Ast_types.model_declaration option

val get_query_declaration :
  Ast_types.declaration -> Ast_types.query_declaration option

val get_filtered_ast : Ast_types.declaration list -> Ast_types.filtered_ast
