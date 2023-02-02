val get_model_declaration :
  Ast_types.declaration -> Ast_types.model_declaration option

val get_query_declaration :
  Ast_types.declaration -> Ast_types.query_declaration option

val get_component_declaration :
  Ast_types.declaration -> Ast_types.component_declaration option

val get_page_declaration :
  Ast_types.declaration -> Ast_types.page_declaration option

val get_filtered_ast : Ast_types.declaration list -> Ast_types.filtered_ast
