val check_declaration :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Ast.Ast_types.declaration list ->
  unit

val run_type_checker : Ast.Ast_types.ast -> unit c
