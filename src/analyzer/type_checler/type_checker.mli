val check_declaration :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Ast.Ast_types.declaration list ->
  unit

val run_type_checker : Ast.Ast_types.ast -> unit
