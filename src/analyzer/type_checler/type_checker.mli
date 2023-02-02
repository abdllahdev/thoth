val check_declaration :
  Environment.ModelEnv.field_info Environment.LocalEnv.t Environment.GlobalEnv.t ->
  Ast.Ast_types.declaration list ->
  unit

val run_type_checker : Ast.Ast_types.ast -> unit
