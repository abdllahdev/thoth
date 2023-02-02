val check_models :
  'a Environment.GlobalEnvironment.t ->
  string ->
  Ast.Ast_types.Query.model list ->
  unit

val check_where_arg :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Lexing.position ->
  string ->
  Ast.Ast_types.Query.model ->
  string ->
  unit

val check_filter_arg :
  'a Environment.LocalEnvironment.t Environment.GlobalEnvironment.t ->
  Lexing.position ->
  Ast.Ast_types.Query.model ->
  string list ->
  unit

val check_data_arg :
  'a Environment.LocalEnvironment.t Environment.GlobalEnvironment.t ->
  Lexing.position ->
  Ast.Ast_types.Query.model ->
  string list ->
  unit

val check_args :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Lexing.position ->
  Ast.Ast_types.Query.typ ->
  string ->
  Ast.Ast_types.Query.model ->
  Ast.Ast_types.Query.arg list ->
  unit

val check_query :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Ast.Ast_types.query_declaration ->
  unit
