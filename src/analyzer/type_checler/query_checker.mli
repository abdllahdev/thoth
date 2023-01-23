val check_models :
  'a Symbol_table.GlobalSymbolTable.t ->
  string ->
  Ast.Ast_types.Query.model list ->
  unit

val check_where_arg :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Lexing.position ->
  string ->
  Ast.Ast_types.Query.model ->
  string ->
  unit

val check_filter_arg :
  'a Symbol_table.GlobalSymbolTable.t ->
  Lexing.position ->
  Ast.Ast_types.Query.model ->
  string list ->
  unit

val check_data_arg :
  'a Symbol_table.GlobalSymbolTable.t ->
  Lexing.position ->
  Ast.Ast_types.Query.model ->
  string list ->
  unit

val check_args :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Lexing.position ->
  Ast.Ast_types.Query.typ ->
  string ->
  Ast.Ast_types.Query.model ->
  Ast.Ast_types.Query.arg list ->
  unit

val check_query :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Lexing.position ->
  string ->
  Ast.Ast_types.Query.body ->
  unit
