val get_custom_scalar_type : Ast.Ast_types.scalar_type -> string option
val get_custom_type : Ast.Ast_types.typ -> string option
val extract_scalar_type : Ast.Ast_types.typ -> Ast.Ast_types.scalar_type

val check_field_attr :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Symbol_table.ModelManager.field_record Symbol_table.LocalSymbolTable.t ->
  string ->
  Ast.Ast_types.Model.attribute ->
  unit

val check_field_attrs :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Symbol_table.ModelManager.field_record Symbol_table.LocalSymbolTable.t ->
  string ->
  Ast.Ast_types.Model.attribute list ->
  unit

val check_field_type :
  'a Symbol_table.GlobalSymbolTable.t ->
  Ast.Ast_types.typ ->
  Lexing.position ->
  unit

val check_field :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Symbol_table.ModelManager.field_record Symbol_table.LocalSymbolTable.t ->
  Ast.Ast_types.Model.field ->
  unit

val check_fields :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Symbol_table.ModelManager.field_record Symbol_table.LocalSymbolTable.t ->
  Ast.Ast_types.Model.field list ->
  unit

val check_model :
  Symbol_table.ModelManager.field_record Symbol_table.GlobalSymbolTable.t ->
  Symbol_table.ModelManager.field_record Symbol_table.LocalSymbolTable.t ->
  Ast.Ast_types.Model.field list ->
  unit
