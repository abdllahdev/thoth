val get_custom_scalar_type : Ast.Ast_types.scalar_type -> string option
val get_custom_type : Ast.Ast_types.typ -> string option
val get_scalar_type : Ast.Ast_types.typ -> Ast.Ast_types.scalar_type

val check_field_attr :
  Environment.ModelEnv.field_info Environment.LocalEnv.t Environment.GlobalEnv.t ->
  Environment.ModelEnv.field_info Environment.LocalEnv.t ->
  string ->
  Ast.Ast_types.Model.attribute ->
  unit

val check_field_attrs :
  Environment.ModelEnv.field_info Environment.LocalEnv.t Environment.GlobalEnv.t ->
  Environment.ModelEnv.field_info Environment.LocalEnv.t ->
  string ->
  Ast.Ast_types.Model.attribute list ->
  unit

val check_field_type :
  Environment.ModelEnv.field_info Environment.LocalEnv.t Environment.GlobalEnv.t ->
  string ->
  string ->
  Ast.Ast_types.typ ->
  Lexing.position ->
  unit

val check_field :
  Environment.ModelEnv.field_info Environment.LocalEnv.t Environment.GlobalEnv.t ->
  Environment.ModelEnv.field_info Environment.LocalEnv.t ->
  string ->
  Ast.Ast_types.Model.field ->
  unit

val check_fields :
  Environment.ModelEnv.field_info Environment.LocalEnv.t Environment.GlobalEnv.t ->
  Environment.ModelEnv.field_info Environment.LocalEnv.t ->
  string ->
  Ast.Ast_types.Model.field list ->
  unit

val check_model :
  Environment.ModelEnv.field_info Environment.LocalEnv.t Environment.GlobalEnv.t ->
  Environment.ModelEnv.field_info Environment.LocalEnv.t ->
  string ->
  Ast.Ast_types.Model.field list ->
  unit
