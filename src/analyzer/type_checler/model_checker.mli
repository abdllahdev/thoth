val get_custom_scalar_type : Ast.Ast_types.scalar_type -> string option
val get_custom_type : Ast.Ast_types.typ -> string option
val get_scalar_type : Ast.Ast_types.typ -> Ast.Ast_types.scalar_type

val check_field_attr :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t ->
  string ->
  Ast.Ast_types.Model.attribute ->
  unit

val check_field_attrs :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t ->
  string ->
  Ast.Ast_types.Model.attribute list ->
  unit

val check_field_type :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  string ->
  string ->
  Ast.Ast_types.typ ->
  Lexing.position ->
  unit

val check_field :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t ->
  string ->
  Ast.Ast_types.Model.field ->
  unit

val check_fields :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t ->
  string ->
  Ast.Ast_types.Model.field list ->
  unit

val check_model :
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t
  Environment.GlobalEnvironment.t ->
  Environment.ModelEnvironment.field_info Environment.LocalEnvironment.t ->
  string ->
  Ast.Ast_types.Model.field list ->
  unit
