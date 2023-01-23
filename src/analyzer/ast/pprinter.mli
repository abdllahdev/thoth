val string_of_loc : Lexing.position -> string
val string_of_scalar_type : Ast_types.scalar_type -> string
val string_of_composite_type : Ast_types.composite_type -> string
val string_of_literal : Ast_types.literal -> string

module ModelPrinter : sig
  val string_of_field_attr_arg : Ast_types.Model.attr_arg -> string
  val string_of_field_attr_args : Ast_types.Model.attr_arg list -> string
  val string_of_field_attr : Ast_types.Model.attribute -> string
  val string_of_field_attrs : Ast_types.Model.attribute list -> string
  val string_of_field_type : Ast_types.typ -> string
  val string_of_field : Ast_types.Model.field -> string
  val string_of_fields : Ast_types.Model.field list -> string
end

module QueryPrinter : sig
  val string_of_query_type : Ast_types.Query.typ -> string
end

val string_of_declaration_type : Ast_types.declaration_type -> string
val string_of_declaration : Ast_types.declaration -> string
val string_of_declarations : Ast_types.declaration list -> string
val string_of_ast : Ast_types.ast -> string
