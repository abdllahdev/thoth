module LocalEnv : sig
  type 'a t = (string, 'a) Base.Hashtbl.t

  val create : unit -> 'a t
  val allocate : 'a t -> key:string -> data:'a -> unit
  val lookup : 'a t -> key:string -> 'a
  val contains : 'a t -> key:string -> bool
end

module GlobalEnv : sig
  type 'a value_record = {
    declaration_type : Ast.Ast_types.declaration_type;
    value : 'a option;
  }

  type 'a t = (string, 'a value_record) Base.Hashtbl.t

  val create : unit -> 'a t
  val allocate : 'a t -> key:string -> data:'a value_record -> unit
  val get_value : 'a t -> key:string -> 'a option

  val get_declaration_type :
    'a t -> key:string -> Ast.Ast_types.declaration_type

  val contains : 'a t -> key:string -> bool
  val check_type : 'a t -> key:string -> Ast.Ast_types.declaration_type -> bool
end

module ModelEnv : sig
  type field_info = {
    typ : Ast.Ast_types.typ;
    field_attrs_table : Ast.Ast_types.Model.attr_arg list LocalEnv.t;
  }

  val allocate_field_attrs :
    Ast.Ast_types.Model.attr_arg list LocalEnv.t ->
    string ->
    Ast.Ast_types.Model.attribute list ->
    unit

  val allocate_fields :
    field_info LocalEnv.t -> Ast.Ast_types.Model.body -> unit

  val allocate_model :
    field_info LocalEnv.t GlobalEnv.t ->
    Lexing.position * string * Ast.Ast_types.Model.body ->
    unit
end

module XRAEnv : sig
  type scope = (string, string * string) Base.Hashtbl.t
  type env = scope list

  val create_scope : unit -> scope
  val create_env : unit -> env
  val lookup : env -> Ast.Ast_types.loc -> Ast.Ast_types.id -> string * string
  val shrink : env -> unit
  val extend : env -> scope -> unit
end

module EnvironmentManager : sig
  val allocate :
    'a GlobalEnv.t ->
    Lexing.position ->
    string ->
    Ast.Ast_types.declaration_type ->
    unit

  val populate :
    ModelEnv.field_info LocalEnv.t GlobalEnv.t -> Ast.Ast_types.ast -> unit
end
