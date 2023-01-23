module LocalSymbolTable : sig
  type 'a t = (string, 'a) Base.Hashtbl.t

  val create : unit -> 'a t
  val allocate : 'a t -> key:string -> data:'a -> unit
  val lookup : 'a t -> key:string -> 'a
  val contains : 'a t -> key:string -> bool
end

module GlobalSymbolTable : sig
  type 'a value_record = {
    declaration_type : Ast.Ast_types.declaration_type;
    some_table : 'a LocalSymbolTable.t option;
  }

  type 'a t = (string, 'a value_record) Base.Hashtbl.t

  val create : unit -> 'a t
  val allocate : 'a t -> key:string -> data:'a value_record -> unit
  val get_table : 'a t -> key:string -> 'a LocalSymbolTable.t option

  val get_declaration_type :
    'a t -> key:string -> Ast.Ast_types.declaration_type

  val contains : 'a t -> key:string -> bool
  val check_type : 'a t -> key:string -> Ast.Ast_types.declaration_type -> bool
end

module ModelManager : sig
  type field_record = {
    typ : Ast.Ast_types.typ;
    field_attrs_table : Ast.Ast_types.Model.attr_arg list LocalSymbolTable.t;
  }

  val allocate_field_attrs :
    Ast.Ast_types.Model.attr_arg list LocalSymbolTable.t ->
    string ->
    Ast.Ast_types.Model.attribute list ->
    unit

  val allocate_fields :
    field_record LocalSymbolTable.t -> Ast.Ast_types.Model.body -> unit
end

module SymbolTableManager : sig
  val populate :
    ModelManager.field_record GlobalSymbolTable.t -> Ast.Ast_types.ast -> unit
end
