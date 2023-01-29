open Core
open Ast
open Ast.Ast_types
open Error_handler.Handler

module LocalSymbolTable = struct
  type 'a t = (string, 'a) Hashtbl.t

  let create () : 'a t = Hashtbl.create ~size:10 (module String)

  let allocate (table : 'a t) ~key:(symbol : string) ~data:(value : 'a) : unit =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let lookup (table : 'a t) ~key:(symbol : string) : 'a =
    Hashtbl.find_exn table symbol

  let contains (table : 'a t) ~key:(symbol : string) : bool =
    Hashtbl.mem table symbol
end

module GlobalSymbolTable = struct
  type 'a value_record = {
    declaration_type : declaration_type;
    some_table : 'a LocalSymbolTable.t option;
  }

  type 'a t = (string, 'a value_record) Hashtbl.t

  let create () : 'a t = Hashtbl.create ~size:10 (module String)

  let allocate (table : 'a t) ~key:(symbol : string)
      ~data:(value : 'a value_record) : unit =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let get_table (table : 'a t) ~key:(symbol : string) :
      'a LocalSymbolTable.t option =
    (Hashtbl.find_exn table symbol).some_table

  let get_declaration_type (table : 'a t) ~key:(symbol : string) :
      declaration_type =
    (Hashtbl.find_exn table symbol).declaration_type

  let contains (table : 'a t) ~key:(symbol : string) : bool =
    Hashtbl.mem table symbol

  let check_type (table : 'a t) ~key:(symbol : string)
      (declaraction_type : declaration_type) : bool =
    if phys_equal (get_declaration_type table ~key:symbol) declaraction_type
    then true
    else false
end

module ModelManager = struct
  type field_record = {
    typ : typ;
    field_attrs_table : Model.attr_arg list LocalSymbolTable.t;
  }

  let rec allocate_field_attrs (local_table : 'a LocalSymbolTable.t)
      (field_id : string) (attrs : Model.attribute list) : unit =
    match attrs with
    | [] -> ()
    | attr :: attrs ->
        (match attr with
        | Model.Attribute (loc, id, args) ->
            if LocalSymbolTable.contains local_table ~key:id then
              raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
            LocalSymbolTable.allocate local_table ~key:id ~data:args);
        allocate_field_attrs local_table field_id attrs

  let rec allocate_fields (local_table : 'a LocalSymbolTable.t)
      (body : Model.body) : unit =
    match body with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Model.Field (loc, id, typ, field_attrs) ->
            if LocalSymbolTable.contains local_table ~key:id then
              raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
            let field_attrs_table = LocalSymbolTable.create () in
            allocate_field_attrs field_attrs_table id field_attrs;
            let field = { typ; field_attrs_table } in
            LocalSymbolTable.allocate local_table ~key:id ~data:field);
        allocate_fields local_table fields

  let allocate_model (global_table : 'a GlobalSymbolTable.t)
      (Model (loc, id, body)) : unit =
    if GlobalSymbolTable.contains global_table ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let table = LocalSymbolTable.create () in
    let some_table = Some table in
    let declaration_type = ModelType in
    allocate_fields table body;
    GlobalSymbolTable.allocate global_table ~key:id
      ~data:{ declaration_type; some_table }
end

module QueryManager = struct
  type query_record = {
    typ : Query.typ;
    args : Query.arg list;
    models : Query.model list;
    permissions : Query.permission list;
  }

  let allocation_body (local_table : 'a LocalSymbolTable.t) (id : id)
      (body : Query.body) : unit =
    let typ, args, models, permissions = body in
    let query_body = { typ; args; models; permissions } in
    LocalSymbolTable.allocate local_table ~key:id ~data:query_body

  let allocate_query (global_table : 'a GlobalSymbolTable.t)
      (Query (loc, id, body)) : unit =
    if GlobalSymbolTable.contains global_table ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let table = LocalSymbolTable.create () in
    let some_table = Some table in
    let declaration_type = QueryType in
    allocation_body table id body;
    GlobalSymbolTable.allocate global_table ~key:id
      ~data:{ declaration_type; some_table }
end

module SymbolTableManager = struct
  (* check for illegal identifiers like any keywords used in the language *)
  let populate (global_table : 'a GlobalSymbolTable.t) (Ast declarations) : unit
      =
    let populate_declaration global_table declaration =
      match declaration with
      | Model model -> QueryManager.allocate_query global_table (Model model)
      | Query query -> QueryManager.allocate_query global_table (Query query)
      | Component _ -> ()
    in
    List.iter
      ~f:(fun declaration -> populate_declaration global_table declaration)
      declarations
end
