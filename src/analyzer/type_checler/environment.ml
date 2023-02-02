open Core
open Ast
open Ast.Ast_types
open Error_handler.Handler

module LocalEnv = struct
  type 'a t = (string, 'a) Hashtbl.t

  let create () : 'a t = Hashtbl.create ~size:10 (module String)

  let allocate (table : 'a t) ~key:(symbol : string) ~data:(value : 'a) : unit =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let lookup (table : 'a t) ~key:(symbol : string) : 'a =
    Hashtbl.find_exn table symbol

  let contains (table : 'a t) ~key:(symbol : string) : bool =
    Hashtbl.mem table symbol
end

module GlobalEnv = struct
  type 'a value_record = {
    declaration_type : declaration_type;
    table : 'a LocalEnv.t option;
  }

  type 'a t = (string, 'a value_record) Hashtbl.t

  let create () : 'a t = Hashtbl.create ~size:10 (module String)

  let allocate (table : 'a t) ~key:(symbol : string)
      ~data:(value : 'a value_record) : unit =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let get_table (table : 'a t) ~key:(symbol : string) =
    (Hashtbl.find_exn table symbol).table

  let get_declaration_type (table : 'a t) ~key:(symbol : string) :
      declaration_type =
    (Hashtbl.find_exn table symbol).declaration_type

  let contains (table : 'a t) ~key:(symbol : string) : bool =
    Hashtbl.mem table symbol

  let check_type (table : 'a t) ~key:(symbol : string)
      (declaration_type : declaration_type) : bool =
    if phys_equal (get_declaration_type table ~key:symbol) declaration_type then
      true
    else false
end

module ModelEnv = struct
  type field_info = {
    typ : typ;
    field_attrs_table : Model.attr_arg list LocalEnv.t;
  }

  let rec allocate_field_attrs (local_env : Model.attr_arg list LocalEnv.t)
      (field_id : string) (attrs : Model.attribute list) : unit =
    match attrs with
    | [] -> ()
    | attr :: attrs ->
        (match attr with
        | Model.Attribute (loc, id, args) ->
            if LocalEnv.contains local_env ~key:id then
              raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
            LocalEnv.allocate local_env ~key:id ~data:args);
        allocate_field_attrs local_env field_id attrs

  let rec allocate_fields (local_env : 'a LocalEnv.t) (body : Model.body) : unit
      =
    match body with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Model.Field (loc, id, typ, field_attrs) ->
            if LocalEnv.contains local_env ~key:id then
              raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
            let field_attrs_table = LocalEnv.create () in
            allocate_field_attrs field_attrs_table id field_attrs;
            let field = { typ; field_attrs_table } in
            LocalEnv.allocate local_env ~key:id ~data:field);
        allocate_fields local_env fields

  let allocate_model (global_env : 'a GlobalEnv.t) (loc, id, body) : unit =
    if GlobalEnv.contains global_env ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let table = LocalEnv.create () in
    let declaration_type = ModelType in
    allocate_fields table body;
    let table = Some table in
    GlobalEnv.allocate global_env ~key:id ~data:{ declaration_type; table }
end

module QueryEnv = struct
  let allocate_query (global_env : 'a GlobalEnv.t) (query : query_declaration) :
      unit =
    let loc, id, _, _, _, _ = query in
    if GlobalEnv.contains global_env ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let table = None in
    let declaration_type = QueryType in
    GlobalEnv.allocate global_env ~key:id ~data:{ declaration_type; table }
end

module XRAEnv = struct
  type xra_env = (string, string * string) Hashtbl.t Stack.t
end

module SymbolTableManager = struct
  let populate (global_env : 'a GlobalEnv.t) (Ast declarations) : unit =
    let populate_declaration global_env declaration =
      match declaration with
      | Model model -> ModelEnv.allocate_model global_env model
      | Query query -> QueryEnv.allocate_query global_env query
      | Component _ -> ()
      | Page _ -> ()
    in
    List.iter
      ~f:(fun declaration -> populate_declaration global_env declaration)
      declarations
end
