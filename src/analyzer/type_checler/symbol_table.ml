open Ast
open Ast.Ast_types
open Error_handler.Error

module LocalSymbolTable = struct
  type 'a t = (string, 'a) Hashtbl.t

  let create () : 'a t = Hashtbl.create 10

  let allocate (table : 'a t) (symbol : string) (value : 'a) : unit =
    Hashtbl.add table symbol value

  let lookup (table : 'a t) (symbol : string) : 'a = Hashtbl.find table symbol

  let contains (table : 'a t) (symbol : string) : bool =
    Hashtbl.mem table symbol
end

module GlobalSymbolTable = struct
  type 'a value_record = {
    declaration_type : declaration_type;
    some_table : 'a LocalSymbolTable.t option;
  }

  type 'a t = (string, 'a value_record) Hashtbl.t

  let create () : 'a t = Hashtbl.create 10

  let allocate (table : 'a t) (symbol : string) (value : 'a value_record) : unit
      =
    Hashtbl.add table symbol value

  let get_table (table : 'a t) (symbol : string) : 'a LocalSymbolTable.t option
      =
    (Hashtbl.find table symbol).some_table

  let get_declaration_type (table : 'a t) (symbol : string) : declaration_type =
    (Hashtbl.find table symbol).declaration_type

  let contains (table : 'a t) (symbol : string) : bool =
    Hashtbl.mem table symbol

  let check_type (table : 'a t) (symbol : string)
      (declaraction_type : declaration_type) : bool =
    if get_declaration_type table symbol = declaraction_type then true
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
            if LocalSymbolTable.contains local_table id then
              raise
                (NameError
                   (Fmt.str
                      "MultiDefinitionsError@(%s): The field '%s' cannot have \
                       the same attribute '%s' assigned twice."
                      (Pprinter.string_of_loc loc)
                      field_id id));
            LocalSymbolTable.allocate local_table id args);
        allocate_field_attrs local_table field_id attrs

  let rec allocate_fields (local_table : 'a LocalSymbolTable.t)
      (body : Model.body) : unit =
    match body with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Model.Field (loc, id, typ, field_attrs) ->
            if LocalSymbolTable.contains local_table id then
              raise
                (MultiDefinitionsError
                   (Fmt.str
                      "MultiDefinitionsError@(%s): The field '%s' was defined \
                       multiple times"
                      (Pprinter.string_of_loc loc)
                      id));
            let field_attrs_table = LocalSymbolTable.create () in
            allocate_field_attrs field_attrs_table id field_attrs;
            let field = { typ; field_attrs_table } in
            LocalSymbolTable.allocate local_table id field);
        allocate_fields local_table fields
end

module SymbolTableManager = struct
  let rec populate (global_table : 'a GlobalSymbolTable.t) (Ast declarations) :
      unit =
    match declarations with
    | [] -> ()
    | declaration :: declarations ->
        (match declaration with
        | Model (loc, id, body) ->
            if GlobalSymbolTable.contains global_table id then
              raise
                (MultiDefinitionsError
                   (Fmt.str
                      "MultiDefinitionsError@(%s): '%s' is defined multiple \
                       times"
                      (Pprinter.string_of_loc loc)
                      id));
            let table = LocalSymbolTable.create () in
            let some_table = Some table in
            let declaration_type = ModelType in
            ModelManager.allocate_fields table body;
            GlobalSymbolTable.allocate global_table id
              { declaration_type; some_table }
        | Query (loc, id, _) ->
            if GlobalSymbolTable.contains global_table id then
              raise
                (MultiDefinitionsError
                   (Fmt.str
                      "MultiDefinitionsError@(%s): '%s' is defined multiple \
                       times"
                      (Pprinter.string_of_loc loc)
                      id));
            let some_table = None in
            let declaration_type = QueryType in
            GlobalSymbolTable.allocate global_table id
              { declaration_type; some_table });
        populate global_table (Ast declarations)
end
