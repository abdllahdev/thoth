open Ast
open Error

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
  type 'a t = (string, 'a LocalSymbolTable.t) Hashtbl.t

  let create () : 'a t = Hashtbl.create 10

  let allocate (table : 'a t) (symbol : string) (value : 'a LocalSymbolTable.t)
      : unit =
    Hashtbl.add table symbol value

  let lookup (table : 'a t) (symbol : string) : 'a LocalSymbolTable.t =
    Hashtbl.find table symbol

  let contains (table : 'a t) (symbol : string) : bool =
    Hashtbl.mem table symbol
end

module SymbolTableManager = struct
  type field_record = {
    typ : typ;
    field_attrs_table : Model.field_attr_arg list LocalSymbolTable.t;
  }

  let rec add_field_attrs (local_table : 'a LocalSymbolTable.t)
      (field_id : string) (attrs : Model.field_attr list) : unit =
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
        add_field_attrs local_table field_id attrs

  let rec add_fields (local_table : 'a LocalSymbolTable.t)
      (fields : Model.field list) : unit =
    match fields with
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
            add_field_attrs field_attrs_table id field_attrs;
            let field = { typ; field_attrs_table } in
            LocalSymbolTable.allocate local_table id field);
        add_fields local_table fields

  let rec populate (global_table : 'a GlobalSymbolTable.t) (Ast declarations) :
      unit =
    match declarations with
    | [] -> ()
    | declaration :: declarations ->
        (match declaration with
        | Model (loc, id, fields) ->
            if GlobalSymbolTable.contains global_table id then
              raise
                (MultiDefinitionsError
                   (Fmt.str
                      "MultiDefinitionsError@(%s): The field '%s' was defined \
                       multiple times"
                      (Pprinter.string_of_loc loc)
                      id));
            let model_table = LocalSymbolTable.create () in
            add_fields model_table fields;
            GlobalSymbolTable.allocate global_table id model_table);
        populate global_table (Ast declarations)
end
