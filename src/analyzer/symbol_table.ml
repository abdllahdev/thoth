open Ast
open Error

(* Tomorrow you need to finish all the functions in this file once you do that
   - you will be able to populate the symbol table
   As a next step, you will need to use the symbol table to find semantic errors like:
   - Undefined and unreachable fields
   - You will also be able to find out any wrong applications of attributes
   - Undefined types

   This should be an easy task to do. So please finish it tomorrow. Once you have that implemented,
   implementing IR and the code generator will be very easy (it should take you a full day of work to do both)
   Once, you finish all of these components and get a database up and running, it will be very easy to
   to build the rest of the backend features *)

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
    field_type : Model.field_type;
    field_type_modifier : Model.field_type_modifier;
    field_attrs_table : Model.field_attr_arg list LocalSymbolTable.t;
  }

  let rec add_field_attrs (table : 'a LocalSymbolTable.t)
      (attrs : Model.field_attr list) : unit =
    match attrs with
    | [] -> ()
    | attr :: attrs ->
        Type_checker.check_field_attr attr;
        (match attr with
        | Model.Attribute (_, id, args) ->
            if LocalSymbolTable.contains table id then
              raise
                (NameError
                   (Fmt.str
                      "NameError: A field cannot have the same attribute %s \
                       assigned twice."
                      id));
            LocalSymbolTable.allocate table id args);
        add_field_attrs table attrs

  let rec add_fields (table : 'a LocalSymbolTable.t) (fields : Model.field list)
      : unit =
    match fields with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Model.Field (_, id, field_type, field_type_modifier, field_attrs) ->
            if LocalSymbolTable.contains table id then
              raise
                (MultiDefinition
                   (Fmt.str
                      "MultiDefinition: The field %s was defined multiple times"
                      id));
            let field_attrs_table = LocalSymbolTable.create () in
            add_field_attrs field_attrs_table field_attrs;
            let field =
              { field_type; field_type_modifier; field_attrs_table }
            in
            LocalSymbolTable.allocate table id field);
        add_fields table fields

  let rec populate (Ast declarations) (table : 'a GlobalSymbolTable.t) : unit =
    match declarations with
    | [] -> ()
    | declaration :: declarations ->
        (match declaration with
        | Model (_, id, fields) ->
            let model_table = LocalSymbolTable.create () in
            add_fields model_table fields;
            GlobalSymbolTable.allocate table id model_table);
        populate (Ast declarations) table
end
