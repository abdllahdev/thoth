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

  let check_field_attr (Model.Attribute (loc, id, args)) : unit =
    let args_length = List.length args in
    match id with
    | "@id" | "@unique" | "@ignore" | "@updatedAt" ->
        if args_length >= 1 then
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Expected 0 argument in '%s' but received %d."
                  (Pprinter.string_of_loc loc)
                  id args_length))
    | "@default" -> (
        if args_length > 1 || args_length == 0 then
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Expected 1 argument in '@default' but \
                   received %d."
                  (Pprinter.string_of_loc loc)
                  args_length))
        else
          let arg = List.hd args in
          match arg with
          | AttrArgFunc (_, _) ->
              raise
                (TypeError
                   (Fmt.str
                      "TypeError@(%s): Attribute '@default' doesn't accept \
                       functions as arguments."
                      (Pprinter.string_of_loc loc)))
          | _ -> ())
    | "@relation" ->
        if args_length > 3 || args_length < 3 then
          raise
            (TypeError
               (Fmt.str
                  "TypeError@(%s): Expected 3 argument in '@relation' but \
                   received %d."
                  (Pprinter.string_of_loc loc)
                  args_length))
    | _ -> raise (NameError (Fmt.str "NameError: Unknown attribute '%s'." id))

  let rec add_field_attrs (table : 'a LocalSymbolTable.t) (field_id : string)
      (attrs : Model.field_attr list) : unit =
    match attrs with
    | [] -> ()
    | attr :: attrs ->
        check_field_attr attr;
        (match attr with
        | Model.Attribute (loc, id, args) ->
            if LocalSymbolTable.contains table id then
              raise
                (NameError
                   (Fmt.str
                      "NameError@(%s): The field '%s' cannot have the same \
                       attribute '%s' assigned twice."
                      (Pprinter.string_of_loc loc)
                      field_id id));
            LocalSymbolTable.allocate table id args);
        add_field_attrs table field_id attrs

  let rec add_fields (table : 'a LocalSymbolTable.t) (fields : Model.field list)
      : unit =
    match fields with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Model.Field (loc, id, field_type, field_type_modifier, field_attrs) ->
            if LocalSymbolTable.contains table id then
              raise
                (MultiDefinitionsError
                   (Fmt.str
                      "MultiDefinitionsError@(%s): The field '%s' was defined \
                       multiple times"
                      (Pprinter.string_of_loc loc)
                      id));
            let field_attrs_table = LocalSymbolTable.create () in
            add_field_attrs field_attrs_table id field_attrs;
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
        | Model (loc, id, fields) ->
            if GlobalSymbolTable.contains table id then
              raise
                (MultiDefinitionsError
                   (Fmt.str
                      "MultiDefinitionsError@(%s): The field '%s' was defined \
                       multiple times"
                      (Pprinter.string_of_loc loc)
                      id));
            let model_table = LocalSymbolTable.create () in
            add_fields model_table fields;
            GlobalSymbolTable.allocate table id model_table);
        populate (Ast declarations) table
end
