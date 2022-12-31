(* First step in the type checker is to:
   1- Populate the symbol table
   2- Check for undefined and unreachable variable
   3- Do type checking *)

open Ast
open Error
open Symbol_table

(* TODO:
   1- Populate the SymbolTable.
   2- Traverse the AST.
   Model errors to check:
   4- Check if there is any *)

module TypeChecker = struct
  let check_model_field_type (table : 'a GlobalSymbolTable.t)
      (field_type : Model.field_type) : unit =
    match field_type with
    | FieldTypeCustom (loc, field_type_custom) ->
        if not (GlobalSymbolTable.contains table field_type_custom) then
          raise
            (NameError
               (Fmt.str "NameError@(%s): Unknown type '%s'"
                  (Pprinter.string_of_loc loc)
                  field_type_custom))
    | _ -> ()

  let rec check_model_fields (table : 'a GlobalSymbolTable.t)
      (fields : Model.field list) : unit =
    match fields with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Field (_, _, field_type, _, _) ->
            check_model_field_type table field_type);
        check_model_fields table fields

  let check_declaration (table : 'a GlobalSymbolTable.t)
      (declaration : declaration) : unit =
    match declaration with
    | Model (_, _, fields) -> check_model_fields table fields

  let rec semantic_check (table : 'a GlobalSymbolTable.t) (Ast declarations) :
      unit =
    match declarations with
    | [] -> ()
    | declaration :: declarations ->
        check_declaration table declaration;
        semantic_check table (Ast declarations)

  let run (Ast declarations) : unit =
    let table = GlobalSymbolTable.create () in
    SymbolTableManager.populate table (Ast declarations);
    semantic_check table (Ast declarations)
end
