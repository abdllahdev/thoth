open Core
open Ast
open Ast.Ast_types
open Error_handler.Handler

module LocalEnvironment = struct
  type 'a t = (string, 'a) Hashtbl.t

  let create () : 'a t = Hashtbl.create ~size:17 (module String)

  let allocate table ~key:symbol ~data:value =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let lookup table ~key:symbol = Hashtbl.find_exn table symbol
  let contains table ~key:symbol = Hashtbl.mem table symbol
end

module GlobalEnvironment = struct
  type 'a value_record = {
    declaration_type : declaration_type;
    value : 'a option;
  }

  type 'a t = (string, 'a value_record) Hashtbl.t

  let create () = Hashtbl.create ~size:17 (module String)

  let allocate table ~key:symbol ~data:value =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let get_value table ~key:symbol = (Hashtbl.find_exn table symbol).value

  let get_declaration_type table ~key:symbol =
    (Hashtbl.find_exn table symbol).declaration_type

  let contains table ~key:symbol = Hashtbl.mem table symbol

  let check_type table ~key:symbol declaration_type =
    if phys_equal (get_declaration_type table ~key:symbol) declaration_type then
      true
    else false
end

module ModelEnvironment = struct
  type field_info = {
    typ : typ;
    field_attrs_table : Model.attr_arg list LocalEnvironment.t;
  }

  let rec allocate_field_attrs local_env field_id attrs =
    match attrs with
    | [] -> ()
    | attr :: attrs ->
        (match attr with
        | Model.Attribute (loc, id, args) ->
            if LocalEnvironment.contains local_env ~key:id then
              raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
            LocalEnvironment.allocate local_env ~key:id ~data:args);
        allocate_field_attrs local_env field_id attrs

  let rec allocate_fields local_env body =
    match body with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Model.Field (loc, id, typ, field_attrs) ->
            if LocalEnvironment.contains local_env ~key:id then
              raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
            let field_attrs_table = LocalEnvironment.create () in
            allocate_field_attrs field_attrs_table id field_attrs;
            let field = { typ; field_attrs_table } in
            LocalEnvironment.allocate local_env ~key:id ~data:field);
        allocate_fields local_env fields

  let allocate_model (global_env : 'a GlobalEnvironment.t) (loc, id, body) =
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let table = LocalEnvironment.create () in
    allocate_fields table body;
    let declaration_type = ModelType in
    let value = Some table in
    GlobalEnvironment.allocate global_env ~key:id
      ~data:{ declaration_type; value }
end

module XRAEnvironment = struct
  type scope = (string, string * string) Hashtbl.t
  type env = scope list

  let create_scope () = Hashtbl.create ~size:17 (module String)
  let create_env () = [ create_scope () ]

  let rec lookup env loc symbol =
    match env with
    | [] -> raise_name_error (Pprinter.string_of_loc loc) "variable" symbol
    | scope :: env ->
        if Hashtbl.mem scope symbol then Hashtbl.find_exn scope symbol
        else lookup env loc symbol

  let shrink env = match env with [] -> () | _ :: tail -> tail |> ignore
  let extend env scope = scope :: env |> ignore
end

module EnvironmentManager = struct
  let allocate (global_env : 'a GlobalEnvironment.t) loc id declaration_type =
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let value = None in
    GlobalEnvironment.allocate global_env ~key:id
      ~data:{ declaration_type; value }

  let populate (global_env : 'a GlobalEnvironment.t) (Ast declarations) : unit =
    let populate_declaration global_env declaration =
      match declaration with
      | Model model -> ModelEnvironment.allocate_model global_env model
      | Query (loc, id, _, _, _, _) -> allocate global_env loc id QueryType
      | Component (loc, id, _, _) -> allocate global_env loc id ComponentType
      | Page (loc, id, _, _, _) -> allocate global_env loc id PageType
    in
    List.iter
      ~f:(fun declaration -> populate_declaration global_env declaration)
      declarations
end
