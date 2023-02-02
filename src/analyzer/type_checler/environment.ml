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
  type field_value = {
    typ : typ;
    field_attrs_table : Model.attr_arg list LocalEnvironment.t;
  }

  type model_value = field_value LocalEnvironment.t

  type query_value = {
    typ : Query.typ;
    args : Query.arg list;
    models : Query.model list;
  }

  type declaration_value =
    | ModelValue of model_value
    | QueryValue of query_value

  type value = {
    declaration_type : declaration_type;
    declaration_value : declaration_value;
  }

  type t = (string, value) Hashtbl.t

  let create () = Hashtbl.create ~size:17 (module String)

  let allocate table ~key:symbol ~data:value =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let get_value table ~key:symbol =
    (Hashtbl.find_exn table symbol).declaration_value

  let get_declaration_type table ~key:symbol =
    (Hashtbl.find_exn table symbol).declaration_type

  let contains table ~key:symbol = Hashtbl.mem table symbol

  let check_type table ~key:symbol declaration_type =
    if phys_equal (get_declaration_type table ~key:symbol) declaration_type then
      true
    else false

  let get_model_value declaration_value =
    (match declaration_value with
    | ModelValue model_value -> Some model_value
    | _ -> None)
    |> Option.value_exn

  let get_query_value declaration_value =
    (match declaration_value with
    | QueryValue query_value -> Some query_value
    | _ -> None)
    |> Option.value_exn
end

module ModelEnvironment = struct
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
            let field : GlobalEnvironment.field_value =
              { typ; field_attrs_table }
            in
            LocalEnvironment.allocate local_env ~key:id ~data:field);
        allocate_fields local_env fields

  let allocate (global_env : GlobalEnvironment.t) model =
    let loc, id, body = model in
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let table = LocalEnvironment.create () in
    allocate_fields table body;
    let declaration_type = ModelType in
    let declaration_value = GlobalEnvironment.ModelValue table in
    GlobalEnvironment.allocate global_env ~key:id
      ~data:{ declaration_type; declaration_value }
end

module QueryEnvironment = struct
  let allocate (global_env : GlobalEnvironment.t) query =
    let loc, id, typ, args, models, _ = query in
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error (Pprinter.string_of_loc loc) id;
    let declaration_type = QueryType in
    let declaration_value =
      GlobalEnvironment.QueryValue { typ; args; models }
    in
    GlobalEnvironment.allocate global_env ~key:id
      ~data:{ declaration_type; declaration_value }
end

module XRAEnvironment = struct
  type scope = (string, string) Hashtbl.t
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
  let populate global_env (Ast declarations) =
    let populate_declaration global_env declaration =
      match declaration with
      | Model model -> ModelEnvironment.allocate global_env model
      | Query query -> QueryEnvironment.allocate global_env query
      | Component (_, _, _, _) -> ()
      | Page (_, _, _, _, _) -> ()
    in
    List.iter
      ~f:(fun declaration -> populate_declaration global_env declaration)
      declarations
end