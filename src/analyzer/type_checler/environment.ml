open Core
open Ast.Ast_types
open Ast.Formatter
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
    field_attrs_table : Model.attr_arg list LocalEnvironment.t option;
  }

  type model_value = field_value LocalEnvironment.t
  type type_value = field_value LocalEnvironment.t

  type query_value = {
    typ : Query.typ;
    body : Query.body;
    model : Query.model option;
    permissions : permission list option;
    route : (loc * string * string) option;
    return_type : typ;
  }

  type component_value = {
    component_type : Component.typ;
    args : Component.arg list option;
  }

  type declaration_value =
    | ModelValue of model_value
    | TypeValue of type_value
    | QueryValue of query_value
    | ComponentValue of component_value
    | PageValue

  type t = (string, declaration_value) Hashtbl.t

  let create () = Hashtbl.create ~size:17 (module String)

  let allocate table ~key:symbol ~data:value =
    Hashtbl.add_exn table ~key:symbol ~data:value

  let lookup table ~key:symbol = Hashtbl.find_exn table symbol
  let contains table ~key:symbol = Hashtbl.mem table symbol

  let infer_type declaration_value =
    match declaration_value with
    | ModelValue _ -> ModelDeclaration
    | QueryValue _ -> QueryDeclaration
    | ComponentValue _ -> ComponentDeclaration
    | TypeValue _ -> TypeDeclaration
    | PageValue -> PageDeclaration

  let check_type declaration_value declaration_type =
    if
      String.equal
        (string_of_declaration_type (infer_type declaration_value))
        (string_of_declaration_type declaration_type)
    then true
    else false

  let get_model_value declaration_value =
    (match declaration_value with
    | ModelValue model_value -> Some model_value
    | _ -> None)
    |> Option.value_exn

  let get_type_value declaration_value =
    (match declaration_value with
    | TypeValue type_value -> Some type_value
    | _ -> None)
    |> Option.value_exn

  let get_query_value declaration_value =
    (match declaration_value with
    | QueryValue query_value -> Some query_value
    | _ -> None)
    |> Option.value_exn

  let get_component_value declaration_value =
    (match declaration_value with
    | ComponentValue component_value -> Some component_value
    | _ -> None)
    |> Option.value_exn
end

module TypeEnvironment = struct
  let rec allocate_fields local_env body =
    match body with
    | [] -> ()
    | field :: fields ->
        (match field with
        | loc, id, typ ->
            if LocalEnvironment.contains local_env ~key:id then
              raise_multi_definitions_error loc id;
            let value : GlobalEnvironment.field_value =
              { typ; field_attrs_table = None }
            in
            LocalEnvironment.allocate local_env ~key:id ~data:value);
        allocate_fields local_env fields

  let allocate (global_env : GlobalEnvironment.t) type_decl =
    let loc, id, fields = type_decl in
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error loc id;
    let table = LocalEnvironment.create () in
    allocate_fields table fields;
    let declaration_value = GlobalEnvironment.TypeValue table in
    GlobalEnvironment.allocate global_env ~key:id ~data:declaration_value
end

module ModelEnvironment = struct
  let rec allocate_field_attrs local_env field_id attrs =
    match attrs with
    | [] -> ()
    | attr :: attrs ->
        (match attr with
        | Model.Attribute (loc, id, args) ->
            if LocalEnvironment.contains local_env ~key:id then
              raise_multi_definitions_error loc id;
            LocalEnvironment.allocate local_env ~key:id ~data:args);
        allocate_field_attrs local_env field_id attrs

  let rec allocate_fields local_env body =
    match body with
    | [] -> ()
    | field :: fields ->
        (match field with
        | Model.Field (loc, id, typ, field_attrs) ->
            if LocalEnvironment.contains local_env ~key:id then
              raise_multi_definitions_error loc id;
            let field_attrs_table = LocalEnvironment.create () in
            allocate_field_attrs field_attrs_table id field_attrs;
            let field : GlobalEnvironment.field_value =
              { typ; field_attrs_table = Some field_attrs_table }
            in
            LocalEnvironment.allocate local_env ~key:id ~data:field);
        allocate_fields local_env fields

  let allocate (global_env : GlobalEnvironment.t) model =
    let loc, id, body = model in
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error loc id;
    let table = LocalEnvironment.create () in
    allocate_fields table body;
    let declaration_value = GlobalEnvironment.ModelValue table in
    GlobalEnvironment.allocate global_env ~key:id ~data:declaration_value
end

module QueryEnvironment = struct
  let allocate (global_env : GlobalEnvironment.t) query =
    let loc, id, typ, return_type, body, model, permissions, route = query in
    (* check if model is passed for default queries *)
    (match model with
    | Some _ -> (
        match typ with
        | Query.Custom ->
            failwith
              (Fmt.str
                 "QueryAttributeError: @(%s): Custom query cannot have @model \
                  attribute"
                 (string_of_loc loc))
        | _ -> ())
    | None -> (
        match typ with
        | Query.Custom -> ()
        | _ ->
            failwith
              (Fmt.str
                 "QueryModelError: @(%s): Query of type %s must have @model \
                  attribute"
                 (string_of_loc loc)
                 (QueryFormatter.string_of_query_type typ))));
    let return_type =
      match return_type with
      | Some _ | None -> (
          match typ with
          | Query.FindMany ->
              let _, model_id = Option.value_exn model in
              Composite (List (CustomType model_id))
          | Query.FindUnique | Query.Create | Query.Update | Query.Delete ->
              let _, model_id = Option.value_exn model in
              Scalar (CustomType model_id)
          | Query.Custom -> (
              match return_type with
              | Some typ -> typ
              | None ->
                  failwith
                    (Fmt.str
                       "QueryReturnTypeError: @(%s): Custom query must have \
                        return type"
                       (string_of_loc loc))))
    in
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error loc id;
    let declaration_value =
      GlobalEnvironment.QueryValue
        { typ; return_type; body; model; permissions; route }
    in
    GlobalEnvironment.allocate global_env ~key:id ~data:declaration_value
end

module XRAEnvironment = struct
  type scope = (string, string) Hashtbl.t
  type t = scope list ref

  let create_scope () = Hashtbl.create ~size:17 (module String)
  let create_env () = ref [ create_scope () ]

  let allocate env loc ~key ~data =
    match !env with
    | [] -> ()
    | scope :: _ ->
        if not (Hashtbl.mem scope key) then Hashtbl.add_exn scope ~key ~data
        else raise_multi_definitions_error loc key

  let rec lookup env loc id =
    match !env with
    | [] -> raise_undefined_error loc "variable" id
    | scope :: env ->
        if Hashtbl.mem scope id then Hashtbl.find_exn scope id
        else lookup (ref env) loc id

  let rec contains env id =
    match !env with
    | [] -> false
    | scope :: env ->
        if Hashtbl.mem scope id then true else contains (ref env) id

  let shrink env = match !env with [] -> () | _ :: tail -> env := tail
  let extend env = create_scope () :: !env |> ignore
end

module EnvironmentManager = struct
  let allocate (global_env : GlobalEnvironment.t) loc id declaration_value =
    if GlobalEnvironment.contains global_env ~key:id then
      raise_multi_definitions_error loc id;
    GlobalEnvironment.allocate global_env ~key:id ~data:declaration_value

  let populate global_env (_, declarations) =
    let populate_declaration global_env declaration =
      match declaration with
      | Model model -> ModelEnvironment.allocate global_env model
      | Query query -> QueryEnvironment.allocate global_env query
      | Component (loc, id, component_type, args, _) ->
          allocate global_env loc id (ComponentValue { component_type; args })
      | Page (loc, id, _, _, _) -> allocate global_env loc id PageValue
      | Type typ -> TypeEnvironment.allocate global_env typ
    in
    List.iter
      ~f:(fun declaration -> populate_declaration global_env declaration)
      declarations
end
