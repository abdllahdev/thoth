open Core
open Ast.Ast_types
open Ast.Formatter
open Ast.Helper
open Error_handler.Handler
open Environment

let is_custom_type scalar_type =
  match scalar_type with CustomType _ -> true | _ -> false

let check_attribute_argument id literal expected_type =
  match expected_type with
  | String -> (
      match literal with
      | BooleanLiteral (loc, boolean) ->
          raise_type_error loc (Scalar expected_type)
            ~received_value:(string_of_bool boolean)
            ~received_type:(Scalar Boolean) ~id
      | IntLiteral (loc, number) ->
          raise_type_error loc (Scalar expected_type)
            ~received_value:(string_of_int number) ~received_type:(Scalar Int)
            ~id
      | _ -> ())
  | Int -> (
      match literal with
      | BooleanLiteral (loc, boolean) ->
          raise_type_error loc (Scalar expected_type)
            ~received_value:(string_of_bool boolean)
            ~received_type:(Scalar Boolean) ~id
      | StringLiteral (loc, str) ->
          raise_type_error loc (Scalar expected_type) ~received_value:str
            ~received_type:(Scalar String) ~id
      | _ -> ())
  | Boolean -> (
      match literal with
      | StringLiteral (loc, str) ->
          raise_type_error loc (Scalar expected_type) ~received_value:str
            ~received_type:(Scalar String) ~id
      | IntLiteral (loc, number) ->
          raise_type_error loc (Scalar expected_type)
            ~received_value:(string_of_int number) ~received_type:(Scalar Int)
            ~id
      | _ -> ())
  | Reference -> (
      match literal with
      | StringLiteral (loc, str) ->
          raise_type_error loc (Scalar expected_type) ~received_value:str
            ~received_type:(Scalar String) ~id
      | IntLiteral (loc, number) ->
          raise_type_error loc (Scalar expected_type)
            ~received_value:(string_of_int number) ~received_type:(Scalar Int)
            ~id
      | BooleanLiteral (loc, boolean) ->
          raise_type_error loc (Scalar expected_type)
            ~received_value:(string_of_bool boolean)
            ~received_type:(Scalar Boolean) ~id)
  | _ -> ()

let check_field_attr global_env model_env model_id field_id
    (Model.Attribute (loc, id, args)) =
  let args_length = List.length args in
  let field_info : GlobalEnvironment.field_value =
    LocalEnvironment.lookup model_env ~key:field_id
  in
  let field_type = get_scalar_type field_info.typ in
  match id with
  | "@id" | "@unique" | "@ignore" | "@updatedAt" ->
      if args_length >= 1 then raise_argument_number_error loc 0 args_length id
  | "@default" -> (
      (match field_type with
      | DateTime | Int | String | Boolean -> ()
      | _ -> raise_attribute_error loc (Scalar field_type) id);

      if args_length > 1 || equal_int args_length 0 then
        raise_argument_number_error loc 1 args_length id
      else
        let arg = List.hd_exn args in
        match arg with
        | Model.AttrArgRef (loc, ref) ->
            raise_type_error loc (Scalar field_type) ~received_value:ref
              ~received_type:(Scalar Reference) ~id
        | Model.AttrArgNow loc -> (
            match field_type with
            | DateTime -> ()
            | _ ->
                raise_type_error loc (Scalar field_type) ~received_value:"now"
                  ~received_type:(Scalar DateTime) ~id)
        | Model.AttrArgLiteral literal ->
            check_attribute_argument id literal field_type)
  | "@relation" -> (
      if not (is_custom_type field_type) then
        raise_attribute_error loc (Scalar field_type) id;

      if not (equal args_length 2) then
        raise_argument_number_error loc 2 args_length id;

      let relation_field_arg = List.nth_exn args 0 in
      let relation_ref_arg = List.nth_exn args 1 in

      (match relation_field_arg with
      | Model.AttrArgRef (loc, field) ->
          if not (LocalEnvironment.contains model_env ~key:field) then
            raise_undefined_error loc "field" field ~declaration_id:model_id
              ~declaration_type:ModelDeclaration
      | Model.AttrArgNow loc ->
          raise_type_error loc (Scalar Reference) ~received_value:"now"
            ~received_type:(Scalar DateTime) ~id
      | Model.AttrArgLiteral literal ->
          check_attribute_argument id literal Reference);

      match relation_ref_arg with
      | Model.AttrArgRef (loc, relation_ref_id) ->
          let other_model_id =
            (LocalEnvironment.lookup model_env ~key:field_id).typ
            |> get_custom_type |> Option.value_exn
          in

          let other_model_env =
            GlobalEnvironment.lookup global_env ~key:other_model_id
            |> GlobalEnvironment.get_model_value
          in

          let relation_field_id =
            (match relation_field_arg with
            | Model.AttrArgRef (_, field) -> Some field
            | _ -> None)
            |> Option.value_exn
          in

          let relation_field_type =
            (LocalEnvironment.lookup model_env ~key:relation_field_id).typ
          in

          let relation_ref_field =
            LocalEnvironment.lookup other_model_env ~key:relation_ref_id
          in

          let relation_ref_attrs = relation_ref_field.field_attrs_table in

          let relation_ref_type = relation_ref_field.typ in

          if
            not
              (String.equal
                 (string_of_type relation_ref_type)
                 (string_of_type relation_field_type))
          then
            raise_type_relation_error loc relation_field_id relation_field_type
              relation_ref_id relation_ref_type;

          if
            not (LocalEnvironment.contains other_model_env ~key:relation_ref_id)
          then
            raise_undefined_error loc "field" relation_ref_id
              ~declaration_id:other_model_id ~declaration_type:ModelDeclaration;

          if
            (not (LocalEnvironment.contains relation_ref_attrs ~key:"@unique"))
            && not (LocalEnvironment.contains relation_ref_attrs ~key:"@id")
          then
            raise_unique_field_error loc Model.UniqueField relation_ref_id
              Model.NonUniqueField ~id
      | Model.AttrArgLiteral literal ->
          check_attribute_argument id literal Reference
      | Model.AttrArgNow loc ->
          raise_type_error loc (Scalar Reference) ~received_value:"now"
            ~received_type:(Scalar DateTime) ~id)
  | _ -> raise_undefined_error loc "attribute" id

let rec check_field_attrs global_env model_env model_id field_id field_attrs =
  match field_attrs with
  | [] -> ()
  | field_attr :: field_attrs ->
      check_field_attr global_env model_env model_id field_id field_attr;
      check_field_attrs global_env model_env model_id field_id field_attrs

let check_field_type global_env loc model_id field_id field_type field_attrs :
    unit =
  (* Check if primary field is of type Int *)
  List.iter field_attrs ~f:(fun attr ->
      let (Model.Attribute (loc, id, _)) = attr in
      match id with
      | "@id" ->
          if
            not
              (String.equal
                 (string_of_type field_type)
                 (string_of_type (Scalar Int)))
          then
            raise_type_error loc (Scalar Int) ~received_value:field_id
              ~received_type:field_type ~id:model_id
      | _ -> ());

  (* Check if model exists *)
  let custom_type = get_custom_type field_type in
  match custom_type with
  | Some custom_type ->
      if not (GlobalEnvironment.contains global_env ~key:custom_type) then
        raise_undefined_error loc "model" custom_type;

      let declaration_value =
        GlobalEnvironment.lookup global_env ~key:custom_type
      in
      if not (GlobalEnvironment.check_type declaration_value ModelDeclaration)
      then
        raise_declaration_type_error loc ModelDeclaration custom_type
          (GlobalEnvironment.infer_type declaration_value)
          ~id:field_id;

      let other_model =
        GlobalEnvironment.lookup global_env ~key:custom_type
        |> GlobalEnvironment.get_model_value
      in
      let all_custom_types =
        Hashtbl.fold ~init:[]
          ~f:
            (fun ~key:_ ~(data : GlobalEnvironment.field_value)
                 (acc : string list) ->
            let scalar_type = get_scalar_type data.typ in
            match scalar_type with CustomType str -> acc @ [ str ] | _ -> acc)
          other_model
      in
      if not (List.mem all_custom_types model_id ~equal:equal_string) then
        raise_relation_error loc field_id model_id custom_type
  | None -> ()

let check_field global_env model_env model_id field =
  match field with
  | Model.Field (loc, id, field_type, field_attrs) ->
      check_field_type global_env loc model_id id field_type field_attrs;
      check_field_attrs global_env model_env model_id id field_attrs

let rec check_fields global_env model_env model_id fields =
  match fields with
  | [] -> ()
  | field :: fields ->
      check_field global_env model_env model_id field;
      check_fields global_env model_env model_id fields

let check_model global_env model_table (_, model_id, fields) =
  check_fields global_env model_table model_id fields
