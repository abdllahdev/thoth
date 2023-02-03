open Core
open Ast
open Ast.Ast_types
open Error_handler.Handler
open Environment

let get_custom_scalar_type scalar_type =
  match scalar_type with CustomType str -> Some str | _ -> None

let get_custom_type typ =
  match typ with
  | Scalar scalar_type -> get_custom_scalar_type scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> get_custom_scalar_type scalar_type
      | Optional scalar_type -> get_custom_scalar_type scalar_type
      | OptionalList scalar_type -> get_custom_scalar_type scalar_type)

let get_scalar_type (typ : typ) : scalar_type =
  match typ with
  | Scalar scalar_type -> scalar_type
  | Composite composite_type -> (
      match composite_type with
      | List scalar_type -> scalar_type
      | Optional scalar_type -> scalar_type
      | OptionalList scalar_type -> scalar_type)

let is_custom_type scalar_type =
  match scalar_type with CustomType _ -> true | _ -> false

let check_attribute_argument id literal expected_type =
  match expected_type with
  | String -> (
      match literal with
      | BooleanLiteral (loc, boolean) ->
          raise_type_error loc "Reference" (string_of_bool boolean) "Boolean"
            ~id
      | IntLiteral (loc, number) ->
          raise_type_error loc "Reference" (string_of_int number) "Int" ~id
      | _ -> ())
  | Int -> (
      match literal with
      | BooleanLiteral (loc, boolean) ->
          raise_type_error loc "Reference" (string_of_bool boolean) "Boolean"
            ~id
      | StringLiteral (loc, str) ->
          raise_type_error loc "Reference" str "Int" ~id
      | _ -> ())
  | Boolean -> (
      match literal with
      | StringLiteral (loc, str) ->
          raise_type_error loc "Reference" str "Int" ~id
      | IntLiteral (loc, number) ->
          raise_type_error loc "Reference" (string_of_int number) "Int" ~id
      | _ -> ())
  | Reference -> (
      match literal with
      | StringLiteral (loc, str) ->
          raise_type_error loc "Reference" str "Int" ~id
      | IntLiteral (loc, number) ->
          raise_type_error loc "Reference" (string_of_int number) "Int" ~id
      | BooleanLiteral (loc, boolean) ->
          raise_type_error loc "Reference" (string_of_bool boolean) "Boolean"
            ~id)
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
      | DateTime -> ()
      | Int -> ()
      | String -> ()
      | Boolean -> ()
      | _ -> raise_attribute_error loc field_type id);

      if args_length > 1 || equal_int args_length 0 then
        raise_argument_number_error loc 1 args_length id
      else
        let arg = List.hd_exn args in
        match arg with
        | Model.AttrArgRef (loc, ref) ->
            raise_type_error loc
              (Pprinter.string_of_scalar_type field_type)
              ref "Reference" ~id
        | Model.AttrArgNow loc -> (
            match field_type with
            | DateTime -> ()
            | _ ->
                raise_type_error loc
                  (Pprinter.string_of_scalar_type field_type)
                  "now" "DateTime" ~id)
        | Model.AttrArgLiteral literal ->
            check_attribute_argument id literal field_type)
  | "@relation" -> (
      if not (is_custom_type field_type) then
        raise_attribute_error loc field_type id;

      if not (equal args_length 2) then
        raise_argument_number_error loc 2 args_length id;
      (let relation_field = List.nth_exn args 0 in
       match relation_field with
       | Model.AttrArgRef (loc, field) ->
           if not (LocalEnvironment.contains model_env ~key:field) then
             raise_undefined_error loc "field" field ~declaration_id:model_id
               ~declaration_type:"model"
       | Model.AttrArgNow loc ->
           raise_type_error loc "Reference" "now" "DateTime" ~id
       | Model.AttrArgLiteral literal ->
           check_attribute_argument id literal Reference);
      let relation_ref = List.nth_exn args 1 in
      match relation_ref with
      | Model.AttrArgRef (loc, ref) ->
          let other_model_id =
            (LocalEnvironment.lookup model_env ~key:field_id).typ
            |> get_custom_type |> Option.value_exn
          in

          let other_model_table =
            GlobalEnvironment.lookup global_env ~key:other_model_id
            |> GlobalEnvironment.get_model_value
          in

          if not (LocalEnvironment.contains other_model_table ~key:ref) then
            raise_undefined_error loc "field" ref ~declaration_id:other_model_id
              ~declaration_type:"model";

          let field_attrs =
            (LocalEnvironment.lookup other_model_table ~key:ref)
              .field_attrs_table
          in

          if
            (not (LocalEnvironment.contains field_attrs ~key:"@unique"))
            && not (LocalEnvironment.contains field_attrs ~key:"@id")
          then raise_type_error loc "UniqueField" ref "NonUniqueField" ~id
      | Model.AttrArgLiteral literal ->
          check_attribute_argument id literal Reference
      | Model.AttrArgNow loc ->
          raise_type_error loc "Reference" "now" "DateTime" ~id)
  | _ -> raise_undefined_error loc "attribute" id

let rec check_field_attrs global_env model_env model_id field_id field_attrs =
  match field_attrs with
  | [] -> ()
  | field_attr :: field_attrs ->
      check_field_attr global_env model_env model_id field_id field_attr;
      check_field_attrs global_env model_env model_id field_id field_attrs

let check_field_type global_env model_id field_id field_type loc : unit =
  let custom_type = get_custom_type field_type in
  match custom_type with
  | Some custom_type ->
      if not (GlobalEnvironment.contains global_env ~key:custom_type) then
        raise_undefined_error loc "model" custom_type;

      let declaration_value =
        GlobalEnvironment.lookup global_env ~key:custom_type
      in
      if not (GlobalEnvironment.check_type declaration_value ModelType) then
        raise_type_error loc "Model" custom_type
          (Pprinter.string_of_declaration_type
             (GlobalEnvironment.infer_type declaration_value))
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
      check_field_type global_env model_id id field_type loc;
      check_field_attrs global_env model_env model_id id field_attrs

let rec check_fields global_env model_env model_id fields =
  match fields with
  | [] -> ()
  | field :: fields ->
      check_field global_env model_env model_id field;
      check_fields global_env model_env model_id fields

let check_model global_env model_table (_, model_id, fields) =
  check_fields global_env model_table model_id fields
