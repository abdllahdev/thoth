open Core
open Ast.Ast_types
open Ast.Formatter
open Ast.Helper
open Error_handler.Handler
open Environment

let rec check_expressions global_env xra_env expressions =
  let check_element loc id attributes children =
    let is_component id =
      let first_char = String.get id 0 in
      Char.is_uppercase first_char
    in

    (if is_component id then
     if not (GlobalEnvironment.contains global_env ~key:id) then
       raise_undefined_error loc "component" id
     else
       let declaration = GlobalEnvironment.lookup global_env ~key:id in
       if not (GlobalEnvironment.check_type declaration ComponentDeclaration)
       then
         raise_declaration_type_error loc ComponentDeclaration id
           (GlobalEnvironment.infer_type declaration));

    let check_component_args attributes =
      if is_component id then
        let component =
          GlobalEnvironment.lookup global_env ~key:id
          |> GlobalEnvironment.get_component_value
        in

        let args = component.args in
        match args with
        | Some args ->
            (* Check passed arguments types *)
            let attributes_ht = Hashtbl.create ~size:17 (module String) in
            let attribute_ids =
              List.map attributes ~f:(fun attribute ->
                  (match attribute with
                  | XRA.Attribute (_, id, value) ->
                      if Hashtbl.mem attributes_ht id then
                        raise_multi_definitions_error loc id;
                      Hashtbl.add_exn attributes_ht ~key:id ~data:value;
                      Some id
                  | _ -> None)
                  |> Option.value_exn)
            in

            let arg_ids = List.map args ~f:(fun (_, id, _) -> id) in

            List.iter attribute_ids ~f:(fun attribute_id ->
                if not (List.mem arg_ids attribute_id ~equal:String.equal) then
                  raise_unexpected_argument_error loc attribute_id ~id);

            List.iter args ~f:(fun arg ->
                let _, arg_id, arg_typ = arg in
                if not (Hashtbl.mem attributes_ht arg_id) then
                  raise_required_argument_error loc arg_id arg_typ id;

                let attribute_value = Hashtbl.find_exn attributes_ht arg_id in
                match attribute_value with
                | XRA.VariableExpression (loc, id) ->
                    if not (XRAEnvironment.contains xra_env id) then
                      raise_undefined_error loc "variable" id;

                    let id_type = XRAEnvironment.lookup xra_env loc id in

                    if
                      not
                        (String.equal (string_of_type id_type)
                           (string_of_type arg_typ))
                    then
                      raise_type_error ~id loc arg_typ ~received_value:id
                        ~received_type:id_type
                | XRA.DotExpression (loc, id, expanded_id) -> (
                    if not (XRAEnvironment.contains xra_env id) then
                      raise_undefined_error loc "variable" id;

                    let id_type = XRAEnvironment.lookup xra_env loc id in

                    let custom_type = get_custom_type id_type in
                    match custom_type with
                    | Some custom_type ->
                        let model_value =
                          GlobalEnvironment.lookup global_env ~key:custom_type
                        in

                        if
                          not
                            (GlobalEnvironment.check_type model_value
                               ModelDeclaration)
                        then raise_dot_operator_error loc expanded_id id id_type
                        else
                          let model_table =
                            GlobalEnvironment.get_model_value model_value
                          in

                          if
                            not
                              (LocalEnvironment.contains model_table
                                 ~key:expanded_id)
                          then
                            raise_dot_operator_error loc expanded_id id id_type;

                          let expanded_id_type =
                            (LocalEnvironment.lookup model_table
                               ~key:expanded_id)
                              .typ
                          in

                          if
                            not
                              (String.equal
                                 (string_of_type expanded_id_type)
                                 (string_of_type arg_typ))
                          then
                            raise_type_error loc arg_typ
                              ~received_value:(Fmt.str "%s.%s" id expanded_id)
                              ~received_type:expanded_id_type
                    | None ->
                        raise_dot_operator_error loc expanded_id id id_type)
                | XRA.Literal literal -> (
                    match literal with
                    | BooleanLiteral (loc, value) ->
                        if
                          not
                            (String.equal
                               (string_of_type (Scalar Boolean))
                               (string_of_type arg_typ))
                        then
                          raise_type_error ~id loc arg_typ
                            ~received_value:(string_of_bool value)
                            ~received_type:(Scalar Boolean)
                    | StringLiteral (loc, value) ->
                        if
                          not
                            (String.equal
                               (string_of_type (Scalar String))
                               (string_of_type arg_typ))
                        then
                          raise_type_error ~id loc arg_typ ~received_value:value
                            ~received_type:(Scalar String)
                    | IntLiteral (loc, value) ->
                        if
                          not
                            (String.equal
                               (string_of_type (Scalar Int))
                               (string_of_type arg_typ))
                        then
                          raise_type_error ~id loc arg_typ
                            ~received_value:(string_of_int value)
                            ~received_type:(Scalar Int))
                | _ -> ())
        | None -> ()
    in

    (match attributes with
    | Some attributes ->
        check_component_args attributes;
        check_expressions global_env xra_env attributes
    | None -> check_component_args []);

    match children with
    | Some children -> check_expressions global_env xra_env children
    | None -> ()
  in

  let rec check_expression expression =
    match expression with
    | XRA.VariableExpression (loc, id) ->
        if not (String.equal id "LoggedInUser") then
          if not (XRAEnvironment.contains xra_env id) then
            raise_undefined_error loc "variable" id
    | XRA.DotExpression (loc, id, expanded_id) ->
        if not (String.equal id "LoggedInUser") then (
          if not (XRAEnvironment.contains xra_env id) then
            raise_undefined_error loc "variable" id;

          let id_type = XRAEnvironment.lookup xra_env loc id in

          let custom_type = get_custom_type id_type in
          match custom_type with
          | Some custom_type ->
              let model_value =
                GlobalEnvironment.lookup global_env ~key:custom_type
              in

              if not (GlobalEnvironment.check_type model_value ModelDeclaration)
              then raise_dot_operator_error loc expanded_id id id_type
              else
                let model_table =
                  GlobalEnvironment.get_model_value model_value
                in

                if not (LocalEnvironment.contains model_table ~key:expanded_id)
                then raise_dot_operator_error loc expanded_id id id_type
          | None -> raise_dot_operator_error loc expanded_id id id_type)
    | XRA.Element (loc, id, attributes, children) ->
        check_element loc id attributes children
    | XRA.Attribute (_, _, expression) -> check_expression expression
    | XRA.LiteralConditionalExpression (_, expression)
    | XRA.NotConditionalExpression (_, expression) ->
        check_expression expression
    | XRA.EqConditionalExpression (_, left_expression, right_expression)
    | XRA.NotEqConditionalExpression (_, left_expression, right_expression)
    | XRA.LtConditionalExpression (_, left_expression, right_expression)
    | XRA.GtConditionalExpression (_, left_expression, right_expression)
    | XRA.LtOrEqConditionalExpression (_, left_expression, right_expression)
    | XRA.GtOrEqConditionalExpression (_, left_expression, right_expression) ->
        check_expression left_expression;
        check_expression right_expression
    | XRA.IfThenElseExpression (_, condition, then_block, else_block) ->
        check_expression condition;
        check_expression then_block;
        check_expression else_block
    | XRA.IfThenExpression (_, condition, then_block) ->
        check_expression condition;
        check_expression then_block
    | XRA.ForExpression (loc, id, lst, for_block) ->
        check_expression lst;

        let lst_loc, lst_id, lst_typ =
          (match lst with
          | XRA.VariableExpression (loc, id) ->
              let typ = XRAEnvironment.lookup xra_env loc id in
              Some (loc, id, typ)
          | _ -> None)
          |> Option.value_exn
        in

        (match lst_typ with
        | Composite composite_type -> (
            match composite_type with
            | List _ -> ()
            | _ ->
                raise_type_error lst_loc (Composite (List (CustomType "List")))
                  ~received_value:lst_id ~received_type:lst_typ)
        | Scalar _ ->
            raise_type_error lst_loc (Composite (List (CustomType "List")))
              ~received_value:lst_id ~received_type:lst_typ);

        XRAEnvironment.extend xra_env;
        XRAEnvironment.allocate xra_env loc ~key:id
          ~data:(Scalar (get_scalar_type lst_typ));
        check_expression for_block;
        XRAEnvironment.shrink xra_env
    | XRA.Fragment (_, children) -> (
        match children with
        | Some children -> check_expressions global_env xra_env children
        | None -> ())
    | _ -> ()
  in

  match expressions with
  | [] -> ()
  | expression :: expressions ->
      check_expression expression;
      check_expressions global_env xra_env expressions

(* TODO: let declarations *)
(* let check_let_expression global_env xra_env let_expression =
     match let_expression with
     | XRA.LetExpression (loc, id, expression) ->
         XRAEnvironment.allocate xra_env loc ~key:id ~data:"let";
         check_expressions global_env xra_env [ expression ]
     | _ -> ()

   let rec check_let_expressions global_env xra_env let_expressions =
     match let_expressions with
     | Some let_expressions -> (
         match let_expressions with
         | [] -> ()
         | let_expression :: let_expressions ->
             check_let_expression global_env xra_env let_expression;
             check_let_expressions global_env xra_env (Some let_expressions))
     | None -> () *)

let rec check_render_expression global_env xra_env elements =
  match elements with
  | [] -> ()
  | element :: elements ->
      check_expressions global_env xra_env [ element ];
      check_render_expression global_env xra_env elements

let check_general_body global_env xra_env body =
  let _, render_expression = body in
  (* check_let_expressions global_env xra_env let_expressions; *)
  check_render_expression global_env xra_env render_expression

let check_page = check_general_body

let check_component global_env xra_env app_declaration loc id typ args body =
  let check_query loc id expected_query_type =
    if not (GlobalEnvironment.contains global_env ~key:id) then
      raise_undefined_error loc "query" id;
    let declaration_value = GlobalEnvironment.lookup global_env ~key:id in
    if not (GlobalEnvironment.check_type declaration_value QueryDeclaration)
    then
      raise_declaration_type_error loc QueryDeclaration id
        (GlobalEnvironment.infer_type declaration_value);
    let query_value = GlobalEnvironment.get_query_value declaration_value in
    if
      not
        (String.equal
           (QueryFormatter.string_of_query_type query_value.typ)
           (QueryFormatter.string_of_query_type expected_query_type))
    then raise_query_type_error loc expected_query_type id query_value.typ
  in
  let check_args loc args component_type =
    let args = match args with Some args -> args | None -> [] in
    match component_type with
    | Component.General ->
        List.iter args ~f:(fun arg ->
            let loc, id, typ = arg in
            let scalar_typ = get_scalar_type typ in
            (match scalar_typ with
            | String | Int | Boolean | DateTime -> ()
            | Reference | Nil | Assoc ->
                raise_argument_type_error loc
                  (ComponentFormatter.string_of_component_type component_type)
                  (Scalar Reference)
            | CustomType custom_type ->
                if not (GlobalEnvironment.contains global_env ~key:custom_type)
                then raise_undefined_error loc "type" custom_type;

                let declaration_value =
                  GlobalEnvironment.lookup global_env ~key:custom_type
                in
                if
                  not
                    (GlobalEnvironment.check_type declaration_value
                       ModelDeclaration)
                then
                  raise_declaration_type_error loc ModelDeclaration custom_type
                    (GlobalEnvironment.infer_type declaration_value));
            XRAEnvironment.allocate xra_env loc ~key:id ~data:typ)
    | Component.Delete | Component.Update -> (
        let args_count = List.length args in
        if args_count > 1 || args_count < 1 then
          raise_argument_number_error loc 1 args_count id
        else
          let loc, _, typ = List.hd_exn args in
          let scalar_type = get_scalar_type typ in
          match scalar_type with
          | Int -> ()
          | _ as received_type ->
              raise_argument_type_error loc
                (ComponentFormatter.string_of_component_type component_type)
                (Scalar received_type))
    | _ -> ()
  in
  let check_form_fields ?query_id form_fields =
    let declaration_id, declaration_type, required_fields =
      match typ with
      | Component.Create | Component.Update ->
          ( Option.value_exn query_id,
            QueryDeclaration,
            (GlobalEnvironment.lookup global_env
               ~key:(Option.value_exn query_id)
            |> GlobalEnvironment.get_query_value)
              .body
            |> List.find_map ~f:(function
                 | Query.Data (_, fields) -> Some fields
                 | _ -> None)
            |> Option.value_exn
            |> List.fold ~init:[] ~f:(fun lst field ->
                   let field, _ = field in
                   lst @ [ field ]) )
      | Component.SignupForm -> (
          let auth_config = get_auth_config app_declaration in
          match auth_config with
          | Some { user_model; _ } ->
              let required_fields =
                GlobalEnvironment.lookup global_env ~key:user_model
                |> GlobalEnvironment.get_model_value
                |> Hashtbl.filter
                     ~f:(fun (data : GlobalEnvironment.field_value) ->
                       if
                         not
                           (LocalEnvironment.contains data.field_attrs_table
                              ~key:"@default"
                           || LocalEnvironment.contains data.field_attrs_table
                                ~key:"@id"
                           || LocalEnvironment.contains data.field_attrs_table
                                ~key:"@updatedAt"
                           || is_custom_type data.typ)
                       then true
                       else false)
                |> Hashtbl.keys
              in
              (user_model, ModelDeclaration, required_fields)
          | None -> raise_requires_configuration loc typ)
      | Component.LoginForm -> (
          let auth_config = get_auth_config app_declaration in
          match auth_config with
          | Some { username_field; password_field; _ } ->
              let _, id, _ = app_declaration in
              (id, AppDeclaration, [ username_field; password_field ])
          | None -> raise_requires_configuration loc typ)
      | _ -> failwith "CompilationError"
    in
    let check_required_fields required_fields =
      let implemented_form_field =
        List.map form_fields ~f:(fun field ->
            let _, id, _, _, _ = field in
            id)
      in
      List.iter required_fields ~f:(fun required_field ->
          if
            not
              (List.mem implemented_form_field required_field
                 ~equal:String.equal)
          then raise_required_form_input_error loc required_field id)
    in
    check_required_fields required_fields;
    List.iter form_fields ~f:(fun field ->
        let loc, id, _, _, _ = field in
        try
          List.find_exn required_fields ~f:(fun field -> String.equal id field)
          |> ignore
        with Not_found_s _ | Caml.Not_found ->
          raise_undefined_error loc "field" id ~declaration_type ~declaration_id)
  in
  let check_component_body body =
    match body with
    | Component.GeneralBody body -> check_general_body global_env xra_env body
    | Component.FindBody
        ( ((query_loc, query_id), (variable_loc, variable)),
          on_error,
          on_loading,
          on_success ) ->
        let expected_query_type =
          if
            String.equal
              (ComponentFormatter.string_of_component_type typ)
              (ComponentFormatter.string_of_component_type Component.FindMany)
          then Query.FindMany
          else Query.FindUnique
        in
        check_query query_loc query_id expected_query_type;
        let query_return_type =
          (GlobalEnvironment.lookup global_env ~key:query_id
          |> GlobalEnvironment.get_query_value)
            .return_type
        in
        XRAEnvironment.allocate xra_env variable_loc ~key:variable
          ~data:query_return_type;
        check_render_expression global_env xra_env on_error;
        check_render_expression global_env xra_env on_loading;
        check_render_expression global_env xra_env on_success
    | Component.CreateBody ((query_loc, query_id), _, form_fields, _) ->
        check_query query_loc query_id Query.Create;
        check_form_fields form_fields ~query_id
    | Component.UpdateBody ((query_loc, query_id), _, form_fields, _) ->
        check_args loc args Component.Update;
        check_query query_loc query_id Query.Update;
        check_form_fields form_fields ~query_id
    | Component.DeleteBody ((query_loc, query_id), _) ->
        check_args loc args Component.Delete;
        check_query query_loc query_id Query.Delete
    | Component.SignupFormBody (_, form_fields, _)
    | Component.LoginFormBody (_, form_fields, _) ->
        check_form_fields form_fields
    | _ -> ()
  in
  check_component_body body
