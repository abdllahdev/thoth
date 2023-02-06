open Core
open Ast.Ast_types
open Error_handler.Handler
open Environment
open Helper

let rec check_expressions global_env xra_env expressions =
  let check_element loc id attributes children =
    (let is_component id =
       let first_char = String.get id 0 in
       Char.is_uppercase first_char
     in
     if is_component id then
       if not (GlobalEnvironment.contains global_env ~key:id) then
         raise_undefined_error loc "component" id
       else
         let declaration = GlobalEnvironment.lookup global_env ~key:id in
         if not (GlobalEnvironment.check_type declaration ComponentDeclaration)
         then
           raise_declaration_type_error loc ComponentDeclaration id
             (GlobalEnvironment.infer_type declaration));
    (match attributes with
    | Some attributes -> check_expressions global_env xra_env attributes
    | None -> ());
    match children with
    | Some children -> check_expressions global_env xra_env children
    | None -> ()
  in

  (* TODO: check dot expressions *)
  let check_dot_expression (XRA.Dot (_, _, _)) = () in

  let rec check_expression expression =
    match expression with
    | XRA.Variable (loc, id) -> XRAEnvironment.lookup xra_env loc id
    | XRA.DotExpression dot_expression -> check_dot_expression dot_expression
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
    | XRA.IfThenElseStatement (_, condition, then_block, else_block) ->
        check_expression condition;
        check_expression then_block;
        check_expression else_block
    | XRA.IfThenStatement (_, condition, then_block) ->
        check_expression condition;
        check_expression then_block
    | XRA.ForLoopStatement (loc, id, lst, for_block) ->
        check_expression lst;
        XRAEnvironment.extend xra_env;
        XRAEnvironment.allocate xra_env loc ~key:id ~data:"var";
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

let check_let_expression global_env xra_env let_expression =
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
  | None -> ()

let rec check_render_expression global_env xra_env elements =
  match elements with
  | [] -> ()
  | element :: elements ->
      check_expressions global_env xra_env [ element ];
      check_render_expression global_env xra_env elements

let check_general_body global_env xra_env body =
  let let_expressions, render_expression = body in
  check_let_expressions global_env xra_env let_expressions;
  check_render_expression global_env xra_env render_expression

let check_page = check_general_body

let check_component global_env xra_env typ args body =
  let check_query loc id expected_query_type =
    if not (GlobalEnvironment.contains global_env ~key:id) then
      raise_undefined_error loc "query" id;

    let declaration_value = GlobalEnvironment.lookup global_env ~key:id in
    if not (GlobalEnvironment.check_type declaration_value QueryDeclaration)
    then
      raise_declaration_type_error loc QueryDeclaration id
        (GlobalEnvironment.infer_type declaration_value);

    let query_value = GlobalEnvironment.get_query_value declaration_value in
    if not (phys_equal query_value.typ expected_query_type) then
      raise_query_type_error loc expected_query_type id query_value.typ
  in

  (match typ with
  | Component.General -> ()
  | Component.FetchMany (loc, id, variable) ->
      check_query loc id Query.FindMany;
      XRAEnvironment.allocate xra_env loc ~key:variable ~data:"someType"
  | Component.FetchOne (loc, id, variable) ->
      check_query loc id Query.FindUnique;
      XRAEnvironment.allocate xra_env loc ~key:variable ~data:"someType"
  | Component.Create (loc, id) -> check_query loc id Query.Create
  | Component.Update (loc, id) -> check_query loc id Query.Update
  | Component.Delete (loc, id) -> check_query loc id Query.Update);

  let check_arg arg =
    let loc, id, typ = arg in
    let typ = get_scalar_type typ in
    match typ with
    | String | Int | Boolean | DateTime -> ()
    | Reference | Void -> raise_bad_argument_type_error loc (Scalar Reference)
    | CustomType typ ->
        if not (GlobalEnvironment.contains global_env ~key:typ) then
          raise_undefined_error loc "type" typ;

        let declaration_value = GlobalEnvironment.lookup global_env ~key:typ in
        if not (GlobalEnvironment.check_type declaration_value ModelDeclaration)
        then
          raise_declaration_type_error loc ModelDeclaration typ
            (GlobalEnvironment.infer_type declaration_value)
        else XRAEnvironment.allocate xra_env loc ~key:id ~data:typ
  in

  (match args with Some args -> List.iter ~f:check_arg args | None -> ());

  let rec check_form_fields form_fields =
    let query_id =
      (match typ with
      | Component.Create (_, id) | Component.Update (_, id) -> Some id
      | _ -> None)
      |> Option.value_exn
    in
    let query =
      GlobalEnvironment.lookup global_env ~key:query_id
      |> GlobalEnvironment.get_query_value
    in

    match form_fields with
    | [] -> ()
    | form_field :: form_fields ->
        let loc, id, input = form_field in

        let rec check_args args =
          match args with
          | [] -> ()
          | arg :: args ->
              (match arg with
              | Query.Data (_, fields) -> (
                  try
                    List.find_exn ~f:(fun field -> String.equal id field) fields
                    |> ignore
                  with Not_found_s _ ->
                    raise_undefined_error loc "field" id
                      ~declaration_type:QueryDeclaration
                      ~declaration_id:query_id)
              | _ -> ());
              check_args args
        in

        check_args query.args;

        let loc, element_id =
          (match input with
          | XRA.Element (loc, id, _, _) -> Some (loc, id)
          | _ -> None)
          |> Option.value_exn
        in

        if
          not
            (String.equal element_id "input"
            || String.equal element_id "select"
            || String.equal element_id "option"
            || String.equal element_id "textarea")
        then
          raise_element_type_error loc "input, select, option, or textarea"
            element_id;

        check_form_fields form_fields
  in

  let check_form_button form_button =
    let loc, element_id =
      (match form_button with
      | XRA.Element (loc, id, _, _) -> Some (loc, id)
      | _ -> None)
      |> Option.value_exn
    in

    if not (String.equal element_id "button") then
      raise_element_type_error loc "button" element_id
  in

  let check_component_body body =
    match body with
    | Component.GeneralBody body -> check_general_body global_env xra_env body
    | Component.FetchBody (on_error, on_loading, on_success) ->
        check_render_expression global_env xra_env on_error;
        check_render_expression global_env xra_env on_loading;
        check_render_expression global_env xra_env on_success
    | Component.CreateBody (form_fields, form_button)
    | Component.UpdateBody (form_fields, form_button) ->
        check_form_fields form_fields;
        check_form_button form_button
    | Component.DeleteBody form_button -> check_form_button form_button
  in

  check_component_body body
