open Core
open Ast.Pprinter
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
         if not (GlobalEnvironment.check_type declaration ComponentType) then
           raise_type_error loc "Component" id
             (GlobalEnvironment.infer_type declaration
             |> string_of_declaration_type));
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

let check_component global_env xra_env typ args body =
  let check_query loc id expected_type =
    if not (GlobalEnvironment.contains global_env ~key:id) then
      raise_undefined_error loc "query" id;

    let declaration_value = GlobalEnvironment.lookup global_env ~key:id in
    if not (GlobalEnvironment.check_type declaration_value QueryType) then
      raise_type_error loc "Query" id
        (GlobalEnvironment.infer_type declaration_value
        |> string_of_declaration_type);

    let query_value = GlobalEnvironment.get_query_value declaration_value in
    if not (phys_equal query_value.typ expected_type) then
      raise_type_error loc
        (Fmt.str "%sQuery" (QueryPrinter.string_of_query_type expected_type))
        id
        (Fmt.str "%sQuery" (QueryPrinter.string_of_query_type query_value.typ))
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
    | Reference ->
        raise_bad_argument_type_error loc (string_of_scalar_type Reference)
    | CustomType typ ->
        if not (GlobalEnvironment.contains global_env ~key:typ) then
          raise_undefined_error loc "model" typ;

        let declaration_value = GlobalEnvironment.lookup global_env ~key:typ in
        if not (GlobalEnvironment.check_type declaration_value ModelType) then
          raise_type_error loc "Model" typ
            (GlobalEnvironment.infer_type declaration_value
            |> string_of_declaration_type)
        else XRAEnvironment.allocate xra_env loc ~key:id ~data:typ
  in

  (match args with Some args -> List.iter ~f:check_arg args | None -> ());

  let check_component_body body =
    match body with
    | Component.GeneralBody body -> check_general_body global_env xra_env body
    | Component.FetchBody (on_error, on_loading, on_success) ->
        check_expressions global_env xra_env on_error;
        check_expressions global_env xra_env on_loading;
        check_expressions global_env xra_env on_success
    (* TODO: check action component body *)
    | Component.CreateBody (_, _) | Component.UpdateBody (_, _) -> ()
    | Component.DeleteBody _ -> ()
  in

  check_component_body body
