open Ast.Ast_types
open Environment

let rec check_expressions global_env xra_env expressions =
  let rec check_expression expression =
    match expression with
    | XRA.BasicExpression basic_expression -> (
        match basic_expression with
        | Variable (loc, id) -> XRAEnvironment.lookup xra_env loc id
        | _ -> ())
    | XRA.Element (_, _, attributes, children) -> (
        (match attributes with
        | Some attributes -> check_expressions global_env xra_env attributes
        | None -> ());
        match children with
        | Some children -> check_expressions global_env xra_env children
        | None -> ())
    | XRA.Attribute (_, _, expression) -> check_expression expression
    | XRA.IfThenElseStatement (_, _, then_block, else_block) ->
        check_expression then_block;
        check_expression else_block
    | XRA.IfThenStatement (_, _, then_block) -> check_expression then_block
    | XRA.ForLoopStatement (loc, id, _, for_block) ->
        XRAEnvironment.extend xra_env;
        XRAEnvironment.allocate xra_env loc ~key:id ~data:"var";
        check_expression for_block;
        XRAEnvironment.shrink xra_env
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

let check_body global_env xra_env body =
  let let_expressions, render_expression = body in
  check_let_expressions global_env xra_env let_expressions;
  check_render_expression global_env xra_env render_expression
