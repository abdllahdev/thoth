open Core
open Ast
open Ast.Ast_types
open Environment
open Error_handler.Handler

let rec check_expressions global_env xra_env expressions =
  let check_element loc id attributes children =
    (let is_component id =
       let first_char = String.get id 0 in
       Char.is_uppercase first_char
     in
     if is_component id then
       if not (GlobalEnvironment.contains global_env ~key:id) then
         raise_unbound_value_error loc "Component" id
       else
         let declaration = GlobalEnvironment.lookup global_env ~key:id in
         let declaration_type = GlobalEnvironment.infer_type declaration in
         if not (GlobalEnvironment.check_type declaration ComponentType) then
           raise_type_error loc "Component" id
             (Pprinter.string_of_declaration_type declaration_type));
    (match attributes with
    | Some attributes -> check_expressions global_env xra_env attributes
    | None -> ());
    match children with
    | Some children -> check_expressions global_env xra_env children
    | None -> ()
  in

  let rec check_expression expression =
    match expression with
    | XRA.BasicExpression basic_expression -> (
        match basic_expression with
        | Variable (loc, id) -> XRAEnvironment.lookup xra_env loc id
        | _ -> ())
    | XRA.Element (loc, id, attributes, children) ->
        check_element loc id attributes children
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
