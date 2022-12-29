(* First step in the type checker is to:
   1- Populate the symbol table
   2- Check for undefined and unreachable variable
   3- Do type checking *)

open Ast
open Error

(* open Symbol_table *)

(* TODO:
   1- Populate the SymbolTable.
   2- Traverse the AST.
   Model errors to check:
   4- Check if there is any *)

let check_field_attr (Model.Attribute (_, id, args)) : unit =
  let args_length = List.length args in
  match id with
  | "@id" | "@unqiue" | "@ignore" | "@updatedAt" ->
      if args_length >= 1 then
        raise
          (TypeError
             (Fmt.str
                "TypeError: Expected 0 argument in @default but received %d."
                args_length))
  | "@default" -> (
      if args_length > 1 || args_length == 0 then
        raise
          (TypeError
             (Fmt.str
                "TypeError: Expected 1 argument in @default but received %d."
                args_length))
      else
        let arg = List.hd args in
        match arg with
        | AttrArgFunc (_, _) ->
            raise
              (TypeError
                 "TypeError: Attribute @default doesn't accept functions as \
                  arguments.")
        | _ -> ())
  | "@relation" ->
      if args_length > 3 || args_length < 3 then
        raise
          (TypeError
             (Fmt.str
                "TypeError: Expected 3 argument in @relation but received %d."
                args_length))
  | _ -> raise (NameError (Fmt.str "NameError: Unknown attribute %s." id))
