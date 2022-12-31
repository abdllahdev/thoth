(* First step in the type checker is to:
   1- Populate the symbol table
   2- Check for undefined and unreachable variable
   3- Do type checking *)

open Ast
open Symbol_table

(* TODO:
   1- Populate the SymbolTable.
   2- Traverse the AST.
   Model errors to check:
   4- Check if there is any *)

module TypeChecker = struct
  (* let check_declaration (declaration : declaration)
         (table : 'a GlobalSymbolTable.t) : unit =
       match declaration with Model (_, id, fields) -> failwith "unimplemented"

     let rec semantic_check (Ast declarations) (table : 'a GlobalSymbolTable.t) :
         unit =
       match declarations with
       | [] -> ()
       | declaration :: declarations ->
           check_declaration declaration table;
           semantic_check (Ast declarations) table *)

  let run (Ast declarations) : unit =
    let table = GlobalSymbolTable.create () in
    SymbolTableManager.populate (Ast declarations) table
  (* semantic_check (Ast declarations) table *)
end
