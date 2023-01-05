open Ast
open Error

let parse_field_type (field_type : string) =
  match field_type with
  | "String" -> String
  | "Int" -> Int
  | "Boolean" -> Boolean
  | "Bytes" -> Bytes
  | "Json" -> Json
  | "DateTime" -> DateTime
  | _ -> CustomType field_type

let parse_query_arg (loc : loc) (arg : string) (fields : string list) =
  match arg with
  | "filter" -> Query.Filter fields
  | "where" -> Query.Where (List.hd fields)
  | "data" -> Query.Data fields
  | _ ->
      raise
        (NameError
           (Fmt.str "NameError@(%s): Undefined query argument %s"
              (Pprinter.string_of_loc loc)
              arg))

let parse_query_type (loc : loc) (typ : string) =
  match typ with
  | "findAll" -> Query.FindAll
  | "findUnique" -> Query.FindUnique
  | "create" -> Query.Create
  | "update" -> Query.Update
  | "delete" -> Query.Delete
  | _ ->
      raise
        (NameError
           (Fmt.str "NameError@(%s): Undefined query type %s"
              (Pprinter.string_of_loc loc)
              typ))
