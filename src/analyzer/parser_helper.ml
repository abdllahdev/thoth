open Ast
open Error

let parse_field_type (field_type : string) : scalar_type =
  match field_type with
  | "String" -> String
  | "Int" -> Int
  | "Boolean" -> Boolean
  | "Bytes" -> Bytes
  | "Json" -> Json
  | "DateTime" -> DateTime
  | _ -> CustomType field_type

let parse_query_arg (loc : loc) (arg : string) (fields : string list) :
    Query.arg =
  match arg with
  | "filter" -> Query.Filter (loc, fields)
  | "where" -> Query.Where (loc, List.hd fields)
  | "data" -> Query.Data (loc, fields)
  | _ ->
      raise
        (NameError
           (Fmt.str "NameError@(%s): Undefined query argument '%s'"
              (Pprinter.string_of_loc loc)
              arg))

let parse_query_type (loc : loc) (typ : string) : Query.typ =
  match typ with
  | "findAll" -> Query.FindAll
  | "findUnique" -> Query.FindUnique
  | "create" -> Query.Create
  | "update" -> Query.Update
  | "delete" -> Query.Delete
  | _ ->
      raise
        (NameError
           (Fmt.str "NameError@(%s): Undefined query type '%s'"
              (Pprinter.string_of_loc loc)
              typ))

let parse_query_permissions (loc : loc) (permissions : string list) :
    Query.permission list =
  List.map
    (fun permission ->
      match permission with
      | "isAuth" -> (loc, "isAuth")
      | "owns" -> (loc, "owns")
      | _ ->
          raise
            (NameError
               (Fmt.str "NameError@(%s): Undefined query permission '%s'"
                  (Pprinter.string_of_loc loc)
                  permission)))
    permissions
