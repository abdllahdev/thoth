type query_specs = { id : string; typ : string }
type service_specs = { name : string; queries : query_specs list }
type server_specs = { services : service_specs list }

val group_queries :
  Ast.Ast_types.query_declaration list ->
  Ast.Ast_types.query_declaration list list

val generate_service_specs :
  Ast.Ast_types.query_declaration list -> service_specs

val generate_server_specs : Ast.Ast_types.query_declaration list -> server_specs
