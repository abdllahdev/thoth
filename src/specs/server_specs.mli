type service_function = { id : string; typ : string }

type service_specs = {
  name : string;
  service_functions : service_function list;
}

type controller_function = {
  id : string;
  typ : string;
  where : string option;
  filter : string list option;
  data : string list option;
}

type controller_specs = {
  name : string;
  controller_functions : controller_function list;
}

type route = { id : string; typ : string; where : string option }
type route_specs = { name : string; list : route list }

type server_specs = {
  services : service_specs list;
  controllers : controller_specs list;
  routes : route_specs list;
}

val group_queries :
  Ast.Ast_types.query_declaration list ->
  Ast.Ast_types.query_declaration list list

val get_model_name : Ast.Ast_types.query_declaration list -> string

val generate_service_specs :
  Ast.Ast_types.query_declaration list -> service_specs

val generate_controller_specs :
  Ast.Ast_types.query_declaration list -> controller_specs

val generate_routes_specs : Ast.Ast_types.query_declaration list -> route_specs
val generate_server_specs : Ast.Ast_types.query_declaration list -> server_specs
