val string_of_option : string option -> string
val list_of_option : 'a list option -> 'a list
val generate_service : Specs.Server_specs.service_specs -> unit
val generate_services : Specs.Server_specs.service_specs list -> unit
val generate_controller : Specs.Server_specs.controller_specs -> unit
val generate_controllers : Specs.Server_specs.controller_specs list -> unit
val generate_route : Specs.Server_specs.route_specs -> unit
val generate_routes : Specs.Server_specs.route_specs list -> unit
val setup_server_folder : unit
val generate_server : Specs.Server_specs.server_specs -> unit
