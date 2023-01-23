type app_specs = { db : Db_specs.db_specs; server : Server_specs.server_specs }

val generate_app_specs : Ast.Ast_types.ast -> app_specs
