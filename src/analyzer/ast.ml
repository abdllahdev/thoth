module type STRING_BLOCK = sig
  type t

  val of_string : string -> t
  val to_string : t -> string
end

module type ID = sig
  include STRING_BLOCK

  val ( = ) : t -> t -> bool
end

module Id = struct
  type t = string

  let of_string x = x
  let to_string x = x
  let ( = ) = String.equal
end

module Block : STRING_BLOCK = struct
  type t = string

  let of_string x = x
  let to_string x = x
end

module ModelName : ID = Id
module PslBlock : STRING_BLOCK = Block

type decl_type = Model
type model_definition = TModel of ModelName.t * PslBlock.t
type program = Prog of model_definition list
