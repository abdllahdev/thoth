open Errors

let raise_type_error loc expected_type received_value received_type id =
  raise
    (TypeError
       (Fmt.str
          "@(%s): Excepted an argument of type '%s' but received '%s' of type \
           '%s' in '%s'"
          loc expected_type received_value received_type id))

let raise_name_error loc typ id =
  raise (NameError (Fmt.str "NameError@(%s): Undefined %s '%s'" loc typ id))

let raise_syntax_error loc value =
  raise (SyntaxError (Fmt.str "@(%s): Unidentifiable '%s'" loc value))

let raise_reserved_keyword_error loc value =
  raise
    (ReservedKeywordError
       (Fmt.str "@(%s): '%s' cannot be used as identifier" loc value))

let raise_argument_number_error loc expected_number received_number id =
  raise
    (ArgumentNumberError
       (Fmt.str
          "@(%s): Expected '%d' arguments but received '%d' arguments in '%s'."
          loc expected_number received_number id))

let raise_multi_definitions_error loc id =
  raise
    (MultiDefinitionsError
       (Fmt.str "@(%s): '%s' was declared multiple times" loc id))

let raise_relation_error loc field_id model_id other_model_id =
  raise
    (RelationError
       (Fmt.str
          "@(%s): The relation field '%s' on model '%s' is missing an opposite \
           relation field on the model '%s'"
          loc field_id model_id other_model_id))
