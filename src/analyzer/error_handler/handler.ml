open Errors
open Core
open Ast.Pprinter

let raise_type_error ?(id = "") loc expected_type received_value received_type =
  if String.is_empty id then
    raise
      (TypeError
         (Fmt.str
            "@(%s): Excepted a value of type '%s' but received '%s' of type \
             '%s'"
            (string_of_loc loc) expected_type received_value received_type))
  else
    raise
      (TypeError
         (Fmt.str
            "@(%s): Excepted a value of type '%s' but received '%s' of type \
             '%s' in '%s'"
            (string_of_loc loc) expected_type received_value received_type id))

let raise_undefined_error ?(declaration_id = "") ?(declaration_type = "") loc
    typ id =
  if String.is_empty declaration_id then
    raise
      (UndefinedError
         (Fmt.str "@(%s): Undefined %s '%s'" (string_of_loc loc) typ id))
  else
    raise
      (UndefinedError
         (Fmt.str "@(%s): Undefined %s '%s' in %s '%s'" (string_of_loc loc) typ
            id declaration_type declaration_id))

let raise_name_error loc declaration_type =
  raise
    (NameError
       (Fmt.str "@(%s): %s declaration name must start with a capital character"
          (string_of_loc loc) declaration_type))

let raise_syntax_error loc value =
  raise
    (InvalidSyntaxError
       (Fmt.str "@(%s): Invalid syntax '%s'" (string_of_loc loc) value))

let raise_reserved_keyword_error loc value =
  raise
    (ReservedKeywordError
       (Fmt.str "@(%s): '%s' cannot be used as identifier" (string_of_loc loc)
          value))

let raise_argument_number_error loc expected_number received_number id =
  raise
    (ArgumentNumberError
       (Fmt.str
          "@(%s): Expected '%d' arguments but received '%d' arguments in '%s'."
          (string_of_loc loc) expected_number received_number id))

let raise_multi_definitions_error loc id =
  raise
    (MultiDefinitionsError
       (Fmt.str "@(%s): '%s' was declared multiple times" (string_of_loc loc) id))

let raise_relation_error loc field_id model_id other_model_id =
  raise
    (RelationError
       (Fmt.str
          "@(%s): The relation field '%s' on model '%s' is missing an opposite \
           relation field on the model '%s'"
          (string_of_loc loc) field_id model_id other_model_id))

let raise_attribute_error loc field_type attribute =
  raise
    (AttributeError
       (Fmt.str "@(%s): Field of type '%s' cannot take '%s' attribute"
          (string_of_loc loc)
          (string_of_scalar_type field_type)
          attribute))

let raise_bad_assignment_error loc id =
  raise
    (BadAssignmentError
       (Fmt.str "@(%s): The value of '%s' cannot be assigned to a variable"
          (string_of_loc loc) id))

let raise_bad_argument_type_error loc typ =
  raise
    (BadArgumentTypeError
       (Fmt.str "@(%s): Component argument type cannot be of type '%s'"
          (string_of_loc loc) typ))

let raise_element_type_error loc expected_type received_type =
  raise
    (ElementTypeError
       (Fmt.str
          "@(%s): Expected an element type of '%s' but received an element of \
           type '%s'"
          (string_of_loc loc) expected_type received_type))
