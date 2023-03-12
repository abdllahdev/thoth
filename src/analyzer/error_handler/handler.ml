open Errors
open Core
open Ast.Formatter

let raise_type_error ?id ?received_value ?received_type loc expected_type =
  if Option.is_some received_type then
    if Option.is_some received_value then
      if Option.is_none id then
        raise
          (TypeError
             (Fmt.str
                "@(%s): Excepted a value of type '%s' but received '%s' of \
                 type '%s'"
                (string_of_loc loc)
                (string_of_type expected_type)
                (Option.value_exn received_value)
                (string_of_type (Option.value_exn received_type))))
      else
        raise
          (TypeError
             (Fmt.str
                "@(%s): Excepted a value of type '%s' but received '%s' of \
                 type '%s' in '%s'"
                (string_of_loc loc)
                (string_of_type expected_type)
                (Option.value_exn received_value)
                (string_of_type (Option.value_exn received_type))
                (Option.value_exn id)))
    else
      raise
        (TypeError
           (Fmt.str
              "@(%s): Excepted a value of type %s but received a value of type \
               %s"
              (string_of_loc loc)
              (string_of_type expected_type)
              (string_of_type (Option.value_exn received_type))))
  else
    raise
      (TypeError
         (Fmt.str "@(%s): Excepted a value of type %s" (string_of_loc loc)
            (string_of_type expected_type)))

let raise_declaration_type_error ?id loc expected_type received_value
    received_type =
  if Option.is_none id then
    raise
      (DeclarationTypeError
         (Fmt.str
            "@(%s): Excepted a declaration of type '%s' but received '%s' of \
             type '%s'"
            (string_of_loc loc)
            (string_of_declaration_type expected_type)
            received_value
            (string_of_declaration_type received_type)))
  else
    raise
      (DeclarationTypeError
         (Fmt.str
            "@(%s): Excepted a declaration of type '%s' but received '%s' of \
             type '%s' in '%s'"
            (string_of_loc loc)
            (string_of_declaration_type expected_type)
            received_value
            (string_of_declaration_type received_type)
            (Option.value_exn id)))

let raise_dot_operator_error loc expanded_value id id_type =
  raise
    (TypeError
       (Fmt.str "@(%s): Cannot get '%s' from '%s' of type '%s'"
          (string_of_loc loc) expanded_value id (string_of_type id_type)))

let raise_unique_field_error ?id loc expected_type received_value received_type
    =
  if Option.is_none id then
    raise
      (UniqueFieldError
         (Fmt.str
            "@(%s): Excepted a field of type '%s' but received '%s' of type \
             '%s'"
            (string_of_loc loc)
            (ModelFormatter.string_of_field_unique_type expected_type)
            received_value
            (ModelFormatter.string_of_field_unique_type received_type)))
  else
    raise
      (UniqueFieldError
         (Fmt.str
            "@(%s): Excepted a field of type '%s' but received '%s' of type \
             '%s' in '%s'"
            (string_of_loc loc)
            (ModelFormatter.string_of_field_unique_type expected_type)
            received_value
            (ModelFormatter.string_of_field_unique_type received_type)
            (Option.value_exn id)))

let raise_query_argument_error loc id expected_args received_arg =
  raise
    (QueryArgumentError
       (Fmt.str "@(%s): Excepted a '%s' %s but received a '%s' argument in '%s'"
          (string_of_loc loc)
          (String.concat ~sep:", "
             (List.map expected_args
                ~f:QueryFormatter.string_of_query_argument_type))
          (if List.length expected_args > 1 then "argument" else "argument")
          (QueryFormatter.string_of_query_argument_type received_arg)
          id))

let raise_query_type_error loc expected_query_type received_query
    received_query_type =
  raise
    (QueryTypeError
       (Fmt.str
          "@(%s): Excepted a query of type '%s' but received '%s' of type '%s'"
          (string_of_loc loc)
          (QueryFormatter.string_of_query_type expected_query_type)
          received_query
          (QueryFormatter.string_of_query_type received_query_type)))

let raise_undefined_error ?declaration_id ?declaration_type loc declaration id =
  if Option.is_none declaration_id && Option.is_none declaration_type then
    raise
      (UndefinedError
         (Fmt.str "@(%s): Undefined %s '%s'" (string_of_loc loc) declaration id))
  else
    raise
      (UndefinedError
         (Fmt.str "@(%s): Undefined %s '%s' in %s '%s'" (string_of_loc loc)
            declaration id
            (string_of_declaration_type (Option.value_exn declaration_type))
            (Option.value_exn declaration_id)))

let raise_name_error loc declaration_type =
  raise
    (NameError
       (Fmt.str "@(%s): %s declaration name must start with a capital character"
          (string_of_loc loc)
          (string_of_declaration_type declaration_type)))

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

let raise_type_relation_error loc relation_field_id relation_field_type
    ref_field_id ref_field_type =
  raise
    (RelationError
       (Fmt.str
          "@(%s): The relation fields must be of the same type but received \
           '%s' of type '%s' and '%s' of type '%s'"
          (string_of_loc loc) relation_field_id
          (string_of_type relation_field_type)
          ref_field_id
          (string_of_type ref_field_type)))

let raise_attribute_error loc field_type attribute =
  raise
    (AttributeError
       (Fmt.str "@(%s): Field of type '%s' cannot take '%s' attribute"
          (string_of_loc loc)
          (string_of_type field_type)
          attribute))

let raise_bad_assignment_error loc id =
  raise
    (BadAssignmentError
       (Fmt.str "@(%s): The value of '%s' cannot be assigned to a variable"
          (string_of_loc loc) id))

let raise_argument_type_error loc component_type typ =
  raise
    (ArgumentTypeError
       (Fmt.str "@(%s): %s components cannot receive an argument of type '%s'"
          (string_of_loc loc) component_type (string_of_type typ)))

let raise_element_type_error loc expected_type received_type =
  raise
    (ElementTypeError
       (Fmt.str
          "@(%s): Expected an element type of '%s' but received an element of \
           type '%s'"
          (string_of_loc loc) expected_type received_type))

let raise_query_return_type_error loc query_type expected_type received_type =
  raise
    (QueryReturnTypeError
       (Fmt.str
          "@(%s): Query of type '%s' expected a return of type '%s' but \
           received a return type of '%s'"
          (string_of_loc loc)
          (QueryFormatter.string_of_query_type query_type)
          (string_of_type expected_type)
          (string_of_type received_type)))

let raise_required_argument_error ?arg_type loc arg_id id =
  match arg_type with
  | Some arg_type ->
      raise
        (RequiredArgumentError
           (Fmt.str "@(%s): Required argument '%s' of type '%s' in '%s"
              (string_of_loc loc) arg_id (string_of_type arg_type) id))
  | None ->
      raise
        (RequiredArgumentError
           (Fmt.str "@(%s): Required argument '%s' in '%s" (string_of_loc loc)
              arg_id id))

let raise_required_form_input_error loc input_id id =
  raise
    (RequiredFormInputError
       (Fmt.str "@(%s): Required form input '%s' in '%s" (string_of_loc loc)
          input_id id))

let raise_unexpected_argument_error ?id loc arg_id =
  if Option.is_none id then
    raise
      (UnexpectedArgumentError
         (Fmt.str "@(%s): Unexpected argument '%s'" (string_of_loc loc) arg_id))
  else
    raise
      (UnexpectedArgumentError
         (Fmt.str "@(%s): Unexpected argument '%s' in '%s'" (string_of_loc loc)
            arg_id (Option.value_exn id)))

let raise_unexpected_permissions_attr loc query_id =
  raise
    (UnexpectedAttributeError
       (Fmt.str
          "@(%s): Unexpected attribute permissions in %s. The app must \
           implement auth to have permissions"
          (string_of_loc loc) query_id))

let raise_unexpected_config loc config_id =
  raise
    (UnexpectedConfigError
       (Fmt.str "@(%s): Unexpected configuration %s." (string_of_loc loc)
          config_id))

let raise_requires_configuration loc component_type =
  raise
    (RequiredConfigError
       (Fmt.str "@(%s): %s requires auth configuration" (string_of_loc loc)
          (ComponentFormatter.string_of_component_type component_type)))

let raise_compiler_error () =
  raise
    (CompilationError (Fmt.str "CompilationError: Something wrong happened"))
