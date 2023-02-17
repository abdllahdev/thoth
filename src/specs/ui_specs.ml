type xra_literal = {
  string_literal : string;
  boolean_literal : bool;
  number_literal : int;
}

type xra_expression = { xra_literal : xra_literal }

type find_component = {
  on_error : xra_expression;
  on_loading : xra_expression;
  on_success : xra_expression;
}
