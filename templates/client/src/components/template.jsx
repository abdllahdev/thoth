{% if component.let_exprs %}
import useSWR from "swr";
{% endif %}
{% if type == "page" %}
import { TaskView } from "../components";
{% endif %}

const {{ component.id }} = ({ {{ component.args }} }) => {
  {% for let_expr in component.let_exprs %}
  const { isLoading, error, data } = useSWR("/{{ let_expr.query.model }}s");

  if (isLoading) return "Loading...";

  if (error) return "Error!";

  const {{ let_expr.id }} = data.data;
  {% endfor %}

  return (
    {%- autoescape false -%}
    {{ component.jsx }}
    {% endautoescape %}
  )
}

export default {{ component.id }}
