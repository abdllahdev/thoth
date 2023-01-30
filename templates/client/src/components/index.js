{% for name in names %}
import {{ name }} from "./{{ name }}"
{% endfor %}

export {
  {% for name in names %}
  {{ name }},
  {% endfor %}
}
