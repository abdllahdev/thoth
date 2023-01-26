{% for name in names %}
const {{ name }}Service = require('./{{ name }}.service');
{% endfor %}

module.exports = {
  {% for name in names %}
    {{ name }}Service,
  {% endfor %}
};
