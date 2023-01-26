{% for name in names %}
const {{ name }}Controller = require('./{{ name }}.controller');
{% endfor %}

module.exports = {
  {% for name in names %}
    {{ name }}Controller,
  {% endfor %}
};
