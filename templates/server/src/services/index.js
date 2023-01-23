{% if services %}

{% for service in services %}
const {{ service.name }}Service = require('./{{ service.name }}.service');
{% endfor %}

module.exports = {
  {% for service in services %}
    {{ service.name }}Service,
  {% endfor %}
};
{% endif %}
