const prismaClient = require('../utils/prismaClient');
{% for func in service.service_functions %}
{% if func.type == 'findUnique' %}
const httpStatus = require('http-status');
const { ApiError } = require('../utils');
{% endif %}
{% endfor %}

{% for func in service.service_functions %}
const {{ func.id }} = async ({% if func.type != 'create' %}where, {% endif %}{% if func.type == 'create' or func.type == 'update' %}data{% endif %}) => {
  const {{ func.name }}{% if type == 'findMany' %}s{% endif %} = await prismaClient.{{ service.name }}.{{ func.type }}({
    {% if func.type != 'create' %}where,{% endif %}
    {% if func.type == 'create' or func.type == 'update' %}data,{% endif %}
  });

  {% if func.type == 'findUnique' %}
  if (!item) throw new ApiError(httpStatus.NOT_FOUND, 'Instance not found');
  {% endif %}

  {% if func.type != 'destroy' %}
  return item{% if func.type == 'findMany' %}s{% endif %};
  {% endif %}
};
{% endfor %}

module.exports = {
{% for func in service.service_functions %}
  {{ func.id }},
{% endfor %}
};
