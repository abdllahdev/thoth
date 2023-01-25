const prismaClient = require('../utils/prismaClient');
{% for func in service.service_functions %}
{% if func.type == 'findUnique' %}
const httpStatus = require('http-status');
const { ApiError } = require('../utils');
{% endif %}
{% endfor %}

{% for func in service.service_functions %}
const {{ func.id }} = async ({% if func.type != 'create' %}where, {% endif %}{% if func.type == 'create' or func.type == 'update' %}data{% endif %}) => {
  const result = await prismaClient.{{ service.name }}.{{ func.type }}({
    {% if func.type != 'create' %}where,{% endif %}
    {% if func.type == 'create' or func.type == 'update' %}data,{% endif %}
  });

  {% if func.type == 'findUnique' %}
  if (!result) throw new ApiError(httpStatus.NOT_FOUND, 'Instance not found');
  {% endif %}

  {% if func.type != 'destroy' %}
  return result;
  {% endif %}
};
{% endfor %}

module.exports = {
{% for func in service.service_functions %}
  {{ func.id }},
{% endfor %}
};
