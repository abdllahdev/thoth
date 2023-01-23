const prismaClient = require('../utils/prismaClient');
{% if function.type == "findUnique" %}
const httpStatus = require('http-status');
const { ApiError } = require('../utils');
{% endif %}

{% for type in service.types %}
const {{ "{{ type }}" if type != "delete" else "destroy" }} = async ({{ "data," if type == "create" or type == "update" }}' '{{"where" if type != "create" }}) => {
  {{"const {{ service.name }}{{ 's' if type == 'findMany' }} = " if type != "delete" }}await prismaClient.{{ service.name }}.{{ type }}({
    {{"where," if type != "create" }}
    {{ "data," if type == "create" or type == "update" }}
  });

  {% if type == "findUnique" %}
  if (!item) throw new ApiError(httpStatus.NOT_FOUND, [{ msg: 'Instance not found' }]);
  {% endif %}

  {{"return item{{ 's' if function.type == 'findMany' }};" if type != "delete" }}
};
{% endfor %}

module.exports = {
{% for type in service.types %}
  {{ type }},
{% endfor %}
};
