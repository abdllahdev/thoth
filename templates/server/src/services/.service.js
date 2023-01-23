const prismaClient = require('../utils/prismaClient');
{% for query in service.queries %}
{% if query.type == 'findUnique' %}
const httpStatus = require('http-status');
const { ApiError } = require('../utils');
{% endif %}
{% endfor %}

{% for query in service.queries %}
const {{ query.id }} = async ({% if query.type != 'create' %}where, {% endif %}{% if query.type == 'create' or query.type == 'update' %}data{% endif %}) => {
  const {{ service.name }}{% if type == 'findMany' %}s{% endif %} = await prismaClient.{{ service.name }}.{{ query.type }}({
    {% if query.type != 'create' %}where,{% endif %}
    {% if query.type == 'create' or query.type == 'update' %}data,{% endif %}
  });

  {% if query.type == 'findUnique' %}
  if (!item) throw new ApiError(httpStatus.NOT_FOUND, [{ msg: 'Instance not found' }]);
  {% endif %}

  {% if query.type != 'destroy' %}
    return item{% if query.type == 'findMany' %}s{% endif %};
  {% endif %}
};
{% endfor %}

module.exports = {
{% for query in service.queries %}
  {{ query.id }},
{% endfor %}
};
