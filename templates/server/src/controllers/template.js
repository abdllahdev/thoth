const httpStatus = require('http-status');
const { {{ controller.name }}Service } = require('../services');
const { handleResponse, getNum, filterObject } = require('../utils');

{% for func in controller.controller_functions %}
const {{ func.id }} = async (req, res, next) => {
  {% if func.type == "create" or func.type == "update" %}
  const data = filterObject(req.body, [ {% for key in func.data %}"{{ key }}",{% endfor %} ]);
  {% endif %}

  {# TODO: implement queries with relations #}
  {% if func.type == "findMany" %}
  const where = filterObject(req.query, [ {% for key in func.filters %}"{{ key }}",{% endfor %} ])
  {% endif %}

  {% if func.type == "findUnique" or func.type == "update" or func.type == "delete" %}
  let {{ func.where }} = req.params.{{ func.where }};

  if (!isNaN(getNum({{ func.where }})))
    {{ func.where }} = parseInt({{ func.where }});

  const where = { {{ func.where }} };
  {% endif %}

  try {
    const result = await {{ controller.name }}Service.{{ func.id }}({% if func.type != 'create' %}where, {% endif %}{% if func.type == 'create' or func.type == 'update' %}data{% endif %});

    const payload = {
      status: {% if func.type == "create" %}httpStatus.CREATED,{% else %}httpStatus.OK,{% endif %}
      {% if func.type != "delete" %}data: result,{% endif %}
    };

    handleResponse(res, payload);
  } catch (err) {
    next(err);
  }
};
{% endfor %}

module.exports = {
{% for func in controller.controller_functions %}
  {{ func.id }},
{% endfor %}
};
