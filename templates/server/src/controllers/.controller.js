const httpStatus = require('http-status');
const { {{ controller.name }}Service } = require('../services');
const { handleResponse, getNum } = require('../utils');

{% for func in controller.controller_functions %}
const {{ func.id }} = async (req, res, next) => {
  {% if func.type == "create" or func.type == "update" %}
  const data = req.body;
  {% endif %}

  {% if func.type == "findUnique" or func.type == "update" or func.type == "delete" %}
  let where = req.params.{{ func.where }};

  if (!isNan(getNum(fun.where)))
    where = parseInt(where);

  const {{ func.where }} = where;
  {% endif %}

  try {
    const result = await {{ controller.name }}Service.{{ func.type }}({% if func.type != 'create' %}where, {% endif %}{% if func.type == 'create' or func.type == 'update' %}data{% endif %});

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
