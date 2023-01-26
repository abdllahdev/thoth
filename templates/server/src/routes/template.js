const express = require('express');
const { {{ route.name }}Controller } = require('../controllers');

const {{ route.name }}Router = express.Router();

{% for element in route.list %}
{{ route.name }}Router.{% if element.type == "findUnique" or element.type == "findMany" %}get{% elif element.type == "create" %}post{% elif element.type == "update" %}put{% elif element.type == "delete" %}delete{% endif %}('/{% if element.type == "findUnique" or element.type == "update" or element.type == "delete" %}:{{ element.where }}{% endif %}', {{ route.name }}Controller.{{ element.id }});
{% endfor %}

module.exports = {{ route.name }}Router;
