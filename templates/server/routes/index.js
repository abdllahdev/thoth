const express = require('express');
const router = express.Router();

{% for name in names %}
const {{ name }}Router = require('./{{ name }}.routes');
{% endfor %}

{% for name in names %}
router.use('/{{ name }}s', {{ name }}Router);
{% endfor %}

module.exports = router;
