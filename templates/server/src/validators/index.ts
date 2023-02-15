{% for name in names %}
export * as {{ name | lower }}Validators from './{{ name | lower }}';
{% endfor %}
