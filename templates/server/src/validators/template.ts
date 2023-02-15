import { z } from 'zod';

{% for validator in list %}
export const {{ validator.id }}Validator = z.object({
  {% if validator.where %}
  params: z.object({
    {{ validator.where.id }}: z.string({
      required_error: '`{{ validator.where.id }}` is required',
    })
    .regex(/^\d+$/)
    .transform((id) => parseInt(id, 10)),
  }),
  {% endif %}

  {% if validator.search %}
  query: z.object({
    {% for field in validator.search %}
    {{ field.id }}: z.{{ field.type | lower }}().optional(),
    {% endfor %}
  }),
  {% endif %}

  {% if validator.data %}
  body: z.object({
    {% for field in validator.data %}
    {{ field.id }}: z.{{ field.type | lower }}({
      required_error: '`{{ field.id }}` is required',
    }),
    {% endfor %}
  }),
  {% endif %}
});
{% endfor %}
