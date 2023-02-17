import { z } from 'zod';

{% for validator in list %}
export const {{ validator.id }}Validator = z.object({
  {% if validator.where %}
    params: z.object({
      {% for field in validator.where %}
        {{ field.id }}: z.string({
          required_error: '`{{ field.id }}` is required',
        })

        {% if field.type[0] == "number" %}
        .regex(/^\d+$/)
        .transform(({{ field.id }} : string) => parseInt({{ field.id }}, 10)),
        {% else %}
        ,
        {% endif %}
      {% endfor %}
    }),
  {% endif %}

  {% if validator.search %}
  query: z.object({
    {% for field in validator.search %}
      {{ field.id }}: z.{{ field.type[0] | lower }}().optional(),
    {% endfor %}
  }),
  {% endif %}

  {% if validator.data %}
  body: z.object({
    {% for field in validator.data %}
      {% if field.field %}
        {{ field.field.id }}: z.{{ field.field.type[0] | lower }}({
          required_error: '`{{ field.field.id }}` is required',
        })
        {% for type in field.field.type %}
          {% if type == "optional" or type == "array" %}
          .{{ type | lower }}()
          {% endif %}
        {% endfor %}
        ,
      {% endif %}

      {% if field.object %}
        {{ field.object.id }}: z.object({
          connect: z.object({
            {{ field.object.field.id }}: z.{{ field.object.field.type[0] | lower }}({
              required_error: '`{{ field.object.field.id }}` is required',
            })
            {% for type in field.object.field.type %}
              {% if type == "optional" or type == "array" %}
              .{{ type | lower }}()
              {% endif %}
            {% endfor %}
            ,
          })
        }),
      {% endif %}
    {% endfor %}
  }),
  {% endif %}
});
{% endfor %}
