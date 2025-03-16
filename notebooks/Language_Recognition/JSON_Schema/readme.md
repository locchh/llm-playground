### To create a basic schema definition, define the following keywords:

To create a basic schema definition, define the following keywords:

- `$schema`: specifies which draft of the JSON Schema standard the schema adheres to.
- `$id`: sets a URI for the schema. You can use this unique URI to refer to elements of the schema from inside the same document or from external JSON documents.
- `title` and `description`: state the intent of the schema. These keywords don’t add any constraints to the data being validated.
- `type`: defines the first constraint on the JSON data. In the product catalog example below, this keyword specifies that the data must be a JSON object.

```
{
  "$schema": "https://json-schema.org/draft/2020-12/schema",
  "$id": "https://example.com/product.schema.json",
  "title": "Product",
  "description": "A product in the catalog",
  "type": "object"
}

```

In JSON Schema terminology, `$schema` and `$id` are **schema keywords**, `title` and `description` are **schema annotations**, and `type` is a validation keyword.

# references

[ Fine-tuning LLM to Generate Persian Product Catalogs in JSON Format](https://huggingface.co/learn/cookbook/en/fine_tuning_llm_to_generate_persian_product_catalogs_in_json_format)

[BaSalam/entity-attribute-dataset-GPT-3.5-generated-v1](https://huggingface.co/datasets/BaSalam/entity-attribute-dataset-GPT-3.5-generated-v1)

[JSON Schema: A Media Type for Describing JSON Documents](https://json-schema.org/draft/2020-12/json-schema-core)

[JON Schema](https://json-schema.org/learn/getting-started-step-by-step#creating-your-first-schema)

[JSON Schema Validator](https://www.jsonschemavalidator.net/)

[Conditional JSON Schema validation](https://json-schema.org/understanding-json-schema/reference/conditionals)

[Regular Expressions](https://json-schema.org/understanding-json-schema/reference/regular_expressions)

[Modular JSON Schema combination](https://json-schema.org/understanding-json-schema/structuring)

[JSON Schema Docs](https://www.learnjsonschema.com/2020-12/)

