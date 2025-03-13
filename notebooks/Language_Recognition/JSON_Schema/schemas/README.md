# Modular JSON Schema Example

This project demonstrates how to build complex JSON schemas using modular components and references. The example represents an e-commerce system with products, customers, and orders.

## Schema Structure

The schemas are organized as follows:

1. **Base Components**:
   - `address.json`: Reusable schema for physical addresses
   - `contact.json`: Reusable schema for contact information

2. **Domain Schemas**:
   - `product.json`: Product schema with internal definitions for variants and prices
   - `customer.json`: Customer schema that references address and contact schemas
   - `order.json`: Order schema that combines products and customer information

## Modular Techniques Demonstrated

This example demonstrates several techniques for creating modular JSON Schemas:

### 1. External References with `$ref`

External references allow you to reuse schemas defined in other files:

```json
"shippingAddress": {
  "$ref": "./address.json"
}
```

### 2. Internal Definitions with `$defs`

The `$defs` keyword provides a standardized place to define reusable subschemas within a schema:

```json
"$defs": {
  "price": {
    "type": "object",
    "properties": {
      "amount": { "type": "number" },
      "currency": { "type": "string" }
    }
  }
},
"properties": {
  "basePrice": {
    "$ref": "#/$defs/price"
  }
}
```

### 3. Schema Identification with `$id`

Each schema has a unique identifier using the `$id` keyword, making it referenceable:

```json
"$id": "https://example.com/schemas/product.json"
```

### 4. Nested References

References can be chained, allowing for complex compositions:

```json
"payment": {
  "$ref": "#/$defs/payment"
},
```

Where the payment definition itself references another schema:

```json
"price": {
  "$ref": "./product.json#/$defs/price"
}
```

### 5. Relative References

You can use relative file paths to reference other schema files in the same directory:

```json
"contact": {
  "$ref": "./contact.json",
  "description": "Customer's contact information"
}
```

## Benefits of Modular JSON Schema

1. **Reusability**: Define common structures once and reuse them
2. **Maintainability**: Update a schema in one place and all references benefit
3. **Readability**: Break complex schemas into logical, manageable pieces
4. **Consistency**: Ensure consistent structure across your data model

## Usage

These schemas can be used with any JSON Schema validator that supports Draft 2020-12.
