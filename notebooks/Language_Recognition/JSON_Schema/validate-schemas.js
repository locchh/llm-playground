const Ajv = require('ajv');
const addFormats = require('ajv-formats');
const fs = require('fs');
const path = require('path');

// Create a new instance of Ajv with options for handling relative references
const ajv = new Ajv({
  allErrors: true,
  strict: false,
  baseUri: 'file:///home/loc/Works/tmp/schemas/'
});

// Add formats like email, date-time, etc.
addFormats(ajv);

// Load all schemas from the schemas directory
function loadAllSchemas() {
  const schemasDir = path.join(__dirname, 'schemas');
  const schemaFiles = fs.readdirSync(schemasDir);
  
  schemaFiles.forEach(file => {
    if (file.endsWith('.json')) {
      const schemaPath = path.join(schemasDir, file);
      const schema = JSON.parse(fs.readFileSync(schemaPath, 'utf8'));
      ajv.addSchema(schema);
    }
  });
}

// Function to validate data against a schema
async function validateData(schemaId, data) {
  try {
    // Get the schema by ID or file path
    const validate = ajv.getSchema(schemaId) || 
                     ajv.getSchema(`file:///home/loc/Works/tmp/schemas/${schemaId}`);
    
    if (!validate) {
      throw new Error(`Schema not found: ${schemaId}`);
    }
    
    // Validate the data
    const valid = validate(data);
    
    if (valid) {
      console.log(`✅ Validation successful for ${schemaId}`);
      return true;
    } else {
      console.log(`❌ Validation failed for ${schemaId}`);
      console.log('Errors:', JSON.stringify(validate.errors, null, 2));
      return false;
    }
  } catch (error) {
    console.error(`Error validating against ${schemaId}:`, error);
    return false;
  }
}

// Main function to run validations
async function runValidations() {
  console.log('Starting schema validations...');
  
  // Load all schemas first
  loadAllSchemas();
  
  // Load example data
  const productData = JSON.parse(fs.readFileSync(path.join(__dirname, 'example-data', 'product-example.json'), 'utf8'));
  const customerData = JSON.parse(fs.readFileSync(path.join(__dirname, 'example-data', 'customer-example.json'), 'utf8'));
  const orderData = JSON.parse(fs.readFileSync(path.join(__dirname, 'example-data', 'order-example.json'), 'utf8'));
  const categoryData = JSON.parse(fs.readFileSync(path.join(__dirname, 'example-data', 'category-example.json'), 'utf8'));

  // Validate each example against its schema
  await validateData('product.json', productData);
  await validateData('customer.json', customerData);
  await validateData('order.json', orderData);
  await validateData('recursive-example.json', categoryData);
  
  console.log('Validation complete!');
}

// Run the validations
runValidations().catch(error => {
  console.error('Error running validations:', error);
});
