import json
import sys
from jsonschema import Draft7Validator, ValidationError

def is_valid_json_schema(file_path):
    try:
        with open(file_path, 'r', encoding='utf-8') as f:
            schema = json.load(f)
        
        # Check if it's a valid JSON Schema
        Draft7Validator.check_schema(schema)
        print(f"The file '{file_path}' is a valid JSON Schema.")
        return True
    except json.JSONDecodeError as e:
        print(f"Invalid JSON format: {e}")
    except ValidationError as e:
        print(f"Invalid JSON Schema: {e}")
    except Exception as e:
        print(f"Error: {e}")
    return False

if __name__ == "__main__":
    if len(sys.argv) != 2:
        print("Usage: python validate_json_schema.py <path_to_json_schema>")
    else:
        file_path = sys.argv[1]
        is_valid_json_schema(file_path)
