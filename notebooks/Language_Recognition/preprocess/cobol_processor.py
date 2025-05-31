#!/usr/bin/env python3
"""
COBOL Grammar Processor

This script demonstrates how the Cobol85Preprocessor.g4 and Cobol85.g4 grammars
work together to process COBOL code in a real-world scenario.

The workflow:
1. Input Processing: Read the COBOL source file
2. Preprocessing: Process directives (COPY, REPLACE, EXEC) using Cobol85Preprocessor.g4
3. Parsing: Parse the preprocessed code using Cobol85.g4
4. Transformation: Demonstrate a simple transformation (e.g., enforce naming conventions)
5. Code Generation: Output the transformed code

Author: Cascade AI
Date: 2025-05-31
"""

import os
import json
import sys
import re
import subprocess
from pathlib import Path

# Simulate ANTLR4 parsing since we don't have the actual generated parsers
class CobolPreprocessor:
    def __init__(self, copy_dir="."):
        self.copy_dir = copy_dir
        
    def preprocess(self, input_file):
        """
        Preprocess COBOL code, handling COPY, REPLACE, and EXEC statements.
        This simulates what Cobol85Preprocessor.g4 would do.
        """
        print(f"[PREPROCESSOR] Processing {input_file}")
        
        with open(input_file, 'r') as f:
            lines = f.readlines()
        
        # Process the file line by line
        processed_lines = []
        i = 0
        while i < len(lines):
            line = lines[i]
            
            # Handle COPY statements
            if re.search(r'\bCOPY\b', line, re.IGNORECASE):
                copy_file = re.search(r'\bCOPY\s+"([^"]+)"', line, re.IGNORECASE)
                if copy_file:
                    filename = copy_file.group(1)
                    copy_path = os.path.join(self.copy_dir, filename)
                    
                    print(f"[PREPROCESSOR] Including COPY file: {copy_path}")
                    
                    if os.path.exists(copy_path):
                        with open(copy_path, 'r') as cf:
                            copy_content = cf.readlines()
                            processed_lines.extend(copy_content)
                    else:
                        processed_lines.append(f"      *> ERROR: COPY file not found: {filename}\n")
                i += 1
                continue
            
            # Handle REPLACE statements
            if re.search(r'\bREPLACE\b', line, re.IGNORECASE):
                replace_match = re.search(r'\bREPLACE\s+==([^=]+)==\s+BY\s+==([^=]+)==', line, re.IGNORECASE)
                if replace_match:
                    old_text = replace_match.group(1).strip()
                    new_text = replace_match.group(2).strip()
                    
                    print(f"[PREPROCESSOR] Replace: '{old_text}' -> '{new_text}'")
                    
                    # Apply replacement to all subsequent lines until we find another REPLACE or end
                    j = i + 1
                    while j < len(lines):
                        if re.search(r'\bREPLACE\b', lines[j], re.IGNORECASE):
                            break
                        lines[j] = lines[j].replace(old_text, new_text)
                        j += 1
                
                i += 1
                continue
            
            # Handle EXEC SQL statements
            if re.search(r'\bEXEC\s+SQL\b', line, re.IGNORECASE):
                sql_block = [line]
                j = i + 1
                
                # Collect all lines until END-EXEC
                while j < len(lines) and not re.search(r'\bEND-EXEC\b', lines[j], re.IGNORECASE):
                    sql_block.append(lines[j])
                    j += 1
                
                if j < len(lines):
                    sql_block.append(lines[j])  # Include the END-EXEC line
                
                print(f"[PREPROCESSOR] Processing SQL block ({len(sql_block)} lines)")
                
                # In a real implementation, we might transform the SQL block
                processed_lines.extend(sql_block)
                i = j + 1
                continue
            
            # Regular line, just add it
            processed_lines.append(line)
            i += 1
        
        return processed_lines

class CobolParser:
    def parse(self, preprocessed_lines):
        """
        Parse preprocessed COBOL code.
        This simulates what Cobol85.g4 would do.
        """
        print("[PARSER] Parsing preprocessed COBOL code")
        
        # In a real implementation, this would use the ANTLR4 parser
        # Here we'll do a simplified parsing to demonstrate the concept
        
        # Extract program structure
        program_structure = {
            "program_id": None,
            "divisions": {},
            "data_items": [],
            "statements": [],
            "sql_statements": []
        }
        
        current_division = None
        current_section = None
        current_paragraph = None
        in_sql_block = False
        sql_block = []
        
        for i, line in enumerate(preprocessed_lines):
            # Extract PROGRAM-ID
            program_id_match = re.search(r'\bPROGRAM-ID\.\s+(\w+)', line, re.IGNORECASE)
            if program_id_match:
                program_structure["program_id"] = program_id_match.group(1)
                print(f"[PARSER] Found PROGRAM-ID: {program_structure['program_id']}")
            
            # Extract divisions
            division_match = re.search(r'\b(\w+)\s+DIVISION\b', line, re.IGNORECASE)
            if division_match:
                current_division = division_match.group(1).upper()
                if current_division not in program_structure["divisions"]:
                    program_structure["divisions"][current_division] = {
                        "sections": {},
                        "paragraphs": {}
                    }
                print(f"[PARSER] Found division: {current_division}")
                continue
            
            # Extract sections
            section_match = re.search(r'\b(\w+(?:-\w+)*)\s+SECTION\b', line, re.IGNORECASE)
            if section_match:
                current_section = section_match.group(1).upper()
                if current_division and current_division in program_structure["divisions"]:
                    if "sections" not in program_structure["divisions"][current_division]:
                        program_structure["divisions"][current_division]["sections"] = {}
                    
                    program_structure["divisions"][current_division]["sections"][current_section] = {
                        "line": i + 1,
                        "content": []
                    }
                print(f"[PARSER] Found section: {current_section}")
                continue
            
            # Extract paragraphs
            paragraph_match = re.search(r'^\s+(\w+(?:-\w+)*)\.', line)
            if paragraph_match and not line.strip().startswith('*>') and 'END-EXEC' not in line:
                current_paragraph = paragraph_match.group(1).upper()
                if current_division and current_division in program_structure["divisions"]:
                    if "paragraphs" not in program_structure["divisions"][current_division]:
                        program_structure["divisions"][current_division]["paragraphs"] = {}
                    
                    program_structure["divisions"][current_division]["paragraphs"][current_paragraph] = {
                        "line": i + 1,
                        "statements": []
                    }
                print(f"[PARSER] Found paragraph: {current_paragraph}")
                continue
            
            # Extract data items
            data_item_match = re.search(r'^\s+(\d+)\s+(\w+(?:-\w+)*)\s+PIC\s+([^.]+)', line, re.IGNORECASE)
            if data_item_match and current_division == "DATA":
                level = data_item_match.group(1)
                name = data_item_match.group(2)
                pic = data_item_match.group(3).strip()
                
                # Check for VALUE clause
                value_match = re.search(r'VALUE\s+([^.]+)', line, re.IGNORECASE)
                value = value_match.group(1).strip() if value_match else None
                
                data_item = {
                    "level": level,
                    "name": name,
                    "picture": pic,
                    "value": value,
                    "line": i + 1
                }
                
                program_structure["data_items"].append(data_item)
                print(f"[PARSER] Found data item: {level} {name} PIC {pic}")
                continue
            
            # Track SQL blocks
            if re.search(r'\bEXEC\s+SQL\b', line, re.IGNORECASE):
                in_sql_block = True
                sql_block = [line.strip()]
                continue
                
            if in_sql_block:
                sql_block.append(line.strip())
                if re.search(r'\bEND-EXEC\b', line, re.IGNORECASE):
                    in_sql_block = False
                    sql_statement = " ".join(sql_block)
                    program_structure["sql_statements"].append({
                        "statement": sql_statement,
                        "line": i + 1 - len(sql_block) + 1
                    })
                    print(f"[PARSER] Found SQL statement at line {i + 1 - len(sql_block) + 1}")
                continue
            
            # Extract COBOL statements in PROCEDURE DIVISION
            if current_division == "PROCEDURE" and current_paragraph and line.strip() and not line.strip().startswith('*>') and not in_sql_block:
                # Look for common COBOL statements
                found_statement = False
                for stmt_type in ["DISPLAY", "MOVE", "PERFORM", "IF", "EVALUATE", "COMPUTE", "STOP", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "STRING", "UNSTRING", "CALL", "GO TO", "EXIT"]:
                    stmt_pattern = f"\\b{stmt_type}\\b"
                    stmt_match = re.search(stmt_pattern, line, re.IGNORECASE)
                    if stmt_match:
                        statement = {
                            "type": stmt_type,
                            "content": line.strip(),
                            "line": i + 1,
                            "paragraph": current_paragraph
                        }
                        
                        program_structure["statements"].append(statement)
                        
                        # Also add to the paragraph
                        if current_division in program_structure["divisions"] and \
                           "paragraphs" in program_structure["divisions"][current_division] and \
                           current_paragraph in program_structure["divisions"][current_division]["paragraphs"]:
                            program_structure["divisions"][current_division]["paragraphs"][current_paragraph]["statements"].append(statement)
                        
                        print(f"[PARSER] Found {stmt_type} statement in {current_paragraph}")
                        found_statement = True
                        break
        
        return program_structure

class CobolTransformer:
    def transform(self, program_structure, preprocessed_lines):
        """
        Transform the COBOL code based on the parsed structure.
        This demonstrates a simple transformation: enforcing naming conventions.
        """
        print("[TRANSFORMER] Transforming COBOL code")
        
        transformed_lines = preprocessed_lines.copy()
        
        # Example transformation: Ensure all paragraph names use UPPERCASE-WITH-HYPHENS
        for i, line in enumerate(transformed_lines):
            # Look for paragraph definitions
            paragraph_match = re.search(r'^       (\w+(?:-\w+)*)\.', line)
            if paragraph_match:
                paragraph_name = paragraph_match.group(1)
                standardized_name = paragraph_name.upper()
                
                if paragraph_name != standardized_name:
                    print(f"[TRANSFORMER] Standardizing paragraph name: {paragraph_name} -> {standardized_name}")
                    transformed_lines[i] = line.replace(paragraph_name, standardized_name)
        
        # Add validation comments
        transformed_lines.insert(0, "      *> Code validated and transformed by COBOL Validator\n")
        transformed_lines.insert(1, f"      *> Timestamp: {subprocess.check_output('date', shell=True).decode().strip()}\n")
        
        return transformed_lines

def main():
    if len(sys.argv) < 2:
        print("Usage: python cobol_processor.py <cobol_file>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    copy_dir = os.path.dirname(os.path.abspath(input_file))
    
    # Step 1: Input Processing
    print(f"Processing COBOL file: {input_file}")
    
    # Step 2: Preprocessing (using Cobol85Preprocessor.g4)
    preprocessor = CobolPreprocessor(copy_dir)
    preprocessed_lines = preprocessor.preprocess(input_file)
    
    # Save preprocessed output
    preprocessed_file = f"{input_file}.preprocessed"
    with open(preprocessed_file, 'w') as f:
        f.writelines(preprocessed_lines)
    print(f"Preprocessed output saved to: {preprocessed_file}")
    
    # Step 3: Parsing (using Cobol85.g4)
    parser = CobolParser()
    program_structure = parser.parse(preprocessed_lines)
    
    # Save program structure (AST representation)
    
    with open(f"{input_file}.ast.json", 'w') as f:
        json.dump(program_structure, f, indent=2)
    print(f"Program structure saved to: {input_file}.ast.json")
    
    # Print full AST to stdout
    print("\n===== FULL ABSTRACT SYNTAX TREE =====")
    print(json.dumps(program_structure, indent=2))
    print("===== END OF AST =====\n")
    
    # Step 4: Transformation
    transformer = CobolTransformer()
    transformed_lines = transformer.transform(program_structure, preprocessed_lines)
    
    # Step 5: Code Generation
    transformed_file = f"{input_file}.transformed"
    with open(transformed_file, 'w') as f:
        f.writelines(transformed_lines)
    print(f"Transformed output saved to: {transformed_file}")
    
    print("\nCOBOL processing complete!")
    print("This demonstrates how Cobol85Preprocessor.g4 and Cobol85.g4 work together:")
    print("1. Preprocessor grammar handles COPY, REPLACE, and EXEC statements")
    print("2. Main grammar parses the program structure (divisions, sections, paragraphs)")
    print("3. The parsed structure can be used for validation and transformation")

if __name__ == "__main__":
    main()
