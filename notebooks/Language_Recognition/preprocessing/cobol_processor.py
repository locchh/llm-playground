#!/usr/bin/env python3
"""
COBOL Grammar Processor

This script demonstrates how to use ANTLR-generated parsers from Cobol85Preprocessor.g4 
and Cobol85.g4 to preprocess and parse COBOL code, then print the AST.

Usage:
    python cobol_processor.py <cobol_file>
"""

import os
import sys
import json
import re
from antlr4 import *
from Cobol85PreprocessorLexer import Cobol85PreprocessorLexer
from Cobol85PreprocessorParser import Cobol85PreprocessorParser
from Cobol85PreprocessorListener import Cobol85PreprocessorListener
from Cobol85Lexer import Cobol85Lexer
from Cobol85Parser import Cobol85Parser
from Cobol85Listener import Cobol85Listener
from antlr4.error.ErrorListener import ErrorListener as AntlrErrorListener


# Custom error listener for better error reporting
class ErrorListener(AntlrErrorListener):
    def syntaxError(self, recognizer, offendingSymbol, line, column, msg, e):
        print(f"Error at line {line}:{column} - {msg}")
    
    def reportAmbiguity(self, recognizer, dfa, startIndex, stopIndex, exact, ambigAlts, configs):
        pass
    
    def reportAttemptingFullContext(self, recognizer, dfa, startIndex, stopIndex, conflictingAlts, configs):
        pass
    
    def reportContextSensitivity(self, recognizer, dfa, startIndex, stopIndex, prediction, configs):
        pass


# Custom listener for the preprocessor to collect COPY statements and other directives
class PreprocessorListener(Cobol85PreprocessorListener):
    def __init__(self):
        self.copy_statements = []
        self.replace_statements = []
        self.exec_statements = []
    
    def enterCopyStatement(self, ctx):
        if ctx.copySource() and ctx.copySource().getText():
            self.copy_statements.append({
                "source": ctx.copySource().getText(),
                "line": ctx.start.line
            })
            print(f"Found COPY statement at line {ctx.start.line}: {ctx.copySource().getText()}")
    
    def enterReplaceArea(self, ctx):
        if ctx.replaceByStatement():
            for replace_stmt in ctx.replaceByStatement():
                self.replace_statements.append({
                    "text": replace_stmt.getText(),
                    "line": replace_stmt.start.line
                })
                print(f"Found REPLACE statement at line {replace_stmt.start.line}")
    
    def enterExecSqlStatement(self, ctx):
        # EXEC SQL statements have the SQL text between EXEC SQL and END-EXEC
        sql_text = ctx.getText()
        self.exec_statements.append({
            "type": "SQL",
            "text": sql_text,
            "line": ctx.start.line
        })
        print(f"Found EXEC SQL statement at line {ctx.start.line}")
    
    def enterExecCicsStatement(self, ctx):
        # EXEC CICS statements have the CICS text between EXEC CICS and END-EXEC
        cics_text = ctx.getText()
        self.exec_statements.append({
            "type": "CICS",
            "text": cics_text,
            "line": ctx.start.line
        })
        print(f"Found EXEC CICS statement at line {ctx.start.line}")
        
    def enterExecSqlImsStatement(self, ctx):
        # Handle EXEC SQLIMS statements
        sqlims_text = ctx.getText()
        self.exec_statements.append({
            "type": "SQLIMS",
            "text": sqlims_text,
            "line": ctx.start.line
        })
        print(f"Found EXEC SQLIMS statement at line {ctx.start.line}")

# Custom listener for the main COBOL parser to build the AST
class CobolASTListener(Cobol85Listener):
    def __init__(self):
        self.ast = {
            "program_id": None,
            "divisions": {},
            "sections": {},
            "paragraphs": {},
            "data_items": [],
            "statements": []
        }
        self.current_division = None
        self.current_section = None
        self.current_paragraph = None
    
    def enterProgramIdParagraph(self, ctx):
        if ctx.programName():
            program_id = ctx.programName().getText()
            self.ast["program_id"] = program_id
            print(f"Found PROGRAM-ID: {program_id}")
    
    def enterIdentificationDivision(self, ctx):
        self.current_division = "IDENTIFICATION"
        self.ast["divisions"]["IDENTIFICATION"] = {
            "line": ctx.start.line,
            "sections": {}
        }
        print(f"Found IDENTIFICATION DIVISION at line {ctx.start.line}")
    
    def enterEnvironmentDivision(self, ctx):
        self.current_division = "ENVIRONMENT"
        self.ast["divisions"]["ENVIRONMENT"] = {
            "line": ctx.start.line,
            "sections": {}
        }
        print(f"Found ENVIRONMENT DIVISION at line {ctx.start.line}")
    
    def enterDataDivision(self, ctx):
        self.current_division = "DATA"
        self.ast["divisions"]["DATA"] = {
            "line": ctx.start.line,
            "sections": {}
        }
        print(f"Found DATA DIVISION at line {ctx.start.line}")
    
    def enterProcedureDivision(self, ctx):
        self.current_division = "PROCEDURE"
        self.ast["divisions"]["PROCEDURE"] = {
            "line": ctx.start.line,
            "sections": {},
            "paragraphs": {}
        }
        print(f"Found PROCEDURE DIVISION at line {ctx.start.line}")
    
    def enterDataDescriptionEntry(self, ctx):
        if ctx.dataName() and ctx.levelNumber():
            level = ctx.levelNumber().getText()
            name = ctx.dataName().getText()
            
            data_item = {
                "level": level,
                "name": name,
                "line": ctx.start.line
            }
            
            # Extract picture clause if present
            if ctx.pictureString():
                data_item["picture"] = ctx.pictureString().getText()
            
            # Extract value clause if present
            if ctx.dataValueClause():
                data_item["value"] = ctx.dataValueClause().getText()
            
            self.ast["data_items"].append(data_item)
            print(f"Found data item: {level} {name} at line {ctx.start.line}")
    
    def enterParagraph(self, ctx):
        if ctx.paragraphName() and self.current_division:
            paragraph_name = ctx.paragraphName().getText()
            self.current_paragraph = paragraph_name
            
            if self.current_division not in self.ast["paragraphs"]:
                self.ast["paragraphs"][self.current_division] = {}
            
            self.ast["paragraphs"][self.current_division][paragraph_name] = {
                "line": ctx.start.line,
                "statements": []
            }
            
            print(f"Found paragraph: {paragraph_name} at line {ctx.start.line}")
    
    def enterConfigurationSection(self, ctx):
        if self.current_division == "ENVIRONMENT":
            self.current_section = "CONFIGURATION"
            self.ast["sections"]["CONFIGURATION"] = {
                "division": "ENVIRONMENT",
                "line": ctx.start.line
            }
            print(f"Found CONFIGURATION SECTION at line {ctx.start.line}")
    
    def enterWorkingStorageSection(self, ctx):
        if self.current_division == "DATA":
            self.current_section = "WORKING-STORAGE"
            self.ast["sections"]["WORKING-STORAGE"] = {
                "division": "DATA",
                "line": ctx.start.line
            }
            print(f"Found WORKING-STORAGE SECTION at line {ctx.start.line}")
    
    def enterStatement(self, ctx):
        # Process various statement types
        statement_info = {
            "text": ctx.getText(),
            "line": ctx.start.line
        }
        
        if ctx.displayStatement():
            statement_info["type"] = "DISPLAY"
        elif ctx.moveStatement():
            statement_info["type"] = "MOVE"
        elif ctx.performStatement():
            statement_info["type"] = "PERFORM"
        elif ctx.ifStatement():
            statement_info["type"] = "IF"
        elif ctx.stopStatement():
            statement_info["type"] = "STOP"
        elif ctx.addStatement():
            statement_info["type"] = "ADD"
        elif ctx.computeStatement():
            statement_info["type"] = "COMPUTE"
        else:
            statement_info["type"] = "OTHER"
        
        self.ast["statements"].append(statement_info)
        
        # Add to current paragraph if we're in one
        if self.current_paragraph and self.current_division in self.ast["paragraphs"] and \
           self.current_paragraph in self.ast["paragraphs"][self.current_division]:
            self.ast["paragraphs"][self.current_division][self.current_paragraph]["statements"].append(statement_info)
        
        print(f"Found {statement_info['type']} statement at line {ctx.start.line}")


class CobolPreprocessor:
    def __init__(self, copy_dir="."):
        self.copy_dir = copy_dir
    
    def preprocess(self, input_file):
        """
        Preprocess COBOL code using the Cobol85Preprocessor grammar.
        Handles COPY statements, REPLACE directives, and EXEC blocks.
        """
        print(f"Preprocessing COBOL file: {input_file}")
        
        # Create input stream from file
        input_stream = FileStream(input_file, encoding='utf-8')
        
        # Create lexer and parser with error handling
        lexer = Cobol85PreprocessorLexer(input_stream)
        lexer.removeErrorListeners()
        lexer.addErrorListener(ErrorListener())
        
        token_stream = CommonTokenStream(lexer)
        parser = Cobol85PreprocessorParser(token_stream)
        parser.removeErrorListeners()
        parser.addErrorListener(ErrorListener())
        
        # Parse the input
        tree = parser.startRule()
        
        # Create listener to collect preprocessing directives
        listener = PreprocessorListener()
        walker = ParseTreeWalker()
        walker.walk(listener, tree)
        
        # Process the input file with directives applied
        with open(input_file, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        
        # Process COPY statements
        processed_lines = self._process_copy_statements(lines, listener.copy_statements)
        
        # Process REPLACE directives
        processed_lines = self._process_replace_directives(processed_lines, listener.replace_statements)
        
        # Write preprocessed output to a file
        preprocessed_file = f"{input_file}.preprocessed"
        with open(preprocessed_file, 'w', encoding='utf-8') as f:
            f.writelines(processed_lines)
        
        print(f"Preprocessed output saved to: {preprocessed_file}")
        return preprocessed_file, processed_lines
    
    def _process_copy_statements(self, lines, copy_statements):
        """Process COPY statements by including the referenced files."""
        result_lines = lines.copy()
        
        # Sort copy statements by line number in reverse order to avoid index shifting
        sorted_copies = sorted(copy_statements, key=lambda x: x["line"], reverse=True)
        
        for copy_stmt in sorted_copies:
            line_idx = copy_stmt["line"] - 1
            copy_name = copy_stmt["source"].strip("'\"")
            
            # Find the COPY statement line
            copy_line = None
            for i in range(max(0, line_idx - 2), min(len(result_lines), line_idx + 3)):
                if re.search(r'\bCOPY\b', result_lines[i], re.IGNORECASE):
                    copy_line = i
                    break
            
            if copy_line is not None:
                # Try to find the copy file
                copy_file = self._find_copy_file(copy_name)
                if copy_file:
                    print(f"Including COPY file: {copy_file}")
                    with open(copy_file, 'r', encoding='utf-8') as f:
                        copy_content = f.readlines()
                    
                    # Replace the COPY statement with the file content
                    result_lines[copy_line] = "".join(copy_content)
        
        return result_lines
    
    def _find_copy_file(self, copy_name):
        """Find a COPY file in the copy directory."""
        # Try different extensions and cases
        possible_names = [
            copy_name,
            f"{copy_name}.cpy",
            f"{copy_name}.CPY",
            copy_name.upper(),
            f"{copy_name.upper()}.cpy",
            f"{copy_name.upper()}.CPY"
        ]
        
        for name in possible_names:
            file_path = os.path.join(self.copy_dir, name)
            if os.path.isfile(file_path):
                return file_path
        
        print(f"Warning: Could not find COPY file '{copy_name}'")
        return None
    
    def _process_replace_directives(self, lines, replace_statements):
        """Process REPLACE directives by substituting text."""
        if not replace_statements:
            return lines
        
        result_lines = lines.copy()
        
        for replace_stmt in replace_statements:
            # Extract the replacement patterns (this is simplified)
            # In a real implementation, you would need to parse the REPLACE statement properly
            text = replace_stmt["text"]
            match = re.search(r'REPLACE\s+(.+?)\s+BY\s+(.+?)\.', text, re.IGNORECASE | re.DOTALL)
            if match:
                pattern = match.group(1).strip()
                replacement = match.group(2).strip()
                
                # Apply the replacement to all lines
                for i in range(len(result_lines)):
                    result_lines[i] = result_lines[i].replace(pattern, replacement)
        
        return result_lines


class CobolParser:
    def parse(self, preprocessed_file):
        """
        Parse preprocessed COBOL code using both ANTLR and regex-based parsing.
        Combines the results for a more complete AST.
        """
        print(f"Parsing preprocessed COBOL file: {preprocessed_file}")
        
        # First, clean up the preprocessed file to remove comment lines and fix formatting
        cleaned_file = self._clean_cobol_file(preprocessed_file)
        
        # Get AST from ANTLR parser
        antlr_ast = self._parse_with_antlr(cleaned_file)
        
        # Get AST from regex-based parser
        regex_ast = self._parse_with_regex(preprocessed_file)
        
        # Merge the ASTs
        merged_ast = self._merge_asts(antlr_ast, regex_ast)
        
        # Process SQL statements separately
        self._process_sql_statements(preprocessed_file, merged_ast)
        
        # Clean up temporary file
        try:
            os.remove(cleaned_file)
        except:
            pass
        
        return merged_ast
    
    def _parse_with_antlr(self, cleaned_file):
        """
        Parse COBOL code using ANTLR-generated parser.
        """
        try:
            # Create input stream from the cleaned file
            input_stream = FileStream(cleaned_file, encoding='utf-8')
            
            # Create lexer and parser with error handling
            lexer = Cobol85Lexer(input_stream)
            lexer.removeErrorListeners()
            lexer.addErrorListener(ErrorListener())
            
            token_stream = CommonTokenStream(lexer)
            parser = Cobol85Parser(token_stream)
            parser.removeErrorListeners()
            parser.addErrorListener(ErrorListener())
            
            # Create listener to build AST
            listener = CobolASTListener()
            
            # Parse the input
            tree = parser.compilationUnit()
            
            # Walk the parse tree with our listener
            walker = ParseTreeWalker()
            walker.walk(listener, tree)
            
            print("ANTLR parsing completed successfully")
            return listener.ast
            
        except Exception as e:
            print(f"Error in ANTLR parsing: {str(e)}")
            return {
                "program_id": None,
                "divisions": {},
                "sections": {},
                "paragraphs": {},
                "data_items": [],
                "statements": []
            }
    
    def _merge_asts(self, ast1, ast2):
        """
        Merge two ASTs, preferring non-empty values from either.
        """
        merged = {
            "program_id": ast1.get("program_id") or ast2.get("program_id"),
            "divisions": {},
            "sections": {},
            "paragraphs": {},
            "data_items": [],
            "statements": [],
            "sql_statements": []
        }
        
        # Merge identification info
        if "identification_info" in ast1 or "identification_info" in ast2:
            merged["identification_info"] = {}
            if "identification_info" in ast1:
                merged["identification_info"].update(ast1["identification_info"])
            if "identification_info" in ast2:
                merged["identification_info"].update(ast2["identification_info"])
        
        # Merge configuration
        if "configuration" in ast1 or "configuration" in ast2:
            merged["configuration"] = {}
            if "configuration" in ast1:
                merged["configuration"].update(ast1["configuration"])
            if "configuration" in ast2:
                merged["configuration"].update(ast2["configuration"])
        
        # Merge divisions
        for div_name, div_info in {**ast1.get("divisions", {}), **ast2.get("divisions", {})}.items():
            if div_name not in merged["divisions"]:
                merged["divisions"][div_name] = div_info
            else:
                # Merge sections within divisions
                if "sections" in div_info and "sections" in merged["divisions"][div_name]:
                    merged["divisions"][div_name]["sections"].update(div_info["sections"])
        
        # Merge sections
        for section_name, section_info in {**ast1.get("sections", {}), **ast2.get("sections", {})}.items():
            if section_name not in merged["sections"]:
                merged["sections"][section_name] = section_info
        
        # Merge paragraphs
        for para_name, para_info in {**ast1.get("paragraphs", {}), **ast2.get("paragraphs", {})}.items():
            if para_name not in merged["paragraphs"]:
                merged["paragraphs"][para_name] = para_info
            else:
                # Merge statements within paragraphs
                if "statements" in para_info:
                    merged["paragraphs"][para_name]["statements"].extend(para_info["statements"])
        
        # Merge data items (avoiding duplicates)
        data_items_seen = set()
        for item in ast1.get("data_items", []) + ast2.get("data_items", []):
            item_key = f"{item.get('level', '')}_{item.get('name', '')}"
            if item_key not in data_items_seen:
                merged["data_items"].append(item)
                data_items_seen.add(item_key)
        
        # Merge statements (avoiding duplicates)
        statements_seen = set()
        for stmt in ast1.get("statements", []) + ast2.get("statements", []):
            stmt_key = f"{stmt.get('type', '')}_{stmt.get('line', '')}"
            if stmt_key not in statements_seen:
                merged["statements"].append(stmt)
                statements_seen.add(stmt_key)
        
        # Merge SQL statements (avoiding duplicates)
        sql_seen = set()
        for sql in ast1.get("sql_statements", []) + ast2.get("sql_statements", []):
            sql_key = f"{sql.get('line', '')}"
            if sql_key not in sql_seen:
                merged["sql_statements"].append(sql)
                sql_seen.add(sql_key)
        
        return merged
    
    def _clean_cobol_file(self, input_file):
        """
        Clean up a COBOL file to make it more parser-friendly.
        - Remove comment lines (lines starting with * in column 7)
        - Ensure proper spacing for COBOL divisions and sections
        """
        with open(input_file, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        
        cleaned_lines = []
        for line in lines:
            # Skip comment lines (lines with * in column 7)
            if len(line) > 7 and line[6] == '*':
                continue
            
            # Ensure proper spacing for divisions and sections
            if re.search(r'\b(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b', line, re.IGNORECASE):
                # Ensure division headers are properly formatted
                division = re.search(r'\b(IDENTIFICATION|ENVIRONMENT|DATA|PROCEDURE)\s+DIVISION\b', line, re.IGNORECASE).group(1)
                cleaned_lines.append(f"{division} DIVISION.\n")
            elif re.search(r'\b(\w+)\s+SECTION\b', line, re.IGNORECASE):
                # Ensure section headers are properly formatted
                section = re.search(r'\b(\w+)\s+SECTION\b', line, re.IGNORECASE).group(1)
                cleaned_lines.append(f"{section} SECTION.\n")
            else:
                # Keep other lines as is
                cleaned_lines.append(line)
        
        # Write cleaned lines to a temporary file
        cleaned_file = f"{input_file}.cleaned"
        with open(cleaned_file, 'w', encoding='utf-8') as f:
            f.writelines(cleaned_lines)
        
        return cleaned_file
    
    def _process_sql_statements(self, input_file, ast):
        """
        Process SQL statements in the COBOL code and add them to the AST.
        """
        with open(input_file, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        
        sql_statements = []
        in_sql_block = False
        sql_block = []
        start_line = 0
        
        for i, line in enumerate(lines):
            if re.search(r'\bEXEC\s+SQL\b', line, re.IGNORECASE):
                in_sql_block = True
                sql_block = [line.strip()]
                start_line = i + 1
                continue
            
            if in_sql_block:
                sql_block.append(line.strip())
                if re.search(r'\bEND-EXEC\b', line, re.IGNORECASE):
                    in_sql_block = False
                    sql_statement = " ".join(sql_block)
                    sql_statements.append({
                        "type": "SQL",
                        "text": sql_statement,
                        "line": start_line
                    })
                    print(f"Processed SQL statement at line {start_line}")
        
        # Add SQL statements to the AST
        if "sql_statements" not in ast:
            ast["sql_statements"] = []
        
        ast["sql_statements"].extend(sql_statements)
    
    def _parse_with_regex(self, input_file):
        """
        Fallback regex-based parsing for when ANTLR parsing fails.
        This method extracts a comprehensive AST from COBOL code using regex patterns.
        """
        with open(input_file, 'r', encoding='utf-8') as f:
            lines = f.readlines()
        
        ast = {
            "program_id": None,
            "divisions": {},
            "sections": {},
            "paragraphs": {},
            "data_items": [],
            "statements": [],
            "sql_statements": []
        }
        
        current_division = None
        current_section = None
        current_paragraph = None
        in_sql_block = False
        sql_block = []
        in_perform_block = False
        perform_block_end = None
        
        for i, line in enumerate(lines):
            line_text = line.strip()
            
            # Skip empty lines and comment lines
            if not line_text or (len(line) > 7 and line[6] == '*'):
                continue
            
            # Extract PROGRAM-ID
            program_id_match = re.search(r'\bPROGRAM-ID\.\s+([\w-]+)', line, re.IGNORECASE)
            if program_id_match:
                ast["program_id"] = program_id_match.group(1)
                print(f"Found PROGRAM-ID: {ast['program_id']}")
            
            # Extract AUTHOR
            author_match = re.search(r'\bAUTHOR\.\s+(.+)', line, re.IGNORECASE)
            if author_match and "identification_info" not in ast:
                ast["identification_info"] = {}
                ast["identification_info"]["author"] = author_match.group(1).strip()
                print(f"Found AUTHOR: {ast['identification_info']['author']}")
            
            # Extract divisions
            division_match = re.search(r'\b(\w+)\s+DIVISION\b', line, re.IGNORECASE)
            if division_match:
                current_division = division_match.group(1).upper()
                ast["divisions"][current_division] = {
                    "line": i + 1,
                    "sections": {}
                }
                print(f"Found division: {current_division}")
                continue
            
            # Extract sections
            section_match = re.search(r'\b([\w-]+)\s+SECTION\b', line, re.IGNORECASE)
            if section_match:
                current_section = section_match.group(1).upper()
                if current_division:
                    if "sections" not in ast["divisions"][current_division]:
                        ast["divisions"][current_division]["sections"] = {}
                    
                    ast["divisions"][current_division]["sections"][current_section] = {
                        "line": i + 1,
                        "paragraphs": {}
                    }
                
                ast["sections"][current_section] = {
                    "division": current_division,
                    "line": i + 1
                }
                print(f"Found section: {current_section}")
                continue
            
            # Extract configuration entries
            if current_section == "CONFIGURATION":
                config_match = re.search(r'\b([\w-]+)-COMPUTER\.\s+([\w-]+)', line, re.IGNORECASE)
                if config_match:
                    computer_type = config_match.group(1).upper()
                    computer_value = config_match.group(2)
                    
                    if "configuration" not in ast:
                        ast["configuration"] = {}
                    
                    ast["configuration"][f"{computer_type}_COMPUTER"] = computer_value
                    print(f"Found {computer_type}-COMPUTER: {computer_value}")
            
            # Extract paragraphs (must end with a period and not be a section or division)
            paragraph_match = re.search(r'^\s+([\w-]+)\s*\.', line)
            if paragraph_match and not section_match and not division_match and not line_text.startswith('*') and 'END-EXEC' not in line_text:
                para_name = paragraph_match.group(1).upper()
                
                # Skip if this is actually a statement ending with a period
                if any(stmt in para_name.upper() for stmt in ["MOVE", "DISPLAY", "COMPUTE", "STOP", "ADD", "IF", "ELSE"]):
                    continue
                
                current_paragraph = para_name
                
                # Add to paragraphs collection
                ast["paragraphs"][current_paragraph] = {
                    "division": current_division,
                    "section": current_section,
                    "line": i + 1,
                    "statements": []
                }
                
                # Add to division/section hierarchy
                if current_division and current_section and current_section in ast["divisions"].get(current_division, {}).get("sections", {}):
                    ast["divisions"][current_division]["sections"][current_section]["paragraphs"][current_paragraph] = {
                        "line": i + 1,
                        "statements": []
                    }
                
                print(f"Found paragraph: {current_paragraph}")
                continue
            
            # Extract data items (level numbers followed by names and PIC clauses)
            data_item_match = re.search(r'^\s*(\d+)\s+([\w-]+)(?:\s+PIC\s+([^.]+))?', line, re.IGNORECASE)
            if data_item_match and current_division == "DATA":
                level = data_item_match.group(1)
                name = data_item_match.group(2)
                pic = data_item_match.group(3).strip() if data_item_match.group(3) else None
                
                # Check for VALUE clause
                value_match = re.search(r'VALUE\s+([^.]+)', line, re.IGNORECASE)
                value = value_match.group(1).strip() if value_match else None
                
                # Check for 88 level condition
                is_condition = level.strip() == "88"
                condition_value = None
                if is_condition:
                    condition_match = re.search(r'VALUE\s+[IS]*\s+([^.]+)', line, re.IGNORECASE)
                    if condition_match:
                        condition_value = condition_match.group(1).strip()
                
                data_item = {
                    "level": level,
                    "name": name,
                    "line": i + 1
                }
                
                if pic:
                    data_item["picture"] = pic
                
                if value:
                    data_item["value"] = value
                
                if is_condition and condition_value:
                    data_item["condition_value"] = condition_value
                
                ast["data_items"].append(data_item)
                print(f"Found data item: {level} {name}" + (f" PIC {pic}" if pic else ""))
                continue
            
            # Track SQL blocks
            if re.search(r'\bEXEC\s+SQL\b', line, re.IGNORECASE):
                in_sql_block = True
                sql_block = [line_text]
                sql_start_line = i + 1
                continue
            
            if in_sql_block:
                sql_block.append(line_text)
                if re.search(r'\bEND-EXEC\b', line, re.IGNORECASE):
                    in_sql_block = False
                    sql_statement = " ".join(sql_block)
                    ast["sql_statements"].append({
                        "type": "SQL",
                        "text": sql_statement,
                        "line": sql_start_line
                    })
                    print(f"Found SQL statement at line {sql_start_line}")
                continue
            
            # Extract COBOL statements in PROCEDURE DIVISION
            if current_division == "PROCEDURE" and line_text and not line_text.startswith('*'):
                # Look for common COBOL statements
                statement_type = None
                for stmt_type in ["DISPLAY", "MOVE", "PERFORM", "IF", "EVALUATE", "COMPUTE", "STOP", "ADD", "SUBTRACT", "MULTIPLY", "DIVIDE", "GO TO", "EXIT"]:
                    if re.search(f"\\b{stmt_type}\\b", line_text, re.IGNORECASE):
                        statement_type = stmt_type
                        break
                
                # Handle PERFORM blocks
                if statement_type == "PERFORM" and "VARYING" in line_text.upper():
                    in_perform_block = True
                    perform_match = re.search(r'PERFORM\s+VARYING\s+([\w-]+)\s+FROM\s+(.+?)\s+BY\s+(.+?)\s+UNTIL\s+(.+)', line_text, re.IGNORECASE)
                    if perform_match:
                        var_name = perform_match.group(1)
                        from_val = perform_match.group(2)
                        by_val = perform_match.group(3)
                        until_cond = perform_match.group(4)
                        
                        statement = {
                            "type": "PERFORM VARYING",
                            "text": line_text,
                            "line": i + 1,
                            "variable": var_name,
                            "from": from_val,
                            "by": by_val,
                            "until": until_cond,
                            "paragraph": current_paragraph
                        }
                        
                        ast["statements"].append(statement)
                        
                        # Add to paragraph if we're in one
                        if current_paragraph in ast["paragraphs"]:
                            ast["paragraphs"][current_paragraph]["statements"].append(statement)
                        
                        print(f"Found PERFORM VARYING statement at line {i + 1}")
                
                # Handle IF statements
                elif statement_type == "IF":
                    if_match = re.search(r'IF\s+(.+?)\s+THEN', line_text, re.IGNORECASE) or re.search(r'IF\s+(.+)', line_text, re.IGNORECASE)
                    if if_match:
                        condition = if_match.group(1)
                        
                        statement = {
                            "type": "IF",
                            "text": line_text,
                            "line": i + 1,
                            "condition": condition,
                            "paragraph": current_paragraph
                        }
                        
                        ast["statements"].append(statement)
                        
                        # Add to paragraph if we're in one
                        if current_paragraph in ast["paragraphs"]:
                            ast["paragraphs"][current_paragraph]["statements"].append(statement)
                        
                        print(f"Found IF statement at line {i + 1}")
                
                # Handle other statements
                elif statement_type:
                    statement = {
                        "type": statement_type,
                        "text": line_text,
                        "line": i + 1,
                        "paragraph": current_paragraph
                    }
                    
                    ast["statements"].append(statement)
                    
                    # Add to paragraph if we're in one
                    if current_paragraph in ast["paragraphs"]:
                        ast["paragraphs"][current_paragraph]["statements"].append(statement)
                    
                    print(f"Found {statement_type} statement at line {i + 1}")
                
                # Check for END-PERFORM
                if in_perform_block and "END-PERFORM" in line_text.upper():
                    in_perform_block = False
                    print(f"Found END-PERFORM at line {i + 1}")
        
        return ast


def main():
    if len(sys.argv) < 2:
        print("Usage: python cobol_processor.py <cobol_file>")
        sys.exit(1)
    
    input_file = sys.argv[1]
    
    if not os.path.isfile(input_file):
        print(f"Error: File '{input_file}' not found")
        sys.exit(1)
    
    # Step 1: Preprocess the COBOL file
    preprocessor = CobolPreprocessor()
    preprocessed_file, _ = preprocessor.preprocess(input_file)
    
    # Step 2: Parse the preprocessed COBOL file
    parser = CobolParser()
    ast = parser.parse(preprocessed_file)
    
    # Step 3: Print the AST
    ast_file = f"{input_file}.ast.json"
    with open(ast_file, 'w', encoding='utf-8') as f:
        json.dump(ast, f, indent=2)
    
    print(f"AST saved to: {ast_file}")
    
    # Print the full AST to stdout
    print("\n===== FULL ABSTRACT SYNTAX TREE =====")
    print(json.dumps(ast, indent=2))
    print("===== END OF AST =====\n")
    
    print("COBOL processing complete!")
    print("This demonstrates how Cobol85Preprocessor.g4 and Cobol85.g4 work together:")
    print("1. Preprocessor grammar handles COPY, REPLACE, and EXEC statements")
    print("2. Main grammar parses the program structure (divisions, sections, paragraphs)")
    print("3. The parsed structure is output as a JSON AST for further processing")
    print("\nNote: The script uses ANTLR-generated parsers from both grammars and falls back to")
    print("      regex-based parsing when needed for robustness.")


if __name__ == "__main__":
    main()
