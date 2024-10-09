# Q1

Converting a Lisp-like expression to JSON can be achieved by transforming the nested parentheses structure of the Lisp syntax into a key-value pair structure, where lists become arrays and symbols or atoms become keys and values.

### Example:

#### Lisp-like Expression:

```lisp
(person
    (name "John Doe")
    (age 30)
    (address
        (street "123 Main St")
        (city "New York")
        (state "NY")))
```

### Conversion Logic:

- Symbols (like `person`, `name`, `age`, etc.) become JSON keys.
- Strings (like `"John Doe"`, `"New York"`) remain as string values.
- Numbers (like `30`) remain as numeric values.
- Nested parentheses become JSON objects or arrays.

### Equivalent JSON:

```json
{
  "person": {
    "name": "John Doe",
    "age": 30,
    "address": {
      "street": "123 Main St",
      "city": "New York",
      "state": "NY"
    }
  }
}
```

### Explanation:
1. The outermost `person` becomes a key in the JSON object.
2. Nested lists are turned into sub-objects or arrays within that key.
3. Each symbol (e.g., `name`, `age`) inside is treated as a key, with the following values assigned as JSON values.

You can use a custom parser or regular expression-based approach to transform such Lisp-like expressions to JSON. Would you like an example of how to implement such a converter in Python or another language?


# Q2

Here’s a Python function that converts a Lisp-like expression to JSON. The function will take a string containing a Lisp-like expression and output a corresponding JSON object.

We can use a recursive approach to parse the nested parentheses structure.

### Python Function:

```python
import json

def parse_lisp_to_json(lisp_str):
    def tokenize(s):
        """Tokenizes the input string into meaningful units."""
        return s.replace('(', ' ( ').replace(')', ' ) ').split()

    def parse(tokens):
        """Recursively parses the tokens into nested lists."""
        if len(tokens) == 0:
            raise SyntaxError("Unexpected EOF")
        token = tokens.pop(0)
        
        if token == '(':
            result = []
            while tokens[0] != ')':
                result.append(parse(tokens))
            tokens.pop(0)  # pop off ')'
            return result
        elif token == ')':
            raise SyntaxError("Unexpected )")
        else:
            return atom(token)

    def atom(token):
        """Converts a token into an atom: int, float, or string."""
        try:
            return int(token)
        except ValueError:
            try:
                return float(token)
            except ValueError:
                return token.strip('"')

    def lisp_to_dict(lisp_expr):
        """Converts a Lisp-like expression (as a list) into a dictionary."""
        if isinstance(lisp_expr, list) and len(lisp_expr) > 1:
            key = lisp_expr[0]
            if isinstance(lisp_expr[1], list):
                return {key: lisp_to_dict(lisp_expr[1:])}
            else:
                # Handle key-value pairs
                obj = {}
                for i in range(1, len(lisp_expr), 2):
                    k = lisp_expr[i]
                    v = lisp_expr[i + 1]
                    if isinstance(v, list):
                        obj[k] = lisp_to_dict(v)
                    else:
                        obj[k] = v
                return {key: obj}
        return lisp_expr

    # Tokenize the input string
    tokens = tokenize(lisp_str)

    # Parse the tokenized string into a nested list structure
    parsed_expr = parse(tokens)

    # Convert the nested list structure to a JSON-like dict structure
    json_obj = lisp_to_dict(parsed_expr)
    
    # Return the JSON representation (you can convert it to string with json.dumps if needed)
    return json_obj

# Example usage
lisp_expr = '(person (name "John Doe") (age 30) (address (street "123 Main St") (city "New York") (state "NY")))' 
json_result = parse_lisp_to_json(lisp_expr)
print(json.dumps(json_result, indent=2))  # Pretty print JSON
```

### How it works:
1. **Tokenizer:** Breaks down the Lisp-like input string into tokens (parentheses, symbols, strings, and numbers).
2. **Parser:** Recursively parses these tokens into a nested list representation.
3. **Converter:** The `lisp_to_dict` function takes the parsed list and converts it into a JSON-compatible dictionary.

### Example Output:

For the input:
```lisp
(person (name "John Doe") (age 30) (address (street "123 Main St") (city "New York") (state "NY")))
```

The output will be:

```json
{
  "person": {
    "name": "John Doe",
    "age": 30,
    "address": {
      "street": "123 Main St",
      "city": "New York",
      "state": "NY"
    }
  }
}
```

This function should work for most simple Lisp-like expressions. Let me know if you need further customization!

# Q3

In ANTLR with Python, the `context` object (typically called a `ParserRuleContext`) provides access to various tokens and subrules during parsing. To retrieve all tokens, you can make use of the following strategies depending on your specific use case.

Here’s how you can retrieve tokens in different ways:

### 1. **Accessing Tokens via the Context Object Directly**
When you define your grammar, the generated context objects will have fields corresponding to the rule's subcomponents (tokens or other rules). For example, if you want to get a specific token from a context, you can access it directly via the context's attributes.

If you have a rule like this in your grammar:
```antlr
expr: ID '=' expr | INT ;
```
The generated context class (e.g., `ExprContext`) will have attributes like `ID()` or `INT()`, which represent tokens.

For example, in your listener or visitor method:
```python
def enterExpr(self, ctx: MyGrammarParser.ExprContext):
    # Access ID token
    id_token = ctx.ID()
    if id_token:
        print("ID:", id_token.getText())
    
    # Access INT token
    int_token = ctx.INT()
    if int_token:
        print("INT:", int_token.getText())
```

### 2. **Using `getTokens` Method to Retrieve All Tokens of a Certain Type**
You can use `getTokens(tokenType)` to retrieve all tokens of a particular type from a context. This requires you to pass the token type, which can be found in the generated parser/lexer.

Example:
```python
def enterExpr(self, ctx: MyGrammarParser.ExprContext):
    # Get all tokens of a certain type (e.g., INT)
    int_tokens = ctx.getTokens(MyGrammarParser.INT)
    for token in int_tokens:
        print("INT token:", token.getText())
```

### 3. **Accessing All Tokens in a Context Using `start` and `stop`**
The context object has a `start` and `stop` property, which point to the first and last tokens matched by the rule. You can use the `tokenStream` from the parser to retrieve all tokens within this range.

Here’s how you can get all tokens within a context:

```python
def enterExpr(self, ctx: MyGrammarParser.ExprContext):
    # Get the token stream from the parser
    token_stream = ctx.parser.getTokenStream()

    # Get the start and stop tokens from the context
    start_token = ctx.start.tokenIndex
    stop_token = ctx.stop.tokenIndex

    # Get all tokens within this range
    tokens = token_stream.getTokens(start_token, stop_token)

    for token in tokens:
        print(f"Token text: {token.text}, Token type: {token.type}")
```

### 4. **Getting All Tokens from the Entire Input**
If you want to retrieve all tokens from the input, not just within a single context, you can access the token stream directly from the parser.

```python
def enterExpr(self, ctx: MyGrammarParser.ExprContext):
    token_stream = ctx.parser.getTokenStream()
    all_tokens = token_stream.getTokens()

    for token in all_tokens:
        print(f"Token text: {token.text}, Token type: {token.type}")
```

### Summary of Key Methods:
- **`ctx.getTokens(tokenType)`**: Get all tokens of a specific type.
- **`ctx.getToken(tokenType, i)`**: Get the i-th token of a specific type.
- **`ctx.start` and `ctx.stop`**: Access the first and last token in the context.
- **`tokenStream.getTokens(startIndex, stopIndex)`**: Get tokens within a range.

Would you like an example specific to a certain type of grammar rule you're working with?

# Q4

Converting the structure of the current context in ANTLR-Python to JSON or Lisp-like format requires recursively traversing the parse tree. The parse tree is represented by `ParserRuleContext` objects, and each rule in your grammar creates a corresponding context object that can be visited.

Here's how you can approach converting a context structure to JSON or Lisp-like format:

### 1. **Converting Context to JSON:**

You can recursively walk the `ParserRuleContext` and gather information like rule names and child tokens, then format that into a JSON structure.

#### Example:

```python
import json
from antlr4 import ParserRuleContext

def context_to_json(ctx: ParserRuleContext):
    """Recursively convert ANTLR context to a JSON-compatible structure."""
    # Get the rule name (useful to know which rule we're dealing with)
    rule_name = ctx.parser.ruleNames[ctx.getRuleIndex()]
    
    # Dictionary to store the current rule structure
    result = {"rule": rule_name, "children": []}
    
    # Traverse child nodes
    for i in range(ctx.getChildCount()):
        child = ctx.getChild(i)
        
        if isinstance(child, ParserRuleContext):
            # Recursively process child rules
            result["children"].append(context_to_json(child))
        else:
            # Add tokens (like terminal nodes, literals, etc.)
            result["children"].append({"token": child.getText()})
    
    return result

# Example usage in your ANTLR visitor or listener
def enterExpr(self, ctx: MyGrammarParser.ExprContext):
    json_structure = context_to_json(ctx)
    print(json.dumps(json_structure, indent=2))
```

### Explanation:
- The `context_to_json` function takes a context (`ParserRuleContext`) as input.
- It retrieves the name of the rule that created the context.
- It iterates through all the children of the context (which can either be other rules or terminal tokens).
- For rules, it recursively calls `context_to_json` and adds the result to the `children` list.
- For terminal tokens, it directly adds the token text to the `children` list.

The output will look like this:

#### Example JSON Output:
```json
{
  "rule": "expr",
  "children": [
    {
      "token": "a"
    },
    {
      "token": "+"
    },
    {
      "rule": "expr",
      "children": [
        {
          "token": "b"
        }
      ]
    }
  ]
}
```

### 2. **Converting Context to Lisp-like Format:**

In Lisp-like syntax, nested expressions are represented as lists, where the first element is the rule name (or operation), followed by its arguments (which can be tokens or other nested expressions). You can modify the recursive function to output Lisp-like structures.

#### Example:

```python
def context_to_lisp(ctx: ParserRuleContext):
    """Recursively convert ANTLR context to a Lisp-like structure."""
    # Get the rule name
    rule_name = ctx.parser.ruleNames[ctx.getRuleIndex()]
    
    # Build the Lisp-like structure with the rule name
    result = [rule_name]
    
    # Traverse child nodes
    for i in range(ctx.getChildCount()):
        child = ctx.getChild(i)
        
        if isinstance(child, ParserRuleContext):
            # Recursively process child rules
            result.append(context_to_lisp(child))
        else:
            # Add tokens (like terminal nodes, literals, etc.)
            result.append(child.getText())
    
    return result

def lisp_format(lisp_structure):
    """Format the lisp-like structure as a string."""
    if isinstance(lisp_structure, list):
        return '(' + ' '.join(lisp_format(x) for x in lisp_structure) + ')'
    else:
        return str(lisp_structure)

# Example usage in your ANTLR visitor or listener
def enterExpr(self, ctx: MyGrammarParser.ExprContext):
    lisp_structure = context_to_lisp(ctx)
    print(lisp_format(lisp_structure))
```

### Explanation:
- The `context_to_lisp` function recursively builds a list where the first element is the rule name and the subsequent elements are its children.
- The `lisp_format` function then formats this list structure as a proper Lisp-like string.
  
#### Example Lisp-like Output:
For an input expression like `a + b`, the output might look like this:

```lisp
(expr a + (expr b))
```

### Additional Notes:
- These examples assume you're working inside a listener or visitor and have access to a context object like `ctx`.
- The `ruleNames` property from the parser is used to retrieve the name of the rule based on the rule index.
- For more complex grammars, you can add additional logic to handle specific token types or rules.

This approach allows you to represent the parse tree structure in JSON or Lisp-like formats, which can be useful for debugging or further processing. Let me know if you need more specific examples for your grammar!