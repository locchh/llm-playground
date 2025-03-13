**Common Errors in ANTLR Parsing and Their Explanations**

ANTLR (Another Tool for Language Recognition) is a powerful tool for parsing and analyzing structured text. However, users often encounter various errors when working with ANTLR grammars. Below is a comprehensive guide explaining common errors in ANTLR parsing and their meanings.

---

### 1. **Mismatched Input Error**
**Error Message:**
```
mismatched input '<token>' expecting '<expected-token>'
```
**Explanation:**
Occurs when the parser encounters an unexpected token. This usually happens due to incorrect grammar rules or missing tokens in the input.

**Solution:**
- Check if the grammar rule correctly defines the expected input.
- Ensure that all necessary tokens are declared in the lexer.
- Verify if any optional tokens should be included.

---

### 2. **No Viable Alternative Error**
**Error Message:**
```
no viable alternative at input '<token>'
```
**Explanation:**
This error occurs when ANTLR cannot determine which rule to follow due to ambiguity or incorrect syntax in the input.

**Solution:**
- Use **left-factoring** to remove ambiguity.
- Check if required tokens are missing in the input.
- Review rule definitions and ensure they are unambiguous.

---

### 3. **Token Recognition Error**
**Error Message:**
```
token recognition error at: '<invalid-token>'
```
**Explanation:**
Occurs when the lexer cannot recognize a given input token.

**Solution:**
- Ensure all valid characters and patterns are defined in lexer rules.
- Check for typos or missing character sets in the grammar.
- Use `skip` or `channel(HIDDEN)` to ignore unwanted tokens.

---

### 4. **Extraneous Input Error**
**Error Message:**
```
extraneous input '<token>' expecting '<expected-token>'
```
**Explanation:**
ANTLR detects an unexpected token that does not fit the current parsing rule.

**Solution:**
- Check if optional tokens are incorrectly included.
- Adjust grammar rules to accommodate additional tokens.

---

### 5. **Early Exit Error**
**Error Message:**
```
early exit from rule <rule-name>
```
**Explanation:**
The parser exits a rule earlier than expected, usually due to an empty alternative or missing required input.

**Solution:**
- Verify if `+`, `*`, or `?` quantifiers are used appropriately.
- Ensure the grammar allows for all expected input sequences.

---

### 6. **Failed Predicate Error**
**Error Message:**
```
rule <rule-name> failed predicate: {<condition>}?
```
**Explanation:**
Occurs when a semantic predicate inside a rule evaluates to `false`.

**Solution:**
- Review semantic conditions inside `{}`.
- Ensure conditions are correctly formulated.

---

### 7. **Missing Token Error**
**Error Message:**
```
missing '<expected-token>' at '<token>'
```
**Explanation:**
The parser expects a certain token but does not find it.

**Solution:**
- Ensure input data follows expected syntax.
- Adjust grammar rules to properly define token sequences.

---

### 8. **Rule Recursion Limit Exceeded**
**Error Message:**
```
too many nested calls to <rule-name>
```
**Explanation:**
Occurs due to left-recursion or infinite recursion in grammar rules.

**Solution:**
- Eliminate **direct** left recursion.
- Convert left-recursive rules to **right-recursive** or **iteration-based** rules.

---

### 9. **Ambiguous Grammar Warning**
**Error Message:**
```
ambiguity between alternatives <alt1> and <alt2> of rule <rule-name>
```
**Explanation:**
ANTLR detects multiple parsing paths that can match the same input.

**Solution:**
- Resolve ambiguity using **explicit lookahead (`LL(k)`)**.
- Use **left-factoring** to differentiate alternatives.

---

### 10. **Lexer Mode Conflict**
**Error Message:**
```
implicit token definition conflicts with lexer mode <mode-name>
```
**Explanation:**
Happens when a token is implicitly used in multiple modes.

**Solution:**
- Define mode-specific token rules using `mode`.
- Ensure token assignments are consistent.

---

### Conclusion
Understanding these errors and their resolutions will significantly improve the debugging process when working with ANTLR. Proper grammar structuring, clear token definitions, and ambiguity resolution are key to successful parsing.

For further debugging, enable **error listeners** and **debug mode** in ANTLR to get more insights into parsing issues.

---

**References:**
- [ANTLR 4 Documentation](https://www.antlr.org/)