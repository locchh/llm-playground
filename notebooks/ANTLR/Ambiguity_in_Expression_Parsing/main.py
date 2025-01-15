from antlr4 import *
from ExpressionLexer import ExpressionLexer
from ExpressionParser import ExpressionParser

# Input expression with potential ambiguity
input_stream = InputStream("1 + 2 * 3")

# Create lexer and parser
lexer = ExpressionLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = ExpressionParser(token_stream)

# Parse and evaluate
tree = parser.prog()
