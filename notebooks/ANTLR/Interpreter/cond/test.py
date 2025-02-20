import sys
from antlr4 import *

# from ExprLexer import ExprLexer
# from ExprParser import ExprParser
# from CustomExprVisitor import CustomExprVisitor

from ConditionLexer import ConditionLexer
from ConditionParser import ConditionParser
from CustomConditionVisitor import CustomConditionVisitor

# Take user input
input_stream = FileStream(sys.argv[1])

# Pass the input stream to the lexer
lexer = ConditionLexer(input_stream)

# Create a token stream from the lexer
token_stream = CommonTokenStream(lexer)

# Pass the token stream to the parser
parser = ConditionParser(token_stream)

# Parse the input and get the parse tree
tree = parser.root()

# Print the parse tree as a string
print(tree.toStringTree(recog=parser))

# Visit Abstract syntax tree
visitor = CustomConditionVisitor()
visitor.visit(tree)
