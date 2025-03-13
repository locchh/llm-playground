from antlr4 import *
from ExprLexer import ExprLexer
from ExprParser import ExprParser
from CustomExprVisitor import CustomExprVisitor

# Take user input
input_stream = InputStream(input('? '))

# Pass the input stream to the lexer
lexer = ExprLexer(input_stream)

# Create a token stream from the lexer
token_stream = CommonTokenStream(lexer)

# Pass the token stream to the parser
parser = ExprParser(token_stream)

# Parse the input and get the parse tree
tree = parser.root()

# Print the parse tree as a string
#print(tree.toStringTree(recog=parser))

# Visit Abstract syntax tree
visitor = CustomExprVisitor()
visitor.visit(tree)
