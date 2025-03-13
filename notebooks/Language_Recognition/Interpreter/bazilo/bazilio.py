import sys
from antlr4 import *
from BazilioLexer import BazilioLexer
from BazilioParser import BazilioParser
from visitor import Visitor

input_stream = FileStream(sys.argv[1],encoding = 'utf-8')
lexer = BazilioLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = BazilioParser(token_stream)
tree = parser.root()

print(tree.toStringTree(recog=parser))

if len(sys.argv) > 2:
    visitor = Visitor(sys.argv[2])
else:
    visitor = Visitor()

visitor.visit(tree)