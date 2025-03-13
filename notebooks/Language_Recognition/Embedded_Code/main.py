from antlr4 import *
from ArithmeticLexer import ArithmeticLexer
from ArithmeticParser import ArithmeticParser

input_stream = InputStream("3 + 5 * (2 + 4)")

lexer = ArithmeticLexer(input_stream)
token_stream = CommonTokenStream(lexer)
parser = ArithmeticParser(token_stream)

tree = parser.prog()
