# Generated from Expression.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .ExpressionParser import ExpressionParser
else:
    from ExpressionParser import ExpressionParser

# This section runs at the start of the parser initialization
print("Initializing Expression Parser with Ambiguity Handling...")


# This class defines a complete listener for a parse tree produced by ExpressionParser.
class ExpressionListener(ParseTreeListener):

    # Enter a parse tree produced by ExpressionParser#prog.
    def enterProg(self, ctx:ExpressionParser.ProgContext):
        pass

    # Exit a parse tree produced by ExpressionParser#prog.
    def exitProg(self, ctx:ExpressionParser.ProgContext):
        pass


    # Enter a parse tree produced by ExpressionParser#Add.
    def enterAdd(self, ctx:ExpressionParser.AddContext):
        pass

    # Exit a parse tree produced by ExpressionParser#Add.
    def exitAdd(self, ctx:ExpressionParser.AddContext):
        pass


    # Enter a parse tree produced by ExpressionParser#Multiply.
    def enterMultiply(self, ctx:ExpressionParser.MultiplyContext):
        pass

    # Exit a parse tree produced by ExpressionParser#Multiply.
    def exitMultiply(self, ctx:ExpressionParser.MultiplyContext):
        pass


    # Enter a parse tree produced by ExpressionParser#Integer.
    def enterInteger(self, ctx:ExpressionParser.IntegerContext):
        pass

    # Exit a parse tree produced by ExpressionParser#Integer.
    def exitInteger(self, ctx:ExpressionParser.IntegerContext):
        pass


    # Enter a parse tree produced by ExpressionParser#Parentheses.
    def enterParentheses(self, ctx:ExpressionParser.ParenthesesContext):
        pass

    # Exit a parse tree produced by ExpressionParser#Parentheses.
    def exitParentheses(self, ctx:ExpressionParser.ParenthesesContext):
        pass



del ExpressionParser