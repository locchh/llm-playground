# Generated from Arithmetic.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .ArithmeticParser import ArithmeticParser
else:
    from ArithmeticParser import ArithmeticParser

# Define any imports or initialization code here
print("Initializing Arithmetic Parser...")


# This class defines a complete listener for a parse tree produced by ArithmeticParser.
class ArithmeticListener(ParseTreeListener):

    # Enter a parse tree produced by ArithmeticParser#prog.
    def enterProg(self, ctx:ArithmeticParser.ProgContext):
        pass

    # Exit a parse tree produced by ArithmeticParser#prog.
    def exitProg(self, ctx:ArithmeticParser.ProgContext):
        pass


    # Enter a parse tree produced by ArithmeticParser#Add.
    def enterAdd(self, ctx:ArithmeticParser.AddContext):
        pass

    # Exit a parse tree produced by ArithmeticParser#Add.
    def exitAdd(self, ctx:ArithmeticParser.AddContext):
        pass


    # Enter a parse tree produced by ArithmeticParser#Multiply.
    def enterMultiply(self, ctx:ArithmeticParser.MultiplyContext):
        pass

    # Exit a parse tree produced by ArithmeticParser#Multiply.
    def exitMultiply(self, ctx:ArithmeticParser.MultiplyContext):
        pass


    # Enter a parse tree produced by ArithmeticParser#Integer.
    def enterInteger(self, ctx:ArithmeticParser.IntegerContext):
        pass

    # Exit a parse tree produced by ArithmeticParser#Integer.
    def exitInteger(self, ctx:ArithmeticParser.IntegerContext):
        pass


    # Enter a parse tree produced by ArithmeticParser#Parentheses.
    def enterParentheses(self, ctx:ArithmeticParser.ParenthesesContext):
        pass

    # Exit a parse tree produced by ArithmeticParser#Parentheses.
    def exitParentheses(self, ctx:ArithmeticParser.ParenthesesContext):
        pass



del ArithmeticParser