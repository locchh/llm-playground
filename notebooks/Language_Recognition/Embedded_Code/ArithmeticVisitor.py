# Generated from Arithmetic.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .ArithmeticParser import ArithmeticParser
else:
    from ArithmeticParser import ArithmeticParser

# Define any imports or initialization code here
print("Initializing Arithmetic Parser...")


# This class defines a complete generic visitor for a parse tree produced by ArithmeticParser.

class ArithmeticVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by ArithmeticParser#prog.
    def visitProg(self, ctx:ArithmeticParser.ProgContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ArithmeticParser#Add.
    def visitAdd(self, ctx:ArithmeticParser.AddContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ArithmeticParser#Multiply.
    def visitMultiply(self, ctx:ArithmeticParser.MultiplyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ArithmeticParser#Integer.
    def visitInteger(self, ctx:ArithmeticParser.IntegerContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ArithmeticParser#Parentheses.
    def visitParentheses(self, ctx:ArithmeticParser.ParenthesesContext):
        return self.visitChildren(ctx)



del ArithmeticParser