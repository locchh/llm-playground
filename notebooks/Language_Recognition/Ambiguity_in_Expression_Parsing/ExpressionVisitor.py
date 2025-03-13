# Generated from Expression.g4 by ANTLR 4.13.2
from antlr4 import *
if "." in __name__:
    from .ExpressionParser import ExpressionParser
else:
    from ExpressionParser import ExpressionParser

# This section runs at the start of the parser initialization
print("Initializing Expression Parser with Ambiguity Handling...")


# This class defines a complete generic visitor for a parse tree produced by ExpressionParser.

class ExpressionVisitor(ParseTreeVisitor):

    # Visit a parse tree produced by ExpressionParser#prog.
    def visitProg(self, ctx:ExpressionParser.ProgContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpressionParser#Add.
    def visitAdd(self, ctx:ExpressionParser.AddContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpressionParser#Multiply.
    def visitMultiply(self, ctx:ExpressionParser.MultiplyContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpressionParser#Integer.
    def visitInteger(self, ctx:ExpressionParser.IntegerContext):
        return self.visitChildren(ctx)


    # Visit a parse tree produced by ExpressionParser#Parentheses.
    def visitParentheses(self, ctx:ExpressionParser.ParenthesesContext):
        return self.visitChildren(ctx)



del ExpressionParser