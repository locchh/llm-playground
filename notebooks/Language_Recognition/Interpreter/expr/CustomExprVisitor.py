if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
    from .ExprVisitor import ExprVisitor
else:
    from ExprParser import ExprParser
    from ExprVisitor import ExprVisitor

class CustomExprVisitor(ExprVisitor):

    # Visit a parse tree produced by ExprParser#root.
    def visitRoot(self, ctx:ExprParser.RootContext):
        l = list(ctx.getChildren())
        print(self.visit(l[0])) # Visit the expr
        return self.visitChildren(ctx)


     # Visit a parse tree produced by ExprParser#Val.
    def visitVal(self, ctx:ExprParser.ValContext):
        return int(ctx.getText())


    def visitDiv(self, ctx:ExprParser.DivContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) / self.visit(l[2])


    # Visit a parse tree produced by ExprParser#Add.
    def visitAdd(self, ctx:ExprParser.AddContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) + self.visit(l[2])


    # Visit a parse tree produced by ExprParser#Sub.
    def visitSub(self, ctx:ExprParser.SubContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) - self.visit(l[2])


    # Visit a parse tree produced by ExprParser#Mul.
    def visitMul(self, ctx:ExprParser.MulContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) * self.visit(l[2])
    