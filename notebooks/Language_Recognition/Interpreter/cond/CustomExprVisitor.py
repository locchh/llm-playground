if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
    from .ExprVisitor import ExprVisitor
else:
    from ExprParser import ExprParser
    from ExprVisitor import ExprVisitor

class CustomExprVisitor(ExprVisitor):

    def visitRoot(self, ctx:ExprParser.RootContext):
        for i in list(ctx.getChildren()):
            self.visit(i)
    
    def visitCondition(self, ctx:ExprParser.ConditionContext):
        l = list(ctx.getChildren())
        if self.visit(l[1])==1:
            self.visit(ctx.action(0))
        else:
            if len(l) == 5:
                self.visit(ctx.action(1))
    
    def visitPrint(self, ctx:ExprParser.PrintContext):
        l = list(ctx.getChildren())
        print(self.visit(l[1]))
    
    def visitValue(self, ctx:ExprParser.ValueContext):
        return int(ctx.NUM().getText())


    def visitLt(self, ctx:ExprParser.LtContext):
        l = list(ctx.getChildren())
        return int(self.visit(l[0]) < self.visit(l[2]))


    def visitGt(self, ctx:ExprParser.GtContext):
        l = list(ctx.getChildren())
        return int(self.visit(l[0]) > self.visit(l[2]))