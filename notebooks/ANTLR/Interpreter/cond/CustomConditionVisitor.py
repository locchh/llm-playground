if __name__ is not None and "." in __name__:
    from .ConditionParser import ConditionParser
    from .ConditionVisitor import ConditionVisitor
else:
    from ConditionParser import ConditionParser
    from ConditionVisitor import ConditionVisitor

class CustomConditionVisitor(ConditionVisitor):

    def visitRoot(self, ctx:ConditionParser.RootContext):
        for action in list(ctx.getChildren()):
            self.visit(action)
    
    def visitCondition(self, ctx:ConditionParser.ConditionContext):
        l = list(ctx.getChildren())
        if self.visit(l[1])==1:
            self.visit(l[2])
        else:
            if (len(l)==5) and (ctx.getChild(3).getText()=='otherwise'):
                self.visit(ctx.action(1))

    def visitPrint(self, ctx:ConditionParser.PrintContext):
        l = list(ctx.getChildren())
        print(self.visit(l[1]))

    def visitNext(self, ctx:ConditionParser.NextContext):
        l = list(ctx.getChildren())
        value = self.visit(l[1])
        next_value = value + 1
        print(next_value)

    def visitSum(self, ctx:ConditionParser.SumContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) + self.visit(l[2])
    
    def visitValue(self, ctx:ConditionParser.ValueContext):
        return int(ctx.NUM().getText())

    def visitLt(self, ctx:ConditionParser.LtContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) < self.visit(l[2])

    def visitNeq(self, ctx:ConditionParser.NeqContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) != self.visit(l[2])

    def visitEq(self, ctx:ConditionParser.EqContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) == self.visit(l[2])

    def visitGt(self, ctx:ConditionParser.GtContext):
        l = list(ctx.getChildren())
        return self.visit(l[0]) > self.visit(l[2])