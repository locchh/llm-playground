if __name__ is not None and "." in __name__:
    from .LanguageParser import LanguageParser
    from .LanguageVisitor import LanguageVisitor
else:
    from LanguageParser import LanguageParser
    from LanguageVisitor import LanguageVisitor


class CustomLanguageVisitor(LanguageVisitor):

    def __init__(self):
        
        self.stack = []
        myvars = {}
        self.stack.append(myvars)

    def visitRoot(self,ctx):
        for i in list(ctx.getChildren()):
            self.visit(i)

    def visitInss(self,ctx):
        for ins in list(ctx.getChildren()):
            self.visit(ins)
    
    def visitAssign(self, ctx):
        """
        assign: VAR ASSIGN expr
        """
        l = list(ctx.getChildren())
        self.stack[-1][ctx.VAR().getText()] = self.visit(l[2])
    
    def visitWhile_(self, ctx):
        """
        while_: 'while' expr inss
        """
        l = list(ctx.getChildren())
        while self.visit(l[1]) == 1:
            self.visit(l[2])

    def visitOutput(self, ctx):
        """
        output: 'send' expr
        """
        l = list(ctx.getChildren())
        print(self.visit(l[1]))
    
    def visitVar(self,ctx):
        return self.stack[-1][ctx.VAR().getText()]
    
    def visitValue(self, ctx):
        return int(ctx.NUM().getText())

    def visitSub(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) - self.visit(l[2])

    def visitSum(self, ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) + self.visit(l[2])
    
    def visitGt(self, ctx):
        l = list(ctx.getChildren())
        return int(self.visit(l[0]) > self.visit(l[2]))
    
    def visitLt(self, ctx):
        l = list(ctx.getChildren())
        return int(self.visit(l[0]) < self.visit(l[2]))
    
    def visitEq(self, ctx):
        l = list(ctx.getChildren())
        return int(self.visit(l[0]) == self.visit(l[2]))
    
    def visitNeq(self, ctx):
        l = list(ctx.getChildren())
        return int(self.visit(l[0]) != self.visit(l[2]))

    