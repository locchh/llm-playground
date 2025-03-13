import operator

if __name__ is not None and "." in __name__:
    from .ExprParser import ExprParser
    from .ExprVisitor import ExprVisitor
else:
    from ExprParser import ExprParser
    from ExprVisitor import ExprVisitor

ops = {"+":operator.add,
       "-":operator.sub,
       "*":operator.mul,
       "/":operator.truediv,
       "^":operator.pow}

class CustomExprVisitor(ExprVisitor):

    def __init__(self):
        self.myvars = {}

    # Visit a parse tree produced by ExprParser#root.
    def visitRoot(self, ctx:ExprParser.RootContext):
        l = list(ctx.getChildren())
        # Visit each action
        for i in range(len(l)-1):
            print(self.visit(l[i]))
        return self.visitChildren(ctx)
    
    # Visit a parse tree produced by ExprParser#expr.
    def visitExpr(self, ctx:ExprParser.ExprContext):
        l = list(ctx.getChildren())
        # if that is a value
        if len(l) == 1:
            return int(l[0].getText())
        # else if that operation
        else:
            return ops[l[1].getText()](int(self.visit(l[0])), int(self.visit(l[2])))

    def visitAction(self,ctx):
        l = list(ctx.getChildren())
        if len(l) == 3: # ASSIGNMENT
            if (l[1].getText() == ':='):
                self.myvars[l[0].getText()] = self.visit(l[2])
                return 'assignment to ' + l[0].getText()
            else:
                return 'ERROR'
        
        else: # len(l) == 2  We have a PRINT
            if (l[0].getText() == 'write'):
                return "The value of " + l[1].getText() + " is: " + str(self.myvars[l[1].getText()])
            else:
                return 'ERROR'
