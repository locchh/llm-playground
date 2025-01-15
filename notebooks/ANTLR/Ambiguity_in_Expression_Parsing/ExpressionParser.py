# Generated from Expression.g4 by ANTLR 4.13.2
# encoding: utf-8
from antlr4 import *
from io import StringIO
import sys
if sys.version_info[1] > 5:
	from typing import TextIO
else:
	from typing.io import TextIO


# This section runs at the start of the parser initialization
print("Initializing Expression Parser with Ambiguity Handling...")

def serializedATN():
    return [
        4,1,6,44,2,0,7,0,2,1,7,1,2,2,7,2,2,3,7,3,1,0,1,0,1,0,1,0,1,1,1,1,
        1,1,1,1,1,1,1,1,5,1,19,8,1,10,1,12,1,22,9,1,1,2,1,2,1,2,1,2,1,2,
        1,2,5,2,30,8,2,10,2,12,2,33,9,2,1,3,1,3,1,3,1,3,1,3,1,3,1,3,3,3,
        42,8,3,1,3,0,0,4,0,2,4,6,0,0,42,0,8,1,0,0,0,2,12,1,0,0,0,4,23,1,
        0,0,0,6,41,1,0,0,0,8,9,3,2,1,0,9,10,5,0,0,1,10,11,6,0,-1,0,11,1,
        1,0,0,0,12,13,3,4,2,0,13,20,6,1,-1,0,14,15,5,1,0,0,15,16,3,4,2,0,
        16,17,6,1,-1,0,17,19,1,0,0,0,18,14,1,0,0,0,19,22,1,0,0,0,20,18,1,
        0,0,0,20,21,1,0,0,0,21,3,1,0,0,0,22,20,1,0,0,0,23,24,3,6,3,0,24,
        31,6,2,-1,0,25,26,5,2,0,0,26,27,3,6,3,0,27,28,6,2,-1,0,28,30,1,0,
        0,0,29,25,1,0,0,0,30,33,1,0,0,0,31,29,1,0,0,0,31,32,1,0,0,0,32,5,
        1,0,0,0,33,31,1,0,0,0,34,35,5,5,0,0,35,42,6,3,-1,0,36,37,5,3,0,0,
        37,38,3,2,1,0,38,39,5,4,0,0,39,40,6,3,-1,0,40,42,1,0,0,0,41,34,1,
        0,0,0,41,36,1,0,0,0,42,7,1,0,0,0,3,20,31,41
    ]

class ExpressionParser ( Parser ):

    grammarFileName = "Expression.g4"

    atn = ATNDeserializer().deserialize(serializedATN())

    decisionsToDFA = [ DFA(ds, i) for i, ds in enumerate(atn.decisionToState) ]

    sharedContextCache = PredictionContextCache()

    literalNames = [ "<INVALID>", "'+'", "'*'", "'('", "')'" ]

    symbolicNames = [ "<INVALID>", "<INVALID>", "<INVALID>", "<INVALID>", 
                      "<INVALID>", "INT", "WS" ]

    RULE_prog = 0
    RULE_expr = 1
    RULE_term = 2
    RULE_factor = 3

    ruleNames =  [ "prog", "expr", "term", "factor" ]

    EOF = Token.EOF
    T__0=1
    T__1=2
    T__2=3
    T__3=4
    INT=5
    WS=6

    def __init__(self, input:TokenStream, output:TextIO = sys.stdout):
        super().__init__(input, output)
        self.checkVersion("4.13.2")
        self._interp = ParserATNSimulator(self, self.atn, self.decisionsToDFA, self.sharedContextCache)
        self._predicates = None



    # Custom method for debugging or resolving ambiguities
    def resolve_ambiguity(self, rule, value):
        print(f"Resolving ambiguity at rule: {rule}, computed value: {value}")
        return value



    class ProgContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser

        def expr(self):
            return self.getTypedRuleContext(ExpressionParser.ExprContext,0)


        def EOF(self):
            return self.getToken(ExpressionParser.EOF, 0)

        def getRuleIndex(self):
            return ExpressionParser.RULE_prog

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterProg" ):
                listener.enterProg(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitProg" ):
                listener.exitProg(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitProg" ):
                return visitor.visitProg(self)
            else:
                return visitor.visitChildren(self)




    def prog(self):

        localctx = ExpressionParser.ProgContext(self, self._ctx, self.state)
        self.enterRule(localctx, 0, self.RULE_prog)
        try:
            self.enterOuterAlt(localctx, 1)
            self.state = 8
            self.expr()
            self.state = 9
            self.match(ExpressionParser.EOF)
            print("Finished parsing program."); 
        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class ExprContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.value = None


        def getRuleIndex(self):
            return ExpressionParser.RULE_expr

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)
            self.value = ctx.value



    class AddContext(ExprContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ExpressionParser.ExprContext
            super().__init__(parser)
            self.e1 = None # TermContext
            self.e2 = None # TermContext
            self.copyFrom(ctx)

        def term(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(ExpressionParser.TermContext)
            else:
                return self.getTypedRuleContext(ExpressionParser.TermContext,i)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterAdd" ):
                listener.enterAdd(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitAdd" ):
                listener.exitAdd(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitAdd" ):
                return visitor.visitAdd(self)
            else:
                return visitor.visitChildren(self)



    def expr(self):

        localctx = ExpressionParser.ExprContext(self, self._ctx, self.state)
        self.enterRule(localctx, 2, self.RULE_expr)
        self._la = 0 # Token type
        try:
            localctx = ExpressionParser.AddContext(self, localctx)
            self.enterOuterAlt(localctx, 1)
            self.state = 12
            localctx.e1 = self.term()
            localctx.value =  localctx.e1.value
            self.state = 20
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==1:
                self.state = 14
                self.match(ExpressionParser.T__0)
                self.state = 15
                localctx.e2 = self.term()
                localctx.value =  self.resolve_ambiguity("addition", localctx.value + localctx.e2.value) 
                self.state = 22
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class TermContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.value = None


        def getRuleIndex(self):
            return ExpressionParser.RULE_term

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)
            self.value = ctx.value



    class MultiplyContext(TermContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ExpressionParser.TermContext
            super().__init__(parser)
            self.e1 = None # FactorContext
            self.e2 = None # FactorContext
            self.copyFrom(ctx)

        def factor(self, i:int=None):
            if i is None:
                return self.getTypedRuleContexts(ExpressionParser.FactorContext)
            else:
                return self.getTypedRuleContext(ExpressionParser.FactorContext,i)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterMultiply" ):
                listener.enterMultiply(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitMultiply" ):
                listener.exitMultiply(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitMultiply" ):
                return visitor.visitMultiply(self)
            else:
                return visitor.visitChildren(self)



    def term(self):

        localctx = ExpressionParser.TermContext(self, self._ctx, self.state)
        self.enterRule(localctx, 4, self.RULE_term)
        self._la = 0 # Token type
        try:
            localctx = ExpressionParser.MultiplyContext(self, localctx)
            self.enterOuterAlt(localctx, 1)
            self.state = 23
            localctx.e1 = self.factor()
            localctx.value =  localctx.e1.value
            self.state = 31
            self._errHandler.sync(self)
            _la = self._input.LA(1)
            while _la==2:
                self.state = 25
                self.match(ExpressionParser.T__1)
                self.state = 26
                localctx.e2 = self.factor()
                localctx.value =  self.resolve_ambiguity("multiplication", localctx.value * localctx.e2.value) 
                self.state = 33
                self._errHandler.sync(self)
                _la = self._input.LA(1)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx


    class FactorContext(ParserRuleContext):
        __slots__ = 'parser'

        def __init__(self, parser, parent:ParserRuleContext=None, invokingState:int=-1):
            super().__init__(parent, invokingState)
            self.parser = parser
            self.value = None


        def getRuleIndex(self):
            return ExpressionParser.RULE_factor

     
        def copyFrom(self, ctx:ParserRuleContext):
            super().copyFrom(ctx)
            self.value = ctx.value



    class IntegerContext(FactorContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ExpressionParser.FactorContext
            super().__init__(parser)
            self._INT = None # Token
            self.copyFrom(ctx)

        def INT(self):
            return self.getToken(ExpressionParser.INT, 0)

        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterInteger" ):
                listener.enterInteger(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitInteger" ):
                listener.exitInteger(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitInteger" ):
                return visitor.visitInteger(self)
            else:
                return visitor.visitChildren(self)


    class ParenthesesContext(FactorContext):

        def __init__(self, parser, ctx:ParserRuleContext): # actually a ExpressionParser.FactorContext
            super().__init__(parser)
            self.e = None # ExprContext
            self.copyFrom(ctx)

        def expr(self):
            return self.getTypedRuleContext(ExpressionParser.ExprContext,0)


        def enterRule(self, listener:ParseTreeListener):
            if hasattr( listener, "enterParentheses" ):
                listener.enterParentheses(self)

        def exitRule(self, listener:ParseTreeListener):
            if hasattr( listener, "exitParentheses" ):
                listener.exitParentheses(self)

        def accept(self, visitor:ParseTreeVisitor):
            if hasattr( visitor, "visitParentheses" ):
                return visitor.visitParentheses(self)
            else:
                return visitor.visitChildren(self)



    def factor(self):

        localctx = ExpressionParser.FactorContext(self, self._ctx, self.state)
        self.enterRule(localctx, 6, self.RULE_factor)
        try:
            self.state = 41
            self._errHandler.sync(self)
            token = self._input.LA(1)
            if token in [5]:
                localctx = ExpressionParser.IntegerContext(self, localctx)
                self.enterOuterAlt(localctx, 1)
                self.state = 34
                localctx._INT = self.match(ExpressionParser.INT)
                localctx.value =  int((None if localctx._INT is None else localctx._INT.text)) 
                pass
            elif token in [3]:
                localctx = ExpressionParser.ParenthesesContext(self, localctx)
                self.enterOuterAlt(localctx, 2)
                self.state = 36
                self.match(ExpressionParser.T__2)
                self.state = 37
                localctx.e = self.expr()
                self.state = 38
                self.match(ExpressionParser.T__3)
                localctx.value =  self.resolve_ambiguity("parentheses", localctx.e.value) 
                pass
            else:
                raise NoViableAltException(self)

        except RecognitionException as re:
            localctx.exception = re
            self._errHandler.reportError(self, re)
            self._errHandler.recover(self, re)
        finally:
            self.exitRule()
        return localctx





