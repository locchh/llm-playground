import operator
import os
from collections import defaultdict
ops = {'+': operator.add, '-': operator.sub, '*': operator.mul,
       '/': operator.truediv, '^': operator.pow}

if __name__ is not None and "." in __name__:
    from .BazilioParser import BazilioParser
    from .BazilioVisitor import BazilioVisitor
else:
    from BazilioParser import BazilioParser
    from BazilioVisitor import BazilioVisitor


class BazilioException(Exception):
        def __init__(self, message):
            self.message = 'Error: ' + message


class Proceso:
    def __init__(self, name, params, inss):
        self.name = name
        self.params = params
        self.inss = inss

class Visitor(BazilioVisitor):
    
    def __init__(self,entryProc = 'Main', entryParams = []):
        
        self.entryProc = entryProc
        self.entryParams = entryParams 
        
        
        self.procs = {}
        self.stack = []
        self.parti = []
        
        self.notes = {}
        self.notes["A0"]=0
        self.notes["B0"]=1
        self.notes["C1"]=2
        self.notes["D1"]=3
        self.notes["E1"]=4
        self.notes["F1"]=5
        self.notes["G1"]=6
        self.notes["A1"]=7
        self.notes["B1"]=8
        self.notes["C2"]=9
        self.notes["D2"]=10
        self.notes["E2"]=11
        self.notes["F2"]=12
        self.notes["G2"]=13
        self.notes["A2"]=14
        self.notes["B2"]=15
        self.notes["C3"]=16
        self.notes["D3"]=17
        self.notes["E3"]=18
        self.notes["F3"]=19
        self.notes["G3"]=20
        self.notes["A3"]=21
        self.notes["B3"]=22
        self.notes["C4"]=23
        self.notes["D4"]=24
        self.notes["E4"]=25
        self.notes["F4"]=26
        self.notes["G4"]=27
        self.notes["A4"]=28
        self.notes["B4"]=29
        self.notes["C5"]=30
        self.notes["D5"]=31
        self.notes["E5"]=32
        self.notes["F5"]=33
        self.notes["G5"]=34
        self.notes["A5"]=35
        self.notes["B5"]=36
        self.notes["C6"]=37
        self.notes["D6"]=38
        self.notes["E6"]=39
        self.notes["F6"]=40
        self.notes["G6"]=41
        self.notes["A6"]=42
        self.notes["B6"]=43
        self.notes["C7"]=44
        self.notes["D7"]=45
        self.notes["E7"]=46
        self.notes["F7"]=47
        self.notes["G7"]=48
        self.notes["A7"]=49
        self.notes["B7"]=50
        self.notes["C8"]=51
        
        self.adins = False

              
    def __proc__(self, name, paramsValues):
        # error handling may be an option
        if len(self.procs[name].params) != len(paramsValues):
            raise BazilioException('En \"' + name + '\" proc se esperaba ' + str(len(
                self.procs[name].params)) + ' param(s), ' + str(len(paramsValues)) + ' param(s) dados.')
        
        newvars = defaultdict(lambda: 0)
        for param, value in zip(self.procs[name].params,paramsValues):
            newvars[param] = value
        self.stack.append(newvars)  # Ho afegim
        self.visit(self.procs[name].inss)
        self.stack.pop()    # Desempilem
        
        
    def visitRoot(self, ctx):
        for proc in list(ctx.getChildren()):
            self.visit(proc)
        
        self.__proc__(self.entryProc,self.entryParams)
        
        # TIME TO GENERATE THE SACRED LILYPOND :)
        
        absolute_path = os.path.dirname(os.path.abspath(__file__))
        notes_partituraMajuscules = ' '.join(map(str,self.parti))
        notes = notes_partituraMajuscules.lower()
        
        file = open(absolute_path + "/music.ly", "w")
        file.write("\\version \"2.20.0\"" + os.linesep)
        file.write("\score {" + os.linesep)
        file.write("\t \\absolute {" + os.linesep)
        file.write("\t\t" + "\\tempo 4 = 120" + os.linesep)
        file.write("\t\t" + notes + os.linesep)
        file.write("\t }" + os.linesep)
        file.write("\t \layout { }" + os.linesep)
        file.write("\t \midi { }" + os.linesep)
        file.write("}")
        file.close()
        
        os.system('lilypond music.ly')
        os.system('timidity -Ow -o music.wav music.midi')
        os.system('ffmpeg -i music.wav -codec:a libmp3lame -qscale:a 2 music.mp3')
        
            
    def visitInss(self,ctx):
        for ins in list(ctx.getChildren()):
            self.visit(ins)
            
        
    def visitIns(self,ctx):
        return self.visitChildren(ctx)
    
    
    def visitParamsId(self,ctx):
        lista = []
        for p in list(ctx.getChildren()):
            lista.append(p.getText())
        
        return lista
    
            
    def visitParamsExpr(self,ctx):
        lista = []
        for p in list(ctx.getChildren()):
            lista.append(self.visit(p))
        
        return lista
    
    
    def visitInput_(self,ctx):
        self.stack[-1][ctx.getChild(1).getText()] = int(input())
        
        
    def visitOutput_(self,ctx):
        l = list(ctx.getChildren())
        for e in l[1:]:
            a = self.visit(e)
            if isinstance(a,list):
                b = str(a)
                b = b.replace(",","")
                b = b.replace("'","")
                if (e != l[-1]):
                    print(b,end=" ")
                else:
                    print(b)
            else:
                if (e != l[-1]):
                    print(self.visit(e),end=" ")
                else:
                    print(self.visit(e))
                
        
    def visitCondition(self,ctx):
        l = list(ctx.getChildren())
        if self.visit(l[1]) == 1:
            self.visit(l[3])
        elif len(l) > 5:
            if ctx.getChild(5).getText() == 'else':
                self.visit(ctx.inss(1))
                
            
    def visitWhile_(self, ctx):
        l = list(ctx.getChildren())
        while self.visit(l[1]) == 1:
            self.visit(l[3])
            
            
    def visitAssign(self, ctx):
        l = list(ctx.getChildren())
        self.stack[-1][ctx.VAR().getText()] = self.visit(l[2])
        
    
    def visitReprod(self, ctx):
        l = list(ctx.getChildren())
        k = self.visit(l[1])
        t = []
        if isinstance(k,list):
            for nota_pre in k:
                nota_pre = nota_pre[:1] + "'" + nota_pre[1:]
                t.append(nota_pre)
            self.parti.extend(t)
        else:
            t = k[:1] + "'" + k[1:]
            self.parti.append(t)
                
        
        # Instruccion (extension) extra, permite ver la partitura en tiempo de ejecucion
    def visitLookpartitura(self, ctx):
        if not(len(self.parti)): print("Ninguna nota en la partitura")
        else:
            a = str(self.parti)
            a = a.replace(",","")
            a = a.replace('"','')
            print(a)
            
 
    def visitProc(self,ctx):
        children = list(ctx.getChildren())
        name = children[0].getText()
        parametros = (self.visit(children[1]))

        
        if name in self.procs:
            self.__proc__(name,parametros)
        else:
            raise BazilioException('Proc \"' + name + '\" no definido.')
        
            
    def visitProcDef (self,ctx):
        children = list(ctx.getChildren())
        name = children[0].getText()
        parametros = self.visit(children[1])
        if name in self.procs:
            raise BazilioException('Proc \"' + name + '\" ya definido.')
        
        else:
            self.procs[name] = Proceso(name,parametros,ctx.inss())
            
        
    def visitString(self,ctx):
        l = list(ctx.getChildren())
        s =  l[0].getText()
        return s[1:-1]
    
    
    def visitMul(self,ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) * self.visit(l[2])
    
    
    def visitDiv(self,ctx):
        l = list(ctx.getChildren())
        den = self.visit(l[2])
        if den == 0:
            raise BazilioException('Division by zero.')
        return self.visit(l[0]) / den
    
    
    def visitVar(self,ctx):
        return self.stack[-1][ctx.VAR().getText()]
    
        
    def visitNum(self,ctx):
        return int(ctx.NUM().getText())
    
    
    def visitLista(self, ctx):
        l = list(ctx.getChildren())
        values = [self.visit(child) for child in l[1:-1]]
        return values
    
    
    def visitSiz(self, ctx):
        l = list(ctx.getChildren())
        size = len(self.stack[-1][ctx.VAR().getText()])
        return size
    
        
    def visitAfegit(self, ctx):
        l = list(ctx.getChildren())
        element = self.visit(l[2]);
        self.stack[-1][ctx.VAR().getText()].append(element);
        
        
    def visitTall(self, ctx):
        l = list(ctx.getChildren())
        i = self.visit(ctx.expr()) # element i-esim a eliminar
        size = len(self.stack[-1][ctx.VAR().getText()])
        if i < 1 or i > (size):
            raise BazilioException('indice ' + str(i) + ' no pertenece a la lista ' + ctx.VAR().getText())
        else:
            del((self.stack[-1][ctx.VAR().getText()])[i-1])
            
            
    def visitConsult(self, ctx):
        l = list(ctx.getChildren())
        i = self.visit(ctx.expr()) # element i-esim a eliminar
        size = len(self.stack[-1][ctx.VAR().getText()])
        if i < 1 or i > (size):
            raise BazilioException('indice ' + str(i) + ' no pertenece a la lista ' + ctx.VAR().getText())
        else:
            return((self.stack[-1][ctx.VAR().getText()])[i-1])
        
    
    def visitNota(self, ctx):
        nota = ctx.NOTA().getText()
        nota_por_defect = nota+"4"
        if len(nota) == 1:
            return nota_por_defect
        else:
            return nota
        
        return ctx.NOTA().getText()
    
    
    def visitParens(self,ctx):
        l = list(ctx.getChildren())
        return self.visit(l[1])
    
    
    #Metode important per saber la nota a partir de la seva constant
    def getkey(self,val):
        for key,value in self.notes.items():
            if val == value:
                return key
    
    
    def visitSum(self,ctx):
        l = list(ctx.getChildren())
        
        #Sumem nota_pura + enter
        if (ctx.getChild(0).getText() in self.notes.keys()):
            nota = ctx.expr(0).getText()
            val1 = self.notes[nota]
            result = val1 + self.visit(l[2])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Sumem enter + nota_pura
        elif (ctx.getChild(2).getText() in self.notes.keys()):
            nota = ctx.expr(1).getText()
            val1 = self.notes[nota]
            result = val1 + self.visit(l[0])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Sumem nota_variable + enter
        elif (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys()):
            nota = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota]
            result = val1 + self.visit(l[2])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Sumem enter + nota_variable
        elif (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys()):
            nota = self.stack[-1][ctx.expr(1).getText()]
            val1 = self.notes[nota]
            result = val1 + self.visit(l[0])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Sumem enters
        else:
            return self.visit(l[0]) + self.visit(l[2]) 
     

    def visitLt(self,ctx):
        l = list(ctx.getChildren())
        
        a = (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys())
        b = (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys())
        
        # nota_variable < nota_variable
        if a and b:
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(0).getText()]
            val2 = self.notes[nota2]
            return int(val1 >= val2)
        
         # nota_variable < nota_pura
        if (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys()):
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = ctx.expr(1).getText()
            val2 = self.notes[nota2]
            return int(val1 < val2)
         # nota_pura < nota_variable
        if (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys()):
            self.adins = True
            nota1 = ctx.expr(0).getText()
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(1).getText()]
            val2 = self.notes[nota2]
            return int(val1 < val2)
        # enter < enter
        else:
            if (not self.adins):
                return int(self.visit(l[0]) < self.visit(l[2]))
            else:
                return
            
        self.adins = False
    
    def visitGt(self,ctx):
        l = list(ctx.getChildren())
        
        a = (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys())
        b = (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys())
        
        # nota_variable >= nota_variable
        if a and b:
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(0).getText()]
            val2 = self.notes[nota2]
            return int(val1 > val2)
        
         # nota_variable > nota_pura
        if (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys()):
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = ctx.expr(1).getText()
            val2 = self.notes[nota2]
            return int(val1 > val2)
         # nota_pura > nota_variable
        if (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys()):
            self.adins = True
            nota1 = ctx.expr(0).getText()
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(1).getText()]
            val2 = self.notes[nota2]
            return int(val1 > val2)
        # enter > enter
        else:
            if (not self.adins):
                return int(self.visit(l[0]) > self.visit(l[2]))
            else:
                return
            
        self.adins = False
    
    def visitEq(self,ctx):
        l = list(ctx.getChildren())
        
        a = (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys())
        b = (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys())
        
        # nota_variable == nota_variable
        if a and b:
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(0).getText()]
            val2 = self.notes[nota2]
            return int(val1 == val2)
        
         # nota_variable == nota_pura
        if (a):
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = ctx.expr(1).getText()
            val2 = self.notes[nota2]
            return int(val1 == val2)
         # nota_pura == nota_variable
        if (b):
            self.adins = True
            nota1 = ctx.expr(0).getText()
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(1).getText()]
            val2 = self.notes[nota2]
            return int(val1 == val2)
        # enter == enter
        else:
            if (not self.adins):
                return int(self.visit(l[0]) == self.visit(l[2]))
            else:
                return
            
        self.adins = False
    
    def visitNeq(self,ctx):
        l = list(ctx.getChildren())
        
        a = (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys())
        b = (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys())
        
        # nota_variable == nota_variable
        if a and b:
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(0).getText()]
            val2 = self.notes[nota2]
            return int(val1 != val2)
        
         # nota_variable == nota_pura
        if (a):
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = ctx.expr(1).getText()
            val2 = self.notes[nota2]
            return int(val1 != val2)
         # nota_pura == nota_variable
        if (b):
            self.adins = True
            nota1 = ctx.expr(0).getText()
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(1).getText()]
            val2 = self.notes[nota2]
            return int(val1 != val2)
        # enter == enter
        else:
            if (not self.adins):
                return int(self.visit(l[0]) != self.visit(l[2]))
            else:
                return
            
        self.adins = False
        
    
    def visitGet(self,ctx):
        l = list(ctx.getChildren())
        
        a = (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys())
        b = (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys())
        
        # nota_variable >= nota_variable
        if a and b:
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(0).getText()]
            val2 = self.notes[nota2]
            return int(val1 >= val2)
        
         # nota_variable >= nota_pura
        if (a):
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = ctx.expr(1).getText()
            val2 = self.notes[nota2]
            return int(val1 >= val2)
         # nota_pura >= nota_variable
        if (b):
            self.adins = True
            nota1 = ctx.expr(0).getText()
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(1).getText()]
            val2 = self.notes[nota2]
            return int(val1 >= val2)
        #enter >= enter
        else:
            if (not self.adins):
                return int(self.visit(l[0]) >= self.visit(l[2]))
            else:
                return
            
        self.adins = False
        
    
    def visitLet(self,ctx):
        l = list(ctx.getChildren())
        
        a = (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys())
        b = (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys())
        
        # nota_variable <= nota_variable
        if a and b:
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(0).getText()]
            val2 = self.notes[nota2]
            return int(val1 <= val2)
        
        
        # nota_variable <= nota_pura
        if (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys()):
            self.adins = True
            nota1 = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota1]
            nota2 = ctx.expr(1).getText()
            val2 = self.notes[nota2]
            return int(val1 <= val2)
        # nota_pura <= nota_variable
        if (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys()):
            self.adins = True
            nota1 = ctx.expr(0).getText()
            val1 = self.notes[nota1]
            nota2 = self.stack[-1][ctx.expr(1).getText()]
            val2 = self.notes[nota2]
            return int(val1 <= val2)
        # enter <= enter
        else:
            if (not self.adins):
                return int(self.visit(l[0]) <= self.visit(l[2]))
            else:
                return
            
        self.adins = False
        
    
    def visitMin(self,ctx):
        l = list(ctx.getChildren())
        #Restem notes
        if (ctx.getChild(0).getText() in self.notes.keys()):
            nota = ctx.expr(0).getText()
            val1 = self.notes[nota]
            result = val1 - self.visit(l[2])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Restem notes
        elif (ctx.getChild(2).getText() in self.notes.keys()):
            nota = ctx.expr(1).getText()
            val1 = self.notes[nota]
            result = val1 - self.visit(l[0])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Restem notes en variables
        elif (self.stack[-1][ctx.expr(0).getText()] in self.notes.keys()):
            nota = self.stack[-1][ctx.expr(0).getText()]
            val1 = self.notes[nota]
            result = val1 - self.visit(l[2])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Restem notes en variables
        elif (self.stack[-1][ctx.expr(1).getText()] in self.notes.keys()):
            nota = self.stack[-1][ctx.expr(1).getText()]
            val1 = self.notes[nota]
            result = val1 - self.visit(l[0])
            for key,value in self.notes.items():
                if result == value:
                    nova_nota = key
                    return nova_nota
        #Restem enters
        else:
            return self.visit(l[0]) - self.visit(l[2]) 
        
    
    def visitMod(self,ctx):
        l = list(ctx.getChildren())
        return self.visit(l[0]) % self.visit(l[2])
