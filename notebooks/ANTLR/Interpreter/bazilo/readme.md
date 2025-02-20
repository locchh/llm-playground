Here's a simplified explanation of the content:

### What is the Project About?
You are working on a **final project** for a programming masterclass. The task is to create a **double interpreter** for a music programming language called **Bazilio**. This interpreter will:
1. **Interpret Bazilio code** (like a normal programming language interpreter).
2. **Produce music**, creating a melody described in the code.

The output of your interpreter will include:
- A musical **score** (a written representation of the melody).
- Audio files that play the melody in formats like **PDF**, **MIDI**, **WAV**, and **MP3**.

### What is Bazilio?
**Bazilio** is a programming language designed for **algorithmic music composition**. This means it lets you write programs to create music using code. For example, the code will generate:
- Musical **notes** (like B, C, A).
- A **score** that can be saved as a file.

### How Bazilio Works:
- **Programs** in Bazilio are written as a series of **procedures** (similar to functions in programming). 
- Every program starts with a procedure named **Main**.
- Procedures are written between the symbols `|:` and `:|`.  
- **Comments** (notes about the code) are written between triple hashtags (###).

Here’s an example of a simple Bazilio program:

```bazilio
Main |:
     <w> "Hello Bazilio"
(:) {B C A} :|
```

**What Does It Do?**
1. The first line `<w> "Hello Bazilio"`:
   - **<w>** is a command to write text (in this case, "Hello Bazilio"). This is mainly for debugging, not for music creation.
2. The second line `(:) {B C A}`:
   - **(:)** is the play command. It adds notes to the score.
   - `{B C A}` represents the musical notes B, C, and A (using English musical notation).
   - When executed, it will generate a melody corresponding to **Si, La, Do** in English notation.

### What Happens After Running This Code?
When the program is run:
1. It displays the message **"Hello Bazilio"** on the screen.
2. It creates a musical **score** with the notes B, C, and A.
3. It generates several output files:
   - **baz.pdf** (the written score).
   - **baz.midi**, **baz.wav**, and **baz.mp3** (audio files of the melody).

This setup lets composers use Bazilio to generate music and save it in various formats.

In short, **Bazilio is a coding language for creating music**, and your project is about making a tool to run Bazilio code and produce both visual and audio outputs.


---


Here’s `Bazilio.g4`:

```
grammar Bazilio;

root: procDef* EOF;

inss: ins*;
ins: (condition | while_)
    | (input_ | output_ | proc | assign | reprod)
    | (afegit | tall | lookpartitura) ;

input_: '<?>' VAR;     
output_: '<!>' expr+;  

condition: 'if' expr LB inss RB ('else' LB inss RB)?;
while_: 'while' expr LB inss RB;

siz: SIZE VAR;
SIZE: '#';

NOTA: [A-G][0-9]?;


PROCNAME: [A-Z][a-zA-Z0-9_]*;
procDef: PROCNAME paramsId  LB inss RB;
proc: PROCNAME paramsExpr (expr)*;

assign: VAR ASSIGN expr;
ASSIGN: '<-';

lookpartitura: LOOK;
LOOK: 'look';

paramsId: (VAR)*;
paramsExpr: (expr)*;


reprod: REPROD expr;
REPROD: '<:>';

tall: TALLA VAR LS expr RS;
TALLA: '8<';

consult: VAR LS expr RS;


afegit: VAR AFEGIT expr;
AFEGIT: '<<';

lista : '{' expr* '}';

expr: expr MUL expr #Mul
    | expr DIV expr #Div
    | expr MOD expr #Mod
    | expr SUM expr #Sum
    | expr MIN expr #Min
    | expr GT expr  #Gt
    | expr GET expr #Get
    | expr LT expr  #Lt
    | expr LET expr #Let
    | expr EQ expr  #Eq
    | expr NEQ expr #Neq
    | VAR           #Var
    | STRING        #String
    | NUM           #Num
    | lista         #lst
    | siz           #sz
    | consult       #consul        
    | NOTA          #Nota
    | LP expr RP    #Parens ;

LC: '{';
RC: '}';
LB: '|:';
RB: ':|';
LP: '(';
RP: ')';
LS: '[';
RS: ']';

SUM: '+';
MIN: '-';
MUL: '*';
DIV: '/';
MOD: '%';
EQ: '=';
NEQ: '/=';
GT: '>';
LT: '<';
GET: '>=';
LET: '<=';

VAR: [a-zA-Z][a-zA-Z0-9]*;
NUM: '-'?[0-9]+('.'[0-9]+)?;
STRING: '"' ( '\\' . | ~('\\'|'"'))* '"';

COMMENT: '~~~' ~[\r\n]* -> skip;

WS: [ \t\r\n]+ -> skip;
```

Here's `visitor.py`:


```python
import os
import operator
from collections import defaultdict


# Import parser and visitor classes
if __name__ is not None and "." in __name__:
    from .BazilioParser import BazilioParser
    from .BazilioVisitor import BazilioVisitor
else:
    from BazilioParser import BazilioParser
    from BazilioVisitor import BazilioVisitor


class BazilioException(Exception):
    """
    Custom exception class for handling Bazilio-specific errors.
    """
    def __init__(self, message):
        self.message = 'Error: ' + message


class Process:
    """
    Represents a process in Bazilio with its name, parameters, and instructions.
    """
    def __init__(self, name, params, inss):
        self.name = name
        self.params = params
        self.inss = inss


class Visitor(BazilioVisitor):
    """
    Custom visitor class that interprets and executes Bazilio code.
    """
    def __init__(self, entryProc='Main', entryParams=[]):
        """
        Initializes the visitor with an entry procedure and parameters.
        
        Args:
            entryProc (str): The name of the main procedure to execute.
            entryParams (list): Parameters for the main procedure.
        """
        self.entryProc = entryProc
        self.entryParams = entryParams
        self.procs = {}  # Dictionary to store defined processes
        self.stack = []  # Stack to store local variables during execution
        self.score = []  # List to store the music score
        
        # Define note mappings for musical notes
        self.notes = {f"{n}{o}": i for o in range(0, 9) for i, n in enumerate(["C", "D", "E", "F", "G", "A", "B"], start=o * 7)}

    def __proc__(self, name, paramsValues):
        """
        Executes a defined procedure by name with given parameter values.
        
        Args:
            name (str): Name of the procedure to execute.
            paramsValues (list): Values for the procedure's parameters.
        """
        if name not in self.procs:
            raise BazilioException(f'Proc "{name}" is not defined.')
        
        # Error handling for parameter count mismatch
        if len(self.procs[name].params) != len(paramsValues):
            raise BazilioException(
                f'In "{name}" proc was expecting {len(self.procs[name].params)} param(s), '
                f'{len(paramsValues)} param(s) given.'
            )
        
        # Map parameters to values and push onto the stack
        newvars = defaultdict(lambda: 0)
        for param, value in zip(self.procs[name].params, paramsValues):
            newvars[param] = value
        
        self.stack.append(newvars)  # Push local variables onto the stack
        self.visit(self.procs[name].inss)  # Execute procedure instructions
        self.stack.pop()  # Remove local variables after execution

    def visitRoot(self, ctx):
        """
        Visits the root node of the Bazilio code and executes the entry procedure.
        """
        for proc in ctx.procDef():
            self.visit(proc)  # Visit and define all procedures
        self.__proc__(self.entryProc, self.entryParams)  # Execute the entry procedure

    def visitProcDef(self, ctx):
        """
        Visits a procedure definition and stores it in the `procs` dictionary.
        """
        name = ctx.PROCNAME().getText()
        params = self.visit(ctx.paramsId())
        if name in self.procs:
            raise BazilioException(f'Proc "{name}" is already defined.')
        self.procs[name] = Process(name, params, ctx.inss())

    def visitProc(self, ctx):
        """
        Visits a procedure call and executes it.
        """
        name = ctx.PROCNAME().getText()
        params = self.visit(ctx.paramsExpr())
        self.__proc__(name, params)

    def visitParamsId(self, ctx):
        """
        Visits parameter identifiers for a procedure definition.
        """
        return [child.getText() for child in ctx.getChildren()]

    def visitParamsExpr(self, ctx):
        """
        Visits parameter expressions for a procedure call.
        """
        return [self.visit(child) for child in ctx.getChildren()]

    def visitAssign(self, ctx):
        """
        Visits an assignment statement and assigns the result of an expression to a variable.
        """
        var_name = ctx.VAR().getText()
        value = self.visit(ctx.expr())
        self.stack[-1][var_name] = value

    def visitExpr(self, ctx):
        """
        Visits and evaluates an expression node.
        """
        return self.visitChildren(ctx)

    def visitInput_(self, ctx):
        """
        Visits an input statement, prompting the user for a value and assigning it to a variable.
        """
        var_name = ctx.VAR().getText()
        try:
            value = int(input(f"Enter value for {var_name}: "))
            self.stack[-1][var_name] = value
        except ValueError:
            raise BazilioException(f"Invalid input for variable {var_name}.")

    def visitOutput_(self, ctx):
        """
        Visits an output statement and prints the result of one or more expressions.
        """
        results = [self.visit(expr) for expr in ctx.expr()]
        print(" ".join(map(str, results)))

    def visitCondition(self, ctx):
        """
        Visits a conditional statement (if-else) and executes the appropriate block.
        """
        condition = self.visit(ctx.expr())
        if condition:
            self.visit(ctx.inss(0))  # Visit the 'if' block
        elif ctx.else_:
            self.visit(ctx.inss(1))  # Visit the 'else' block, if present

    def visitWhile_(self, ctx):
        """
        Visits a while loop and executes the loop body while the condition is true.
        """
        while self.visit(ctx.expr()):
            self.visit(ctx.inss())

    # Arithmetic operations
    def visitSum(self, ctx):
        return self.visit(ctx.expr(0)) + self.visit(ctx.expr(1))

    def visitSub(self, ctx):
        return self.visit(ctx.expr(0)) - self.visit(ctx.expr(1))

    def visitMul(self, ctx):
        return self.visit(ctx.expr(0)) * self.visit(ctx.expr(1))

    def visitDiv(self, ctx):
        right = self.visit(ctx.expr(1))
        if right == 0:
            raise BazilioException("Division by zero.")
        return self.visit(ctx.expr(0)) / right

    # Variable and literal handling
    def visitVar(self, ctx):
        var_name = ctx.VAR().getText()
        return self.stack[-1][var_name]

    def visitNum(self, ctx):
        return int(ctx.NUM().getText())

    def visitListe(self, ctx):
        return [self.visit(child) for child in ctx.getChildren()[1:-1]]

    def visitNote(self, ctx):
        """
        Visits a musical note and returns its value from the note mappings.
        """
        note = ctx.NOTE().getText()
        return self.notes.get(note, f"Invalid note: {note}")

    # Comparison operations
    def visitLt(self, ctx):
        return int(self.visit(ctx.expr(0)) < self.visit(ctx.expr(1)))

    def visitGt(self, ctx):
        return int(self.visit(ctx.expr(0)) > self.visit(ctx.expr(1)))

    def visitEq(self, ctx):
        return int(self.visit(ctx.expr(0)) == self.visit(ctx.expr(1)))

    def visitNeq(self, ctx):
        return int(self.visit(ctx.expr(0)) != self.visit(ctx.expr(1)))

    def visitGet(self, ctx):
        return int(self.visit(ctx.expr(0)) >= self.visit(ctx.expr(1)))

    def visitLet(self, ctx):
        return int(self.visit(ctx.expr(0)) <= self.visit(ctx.expr(1)))
```