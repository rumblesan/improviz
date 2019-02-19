# Grammar

This is a very rough grammar for the Improviz language. Highly likely to change and be out of date.

```
program = statement*

statement = (loop | assignment | functionDef | if) eol

loop = expression "times" ("with" identifier)? "newline" block

assignment
  = "var" identifier "=" expression
  | "var" identifier ":=" expression

functionDef
  = "func" identifier "(" argList? ")" "=>" expression
  | "func" identifier "(" argList? ")" "=>" block

argList
  = functionArg
  | functionArg "," arglist

functionArg = identifier | &identifier

block = (<indent> element)+

element = (loop | assignment | if | expression) eol

if = "if" "(" expression ")" block else?
 
else = "else" block

expression = application | variable | value | "(" expression ")"

application = identifier "(" expressionList? ")" block?

expressionList
  = expression
  | expression "," expressionList

variable = identifier

value = number | symbol | null

number = <floating point or integer>

symbol = ":" identifier

null = "null"
```
