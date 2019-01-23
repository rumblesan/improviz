# Grammar

This is a very rough grammar for the Improviz language. Highly likely to change and be out of date.

```
program = block

block = (<indent> element eol)+

element = loop | assignment | functionDef | if | expression

loop = expression "times" ("with" identifier)? "newline" block

assignment
  = "var" identifier "=" expression
  | "var" identifier ":=" expression

functionDef
  = "func" identifier "(" argList? ")" "=>" expression
  | "func" identifier "(" argList? ")" "=>" block

argList
  = identifier
  | identifier "," arglist

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
