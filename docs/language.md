# Language

The Improviz language is still changing somewhat but should be fairly stable at this point. This reference should keep up with it.

The most basic part of writing code with Improviz is calling functions that either draw shapes to the screen, or change those shapes in some way, with each of these functions being on their own separate line.

The [reference document](./reference.md) lists all of the available, functions what they do, and what values they can be given.

## Calling Functions

To call a function, you need to have its name, followed by a pair of parentheses `()`. Within these you place any values that will change the way the function behaves, separated by commas.

A blue rectangle rotating on a red background.

```
background(255, 0, 0)
fill(0, 0, 255)
rotate()
rectangle(2, 1)
```

## Loops

Loops allow you to repeatedly call some lines of code. The piece of code that's repeated is called a *block* and it must be indented with a tab.

```
stroke(0, 0, 0)
fill(255, 0, 0)
100 times
	rotate()
	cube(8, 8, 8)
```

Optional `with` variable can be used as well.

```
stroke(0, 0, 0)
fill(255, 0, 0)
n = 100
n times with i
	rotate()
  move(i)
	cube(8, 8, 8)
```

## Comments

Any text that comes after two slashes `//` is considered a comment and won't be run

```
cube() //this is a comment
//sphere()  this line won't run
```

## Time

*time* is the major global variable and is used for all animation, whether as default variables for commands or used by the user. It is the only value that actually changes between frames.

```
rotate(1, 2, 3)
cube()       // this cube won't move
rotate(time)
sphere()     // this sphere will move
```

## Assignment

If you want to calculate a value and then use it in multiple places, you can assign it to a variable.

```
a = 3 + 4
cube(a)
```

Variables can have their value re-assigned.

```
a = 3 + 4
cube(a)
a = 2
sphere(a)
```

Conditional assignment is also available, and will set a variable only if it currently doesn't have a value.
This is mostly useful when defining functions and setting defaults for the arguments that are passed in.

```
a = 3
a := 4
cube(a) // cube will be of size 3
```

## If

Control flow can be done using `if`, `elif` and `else`. The number 0 is considered False, with anything else being considered True.

```
10 times with x
  if (x % 3 < 1)
    fill(255, 0, 0)
  elif (x % 3 < 2)
    fill(0, 255, 0)
  else
    fill(0, 255, 0)
  rotate()
  rectangle(8)
```

## Function Definition

Functions are defined using the `func` keyword, followed by a name, argument list and then either an indented block or an arrow and single line expression

```
func val(a) => a + 1

func draw(r, g, b)
  rotate()
  fill(r, g, b)
  cube(val(1))

draw(255, 0, 0)
```

## Function Blocks

When a function is called in can be passed an optional block.
If the function has a **BlockArg** argument then this block is available to be used within the function body.

```
func myf(x, y, &blk)
	if (isNull(blk))
		sphere()
	else
		move(x, y, 0)
			blk()

myf(1, 1)
	rotate()
	cube()
```

The **BlockArg** must start with an ampersand *(&)* symbol.

## First-Class Functions

Functions can be passed as values.

```
func draw(f)
  rotate()
  fill(r, g, b)
  f(3)

draw(cube)
```

## Lists

Improviz supports basic lists, though currently they have a fixes size once declared.

```
sizes = [1,2,3,2+2]
s = sizes[(time * 4) % length(sizes)]

rotate(time)
cube(s)
```

They can be created using square brackets and accessed also using the square brackets. The `length` function is available to return the length of a list as a number.

## Saving and Loading GFX state

The built in functions **pushScope** and **popScope** can be used to save and load snapshots of the style and transformation state on a stack.

```
pushScope()
rotate()
cube()
popScope()
move(1,0,0)
sphere()
```

This state will include the stroke and fill styling, as well as the matrix manipulations.

This feature is used in conjuction with the function blocks to allow simplified scoping of some commands.

```
fill(255, 0, 0)
	cube()
rotate()
move()
fill(0, 255, 0)
	sphere()
```

## Symbols

A symbol is really just a name and is primarily used for giving the name of textures to the texture function. They can be assigned to variables if desired.

```
texture(:crystal)
  cube()
move()

t = :another
texture(t)
  ball()
```
