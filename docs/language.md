# Language Reference

The Improviz language is still changing and being fleshed out. This reference should keep up with it however.

There will be many similarities to LiveCodeLab, but plenty of differences as well.

## Global Variables

*time* is the major global variable and is used for all animation, whether as default variables for commands or used by the user.

*pi*

## Assignment

There are two types of assignment.

Absolute assignment will set a variable identifier to a value as usual.

`var a = 3 + 4`

Conditional assignment will set a variable only if it currently doesn't have a value.
This is mostly useful when expecting to have values set via OSC messages which may not have arrived yet, and so avoid `variable undefined` errors.

```
var a = 3
var a := 4
cube(a) // cube will be of size 3
```

## Function Application

All functions require parentheses, with arguments separated by commas.

A blue rectangle rotating on a red background.

```
background(255, 0, 0)
fill(0, 0, 255)
rotate(time, time/10, 3)
rectangle(2, 1)
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

## Function Application Blocks

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

## Loops

Loops in Improviz work the same as in LiveCodeLab

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
var n = 100
n times with i
	rotate()
  move(i)
	cube(8, 8, 8)
```

Tabs are used for indentation.

## Saving and Loading GFX state

The built in functions **pushScope** and **popScope** can be used to save and load snapshots of the GFX engine state on a stack.

```
pushScope()
rotate()
cube()
popScope()
move(1,0,0)
sphere()
```

This state will include the stroke and fill styling, as well as the matrix manipulations.

This feature is used in conjuction with the function blocks to allow simplified scoping of some commands in much the same way as LiveCodeLab.

```
fill(255, 0, 0)
	cube()
rotate()
move()
fill(0, 255, 0)
	sphere()
```

## Symbols

A symbol is really just a name. Currently it's only used for giving the name of textures to the texture function. They can be assigned to variables if desired.

```
texture(:crystal)
  cube()
move()

var t = :another
texture(t)
  ball()
```

### Currently Available Functions

#### Shapes
*cube* [x, y, z]
Create a cube with dimensions x, y and z. Default value for any missed is 1.
*sphere* [x, y, z]
Create a sphere with dimensions x, y and z. Default value for any missed is 1.
*cylinder* [x, y, z]
Create a cylinder with dimensions x, y and z. Default value for any missed is 1.
*rectangle* [x, y]
Create a flat rectangle with dimensions x and y. Default value for any missed is 1.
*line* [length]
Create a line of the desired length. Default value is 1.

#### Movement
*rotate* [x, y, z]
*scale* [x, y, z]
*move* [x, y, z]

#### Styling
*fill* [r, g, b, a]
*texture* [name, frame]
*noFill* []
*stroke* [r, g, b, a]
*noStroke* []
*background* [r, g, b]

#### Animation Style
*paintOver* []
*motionBlur* []

#### Maths
*sin* [rads]
*cos* [rads]
*tan* [rads]
*abs* [val]
*ceil* [val]
*floor* [val]
*round* [val]
*max* [val]
*min* [val]
*log* [val]
*sqrt* [val]
