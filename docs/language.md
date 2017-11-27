# Language Reference

The Improviz language is still changing and being fleshed out. This reference should keep up with it however.

There will be many similarities to LiveCodeLab, but plenty of differences as well.

## Global Variables

*time* is the major global variable and is used for all animation, whether as default variables for commands or used by the user.

*pi*

## Functions

All functions require parentheses, with arguments separated by commas.

A blue rectangle rotating on a red background.
```
background(255, 0, 0)
fill(0, 0, 255)
rotate(time, time/10, 3)
rectangle(2, 1)
```

## Function Definition

```
val = (a) => a + 1
rotate()
fill(255, 0, 0)
box(val(1))
```

## Loops

Loops in improviz work the same as in LiveCodeLab

```
stroke(0, 0, 0)
fill(255, 0, 0)
100 times
	rotate()
	box(8, 8, 8)
```

Optional `with` variable can be used as well.

```
stroke(0, 0, 0)
fill(255, 0, 0)
100 times with i
	rotate()
  move(i)
	box(8, 8, 8)
```

Tabs are used for indentation.

## Blocks

Scoping of commands with blocks also works the same as LiveCodeLab

```
fill(255, 0, 0)
	box()
rotate()
move()
fill(0, 255, 0)
	sphere()
```

## Symbols

A symbol is really just a name. Currently it's only used for giving the name of textures to the texture function. They can be assigned to variables if desired.

```
texture(:crystal)
  box()
move()
texture(:another)
  ball()
```

### Currently Available Functions

#### Shapes
*box* [x, y, z]
Create a box with dimensions x, y and z. Default value for any missed is 1.
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
