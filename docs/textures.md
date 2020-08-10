# Textures

Improviz supports textures on shapes. These can either be single bitmap images, pngs, or GIFs with one or more frames.

```
texture(:another)
  cube()
move()
fill(255, 0, 0)
  sphere()
```

The program loads the textures at start-up from the specified directories. By default this is the *./textures* folder in the directory the program is run from. This can be changed, or more directories added, by changing the *config.yaml* file settings.

There needs to be a *config.yaml* file within any folder that textures will be loaded from. To add more textures, just drop the image files into the directory and then add update the file.

Each entry for a texture needs to have the name it will be referred to by and the file path within the folder.

Transparency is supported.

It may be better for the texture to have a sizing that's a power of 2. e.g. 256*256  pixels. It supports those that aren't though. Needs more investigation currently.

## Animation

Animated GIFs can be loaded and used. This may be somewhat resource intensive at startup. Individual frames of a GIF can then be used by using the second, *frame* argument of the `texture` function.

```
texture(:loop1, 1)
  cube()
```

This can be animated as necessary.

```
f = (time * 90) % 90
texture(:loop1, f)
  rotate()
  cube()
```

## Code Text

The rendered code is also available as a texture that can be applied to shapes by using the `:code` texture name.
Note: this will override any configuration in texture folders.

```
background(0, 255, 100)

noStroke()
move(3, -3, -10)
texture(:code)
100 times with i
	x = cos(time + (i / 10)) * 0.05
	y = sin(time + (i / 10)) * 0.07
	move(x, y, 0.1)
	rectangle(13)
```
