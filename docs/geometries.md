# Geometries

External 3D geometries can be loaded and used by Improviz, and this is actually how the existing shapes in improviz are now loaded.

They can be accessed directly using the `shape` function.

```
rotate()
shape(:teapot, 1, 1, 1)
```

The standard shapes are loaded from the *./geometries* folder in the directory the program is run from. This can be changed, and new folders added, but changing the *config.yaml* file that Improviz loads at startup.
There needs to be a *config.yaml* file within any folder that geometries will be loaded from which specifies the name the shape will be available as, and the path to the geometry file.

The files need to be in the [Wavefront OBJ format](https://en.wikipedia.org/wiki/Wavefront_.obj_file).

## Texture Coordinates

The OBJ files can define coordinates for how the textures are mapped to the geometries. For complex shapes these are best created by the 3D modelling software used to create the geometries, but for simple shapes it's easy enough to do these by hand.

```
v 0 0.5 0
v 0.5 -0.5 0
v -0.5 -0.5 0

f 1/1 2/2 3/3

vt 0.5 0
vt 0 1
vt 1 1
```

This is the geometry file for the Triangle shape. It has :-

* Three vertexes, each with X, Y and Z values
* Three texture coordinates, each with X and Y values
* One face, which links vertices 1, 2 and 3 with texture coordinates 1, 2 and 3 respectively.

It's worth while remembering that because these values are used directly by OpenGL, the vertex coordinates have the Y axis increasing upwards, whilst with the texture coordinates the Y axis increases downwards.
