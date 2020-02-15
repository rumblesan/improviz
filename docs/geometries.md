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
