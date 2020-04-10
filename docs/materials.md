# Materials

Improviz now has configurable materials that can be used when drawing shapes. In technical terms, these are just defining the vertex and fragment shaders that are being used when rendering geometries.

The material files themselves are all just YAML files that by default live in the __materials__ folder, but similar to textures and geometries, more folders can be added and the existing ones changed.
The YAML files specify a name, a list of attributes and uniforms that tell Improviz what data needs to be passed to the shaders, and then the vertex and fragment shaders themselves.

## Usage

To use a material, just use the `material` function and pass it the name of a material as a symbol. This works in a similar fashion to other styling functions like `fill` or `stroke`.

```
rotate(time)

material(:ceed)
	cube(3)

material(:weird)
	sphere(3)

move(3)
cylinder(3)
```

To see what materials are available, look the in __materials__ folder of your Improviz installation. The filenames should match the required usage names for each.

## Creating Custom Materials

There are four sets of attribute data and eight uniforms that Improviz can pass to the material shaders.

### Attributes

The attributes are all vertex arrays that are defined by the geometry files loaded or in some cases calculated from that data. They all have to use specific layout locations when using them in a shader.

* position
    The model vertex position coordinates. Uses layout location 0 by default.
* texcoord
    The model vertex texture coordinates. Uses layout location 1 by default.
* barycentric
    The model vertex barycentric coordinates. Currently used mostly for the new style of wireframes, with the idea shamelessly stolen from [Matt DesLauriers](https://www.mattdesl.com) [webgl-wireframe library](https://github.com/mattdesl/webgl-wireframes). Uses layout location 2 by default.
* normals
    The model vertex normals. Uses layout location 3 by default.

### Uniforms

There are eight uniforms that Improviz can pass to the shaders.

* PMatrix
    The 4x4 projection matrix.
* VMatrix
    The 4x4 view matrix.
* MMatrix
    The 4x4 model matrix.
* MVPMat
    The 4x4 combination of the above three matrices.
* Color
    The currently chosen fill colour.
* WireColor
    The currently chosen stroke colour.
* StrokeSize
    The currently chosen stroke size.
* Texture
    The currently chosen texture.
