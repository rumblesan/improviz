# Materials

Improviz now has configurable materials that can be used when drawing shapes. In technical terms, these are just defining the vertex and fragment shaders that are being used when rendering geometries.

The material files themselves are all just YAML files that by default live in the __materials__ folder, but similar to textures and geometries, more folders can be added and the existing ones changed.
The YAML files just need to define the vertex and fragment shaders that will be used when rendering the geometries.

## Usage

To use a material, just use the `material` function and pass it the name of a material as a symbol. This works in a similar fashion to other styling functions like `fill` or `stroke`.

```
rotate(time)

material(:ceed)
	cube(3)

material(:barycentric)
	sphere(3)

move(3)
cylinder(3)
```

To see what materials are available, look the in __materials__ folder of your Improviz installation. The filenames should match the required usage names for each.

## Creating Custom Materials

There are four sets of attribute data and eight uniforms that Improviz makes available to the material shaders by default. It is also possible to pass custom uniform values to shaders for further control in user defined materials.

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

There are eight uniforms that Improviz can pass to the shaders. There isn't anything special about these being capitalised, they just match the uniform names defined in the GLSL code.

* MVPmatrix
    The 4x4 combination of the above three matrices.
* Mmatrix
    The 4x4 model matrix.
* Vmatrix
    The 4x4 view matrix.
* Pmatrix
    The 4x4 projection matrix.
* FillColour
    The currently chosen fill colour.
* StrokeColour
    The currently chosen stroke colour.
* StrokeSize
    The currently chosen stroke size.
* Texture
    The currently chosen texture.

### Custom Uniforms

If a custom material defines a uniform other than those above, the value can be set using the `materialVar` function. Improviz will automatically detect what uniforms have been defined in the GLSL code and will expect the same names to be used when passing values in.

For example, the __wobbler__ material has uniforms for `Time`, `Freq` and `Depth` as well as those for `Texture` and `MVPmatrix`, and so you would need to use the material in the following way.

```
texture(:algorave)
materialVar(:Time, time/3)
materialVar(:Freq, 8)
materialVar(:Depth, 0.2)
material(:wobbler)

rotate(time/10)
cube(5)
```

If a material that defines custom uniforms is used, but without them being specified in the program, then the values in the GLSL will default to 0.0 (unless the shader specifies defaults) and Improviz will complain in the logs but still keep running.

## Live-Coding Materials

Improviz has the ability to hot-reload materials, and so in theory it is possible to fully live-code both the geometries, as well as the materials themselves.
Currently the implementation will crash occasionally and so probably isn't yet ready for live use. The feature is definitely on the road-map though and will hopefully be stable in the not too distant future.

As a result, no documentation is provided at the moment, but the __materials/send.sh__ script shows how material files can be sent to improviz once it's running, so feel free to try it out and report back.
