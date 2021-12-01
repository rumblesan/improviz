# Post Processing Filters

The two long-standing, built-in post processing filters in Improviz have been the *motionBlur* and the *paintOver*. These have been hard-coded into the program with no customisation possible.

The entire post processing chain has been overhauled to make it possible to not only create custom post processing shaders, but also to allow setting variables within them from the Improviz program, in the same way it can be done with the material shaders.

The post processing filters are very similar to the material shaders, being YAML files with a *vertexShader* and *fragmentShader* section. By default they can be found in the __filters__ folder but because everything is defined in the main __config.yaml__ file this can be changed, or more folders added if necessary.

## Usage

Using a filter is done with the `animationStyle` function, and then giving it a symbol that corresponds to the name of the filter defined in the config.

```
animationStyle(:paintOver)
```

## Creating Custom Filters

There are two sets of attribute data and three uniforms that Improviz makes available to the filter shaders by default. It is also possible to pass custom uniform values to shaders for further control.

### Atributes

The attributes are all vertex arrays that define the flat rectangle the scene is rendered onto, and are fixed for all the post processing filters. They all have to use specific layout locations when using them in a shader.

* position
    The vertex position coordinates. Uses layout location 0 by default.
* texcoord
    The vertex texture coordinates. Uses layout location 1 by default.

### Uniforms

There are three uniforms that Improviz can pass to the filter shaders.

* texFramebuffer
    The texture that the current frame is rendered to before any post processing.
* lastFrame
    The texture that the last frame was rendered to without any post processing.
* depth
    The depth texture for the current frame

### Custom Uniforms

A post processing filter can define other uniforms that can be set from within the Improviz code, with the value being changeable using the `postProcessing` function. Improviz will automatically detect what uniforms have been defined in the GLSL code and will expect the same names to be used when passing values in.

For example, the __motionBlur__ material has a uniform for `BlurRatio`, setting what the level of the previous frame that's blended into the next is. To change this you would do something like the following.

```
animationStyle(:motionBlur)
postProcess(:variable, :BlurRatio, sin(time/5) * 0.93)

rotate(time/10)
cube(5)
```

If a material that defines custom uniforms is used, but without them being specified in the program, then the values in the GLSL will default to 0.0 (unless the config specifies defaults) and Improviz will complain in the logs but still keep running.
