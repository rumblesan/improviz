# Textures

Improviz supports bitmap textures on shapes.

```
texture(:another)
  box()
move()
fill(255, 0, 0)
  ball()
```

The program loads the textures at start-up from the *textures* directory by reading the *config.yaml* file. To add more textures, just drop the BMP files into the directory and then add the name to the config file.

Transparency is supported.

It may be better for the texture to have a sizing that's a power of 2. e.g. 256*256  pixels. It supports those that aren't though. Needs more investigation currently.
