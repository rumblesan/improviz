# Configuration

The configuration for Improviz is mainly set in the config file that it loads at startup.  By default, Improviz will attempt to load the *improviz.yaml* file in the same directory it's run from. This file can be changed by using the `-c <filepath>` command line option.
The default *improviz.yaml* file should have all the available settings visible (though fullscreen is commented out).

Some configuration can be set via flags given to Improviz when it is run from the command line.


## Screen size

The *screenwidth* and *screenheight* keys in the confifg file are used to specify the size of the screen, with the default values being 640px wide by 480px high.
This can be set via the command line using the `-w` and `-h` command line flags.

```bash
improviz -w 1024 -h 768
```

```yaml
screenwidth: 1024
screenheight: 768
```


## Full Screen

When making Improviz fullscreen, it's also necessary to pass in the number of the display it should be on.

```bash
improviz -f 0
```

0 is the primary display. 1 would be the next attached display to the machine.

This can also be set by the *fullscreen* key in the confifg file.

```yaml
fullscreen: 0
```


## Font File

The font used by default is arial but an alternative can be used by specifying a path to the .ttf file.

```yaml
font:
  filepath: /opt/fonts/ComicSans.ttf
```

The colour of the font and the background can also be set. This is done by giving a list of values corresponding to red, green, blue and alpha. These values should be between 0 and 255.
*Note* a value of 0 for alpha will be fully transparent.

```yaml
font:
  foregroundColour: [255, 0, 255, 255]
  backgroundColour: [0, 0, 0, 0]
```


## Assets Directory

Improviz has a very basic built in editor that can be used by browsing to [http://localhost:3000/editor](http://localhost:3000/editor) once it's running. The asset directory is currently just a path to the folder containing the HTML used for serving this editor.

```yaml
assetsDirectory: "./assets"
```


## Texture Directories

Improviz can be given a list of directories to read texture files from. The details of how this works can be found in [textures.md](textures.md)

```yaml
textureDirectories:
 - /opt/improviz/textures
 - textures
 - ./downloads/textures
```


## Geometry Directories

Improviz can be given a list of directories to read geometry files from. The details of how this works can be found in [geometries.md](geometries.md)

```yaml
geometryDirectories:
 - ./geometries
```


## Code Files

Most of the generally used functions Improviz provides are written on top of the slightly lower level functions in the language. So code for `rotate`, `move`, `cube` and `fill` for example is actually all in files contained in the **stdlib** folder which is loaded when Improviz starts up. This is specified with the *codeFiles* setting in the config file.

There is also the **usercode** directory which contains a few other useful functions, and is meant to show how you can also create your own functions that further expand on what you can do with Improviz. This makes it possible to have specific functions for performances available without having to modify the source or copying it into the editor window each time.

All these files can be loaded at startup just by specifying the list of *codeFiles*.

```yaml
codeFiles:
  - "./stdlib/variables.pz"
  - "./stdlib/transformations.pz"
  - "./stdlib/shapes.pz"
  - "./stdlib/style.pz"
  - "./stdlib/textures.pz"
  - "./usercode/grid.pz"
  - "./usercode/seq.pz"
```


## Debug

The debug setting can be turned on via the cli or the config file. This is mainly meant to be used when developing features on Improviz, and it's unlikely to be useful for performing.

```bash
improviz -d
```

```yaml
debug: true
```


## Window Title Bar

By default the Improviz window hides the title bar but this can be enabled using the *decorated* setting in the config file. It's also possible to change the title displayed there with the *apptitle* setting.

```yaml
decorated: true
apptitle: "My Performance"
```


## Server Ports

Improviz runs two servers, one for HTTP connections and one for OSC messages. By default the OSC server is disabled unless enabled in the config file. The HTTP server is always enabled because this is how editors communicate with the process. The ports that these run on can also be configued, with the HTTP server running on port 3000 and the OSC server running on port 5510 by default.

```yaml
serverPort: 3000
osc:
  enabled: true
  port: 5510
```


# Screen Settings

The screen settings allow changes to be made to the OpenGL clipping planes. It's unlikely that these will need to be changed, but the option is there to do so. This document on [Depth Buffer Precision](https://www.khronos.org/opengl/wiki/Depth_Buffer_Precision) may be useful for more information on why you might want to do this. The default values are 0.1 for zNear and 100 for zFar.

```yaml
screen:
  front: 0.1
  back: 100
```
