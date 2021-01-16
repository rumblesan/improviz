# Configuration

The configuration for Improviz is mainly set in the config file that it loads at startup.  By default, Improviz will attempt to load the *improviz.yaml* file in the same directory it's run from. This file can be changed by using the `-c <filepath>` command line option.
The default *improviz.yaml* file should have all the available settings visible (though fullscreen is commented out).

Some configuration options can also be set via flags given to Improviz when it is run from the command line.

## UI

The following settings are all related to the Improviz UI.

### Screen size

The *screenwidth* and *screenheight* keys in the confifg file are used to specify the size of the screen, with the default values being 640px wide by 480px high.
This can be set via the command line using the `-w` and `-h` command line flags.

```bash
improviz -w 1024 -h 768
```

```yaml
screenwidth: 1024
screenheight: 768
```


### Full Screen

When making Improviz fullscreen, it's also necessary to pass in the number of the display it should be on.

```bash
improviz -f 0
```

0 is the primary display. 1 would be the next attached display to the machine.

This can also be set by the *fullscreen* key in the confifg file.

```yaml
fullscreen: 0
```


### Decorated

The decorated option allows enabling or disabling the bezel around the screen. This is primarily useful when making it full screen so that it can occupy all the space available. The downside is that it's not possible to move the position of the screen when this is disabled.

```yaml
decorated: true
```


### App Title

If the screen bezel is shown by enabling the decorated option, then the apptitle setting can be used to change the text shown in the title bar of the bezel. By default this is just "Improviz" but you're free to change it.

```yaml
apptitle: "Improviz"
```

### Show Text

The code of the program you're running can be shown as an overlay, which can be toggled on and off from the editor. By default it will start shown, but if you want Improviz to start with the code not being shown (to save you toggling it off at the start) then you can use the showText setting. Setting this to false won't stop you being able to show the text, and it can still be toggled from the editor.

```yaml
showText: true
```


## Directory Paths

Improviz can load gifs, textures, geometries, and material shaders from other folders to use in the programs you write. If you want to load these from new folders, or stop loading the default assets, then these are the relevant settings.

### Assets Directory

Improviz has a very basic built in editor that can be used by browsing to [http://localhost:3000/editor](http://localhost:3000/editor) once it's running. The asset directory is currently just a path to the folder containing the HTML used for serving this editor.

```yaml
assetsDirectory: "./assets"
```


### Texture Directories

Improviz can be given a list of directories to read texture files from. The details of how this works can be found in [textures.md](textures.md)

```yaml
textureDirectories:
 - /opt/improviz/textures
 - textures
 - ./downloads/textures
```


### Geometry Directories

Improviz can be given a list of directories to read geometry files from. The details of how this works can be found in [geometries.md](geometries.md)

```yaml
geometryDirectories:
 - ./geometries
```


### Material Directories

Improviz can be given a list of directories to read material files from. The details of how this works can be found in [materials.md](materials.md)

```yaml
materialDirectories:
 - ./materials
```


### Code Files

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

## Font Settings

The font settings allow changing not just the font used, but also the colours and size of the displayed text. All these settings live in a sub section of the main config under the `font` key.

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

The font size can be changed just by setting the size value.

```yaml
font:
  size: 14
```

## Misc Settings

These are all settings that you probably don't need to change, but are documented here just in case.

### Debug

The debug setting can be turned on via the cli or the config file. This is mainly meant to be used when developing features on Improviz, and it's unlikely to be useful for performing.

```bash
improviz -d
```

```yaml
debug: true
```


### Server Ports

Improviz runs two servers, one for HTTP connections and one for OSC messages. By default the OSC server is disabled unless enabled in the config file. The HTTP server is always enabled because this is how editors communicate with the process. The ports that these run on can also be configued, with the HTTP server running on port 3000 and the OSC server running on port 5510 by default.

```yaml
serverPort: 3000
osc:
  enabled: true
  port: 5510
```


### Screen Settings

The screen settings allow changes to be made to the OpenGL clipping planes. It's unlikely that these will need to be changed, but the option is there to do so. This document on [Depth Buffer Precision](https://www.khronos.org/opengl/wiki/Depth_Buffer_Precision) may be useful for more information on why you might want to do this. The default values are 0.1 for zNear and 100 for zFar.

```yaml
screen:
  front: 0.1
  back: 100
```
