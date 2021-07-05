# Improviz

[![Build Status](https://travis-ci.org/rumblesan/improviz.svg?branch=main)](https://travis-ci.org/rumblesan/improviz)
[![Windows build](https://ci.appveyor.com/api/projects/status/sv8a7mosacfsng2l?svg=true)](https://ci.appveyor.com/project/rumblesan/improviz)


## Description

Improviz is a live-coding environment built for creating visual performances of abstract shapes, blurred shades and broken GIFs.

It is built in Haskell and interacts directly with OpenGL. It's very much a work in progress but is definitely stable enough to use for performances.


## Documentation

The most up-to-date documentation can be found in [the docs folder](docs/index.md) which are also used to build the [Improviz site](https://improviz.rumblesan.com), but the README should give enough of an overview to get started.


## Community

If you find bugs, need help setting things up, are unsure where to begin, or just want to share what you've made, then there is an Improviz channel on the [*TOPLAP live coding* discord](https://discord.gg/mhjekDKXXg).


## Browser Version

There is now a browser based version of Improviz available at [https://improviz-web.rumblesan.com/](https://improviz-web.rumblesan.com/) which has much of the functionality of the native version.

The primary differences are that it currently lacks the ability to load custom textures, geometries and materials, though hopefully that will be changed in future.

More details can be found [in the docs](docs/web.md) whilst the code can be found in a [different repository](https://github.com/rumblesan/improviz-web).


## Installing

Pre-built binaries are available for OSX, Windows and Linux, and can be found on the [Releases Page](https://github.com/rumblesan/improviz/releases). Download one and then jump straight to the [relevant section in Running](#pre-built-binary).

These should **"just work"** but if there are any problems please raise an issue.


## Building

Improviz is built using [Stack](https://docs.haskellstack.org/en/stable/README/).

With that installed, it should just be a matter of cloning the repository, entering the directory and running

```bash
stack build
```

Depending on your platform it may be necessary to install some or all of the following packages.

* libgmp-dev
* libxrandr-dev
* libxi-dev
* libxinerama-dev
* libxcursor-dev
* freeglut3-dev
* zlib1g-dev

From there, you'll be able to run what was just built using

```bash
stack exec improviz
```

which will use the *improviz.yaml* file in the root of the repository.

For more information about building and installing Improviz, check the [development documentation](docs/development.md).

## Running

This differs slightly depending on whether you've built from source or downloaded a binary.

### Pre-built binary

After downloading the zip file
* Unzip it to somewhere on your machine
* Open a terminal and browse to the folder (double clicking on it doesn't work currently because the paths need fixing)
* Run the `improviz` or `improviz.exe` command (depending on your platform)
* If a white window appears in the top left of your screen then you're ready to go!
* If it doesn't then have a look at the errors that have appeared in the terminal


## Using

Once Improviz is running, you need to send code for it to run. The simplest way to do this to test is to use the (very basic) built in editor.

Open your browser and go to [http://localhost:3000/editor](http://localhost:3000/editor), then try entering the following. Indentation is a single tab.

```
background(255, 0, 0)

fill(0, 255, 255)
loop 10 times
	rotate(time, 3, time)
	cube(4)
```

There are better clients available for [Vim](https://github.com/rumblesan/improviz-vim) and [Atom](https://github.com/rumblesan/improviz-atom).
There is a very basic client available for [Emacs](https://github.com/rumblesan/improviz/tree/main/editor/emacs).


## Language

Please check the [language](docs/language.md) document for details of the language itself.


## Configuration

Please check the [configuration](docs/configuration.md) document for details on how to configure Improviz.


## hellocatfood GIFS

As part of the project, artist, algoraver, and all-round excellent human being [hellocatfood](https://hellocatfood.com) was commissioned to create a series of animated GIFs that can be used as textures with Improviz.

These excellent and unique images are bundled in the pre-built binary releases and can be found in the **hellocatfood/gifs** folder in this repo. The **hellocatfood/geometries** folder contains the *.obj* files used to make some of them which can also be loaded by Improviz. They're all licensed under the [Creative Commons Attribution 4.0 International licence](https://creativecommons.org/licenses/by/4.0/) so can be freely used and misused for performances, projects, and anything else you want.

By default they're not loaded by improviz when using the default configuration as they're quite large and slow startup a bit, but just uncomment the relevant lines in the **improviz.yaml** config file and you're ready to go.

## Contributors

Many thanks to the following people who have contributed to Improviz and it's development.

[hellocatfood](https://hellocatfood.com) for the excellent gifs and general feedback
[darch](http://www.darch.dk/) for some really helpful feedback, and teaching improviz workshops
[nihilazo](https://itwont.work/git/) for helping polish the vim plugin and give it some necessary updates


## Contact

Drop me an email at guy@rumblesan.com


## License

BSD License.
