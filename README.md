# Improviz

[![Build Status](https://travis-ci.org/rumblesan/improviz.svg?branch=master)](https://travis-ci.org/rumblesan/improviz)
[![Windows build](https://ci.appveyor.com/api/projects/status/sv8a7mosacfsng2l?svg=true)](https://ci.appveyor.com/project/rumblesan/improviz)

An offshoot of [LiveCodeLab](https://livecodelab.net)


## Description

Improviz is built in Haskell and interacts directly with OpenGL. It's very much a work in progress but it should be about usable.


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
10 times
	rotate(time, 3, time)
	cube(4)
```

There are better clients available for [Vim](https://github.com/rumblesan/improviz-vim) and [Atom](https://github.com/rumblesan/improviz-atom).
There is a very basic client available for [Emacs](https://github.com/rumblesan/improviz/tree/master/editor/emacs).


## Language

Please check the [language](docs/language.md) document for details of the language itself.


## Configuration

Please check the [configuration](docs/configuration.md) document for details on how to configure Improviz.


## Hellocatfood GIFS

As part of the project, artist, algoraver, and all-round excellent human being [hellocatfood](https://twitter.com/hellocatfood) was commissioned to create a series of animated GIFs that can be used as textures with Improviz.

These excellent and unique images are bundled in the pre-built binary releases and can also be found in the [Improviz Performance](https://github.com/rumblesan/improviz-performance/) repository. They're licensed under the [Creative Commons Attribution 4.0 International licence](https://creativecommons.org/licenses/by/4.0/) so can be freely used and misused for performances, projects, and anything else you want.

## Contact

Drop me an email at guy@rumblesan.com


## License

BSD License.

