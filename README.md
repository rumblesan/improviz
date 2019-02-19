# Improviz

[![Build Status](https://travis-ci.org/rumblesan/improviz.svg?branch=master)](https://travis-ci.org/rumblesan/improviz)
[![Windows build](https://ci.appveyor.com/api/projects/status/sv8a7mosacfsng2l?svg=true)](https://ci.appveyor.com/project/rumblesan/improviz)

An offshoot of LiveCodeLab


## Description

Improviz is built in Haskell and interacts directly with OpenGL. It's very much a work in progress but it should be about usable.


## Installing

Pre-buily binaries are available for OSX, Windows and Linux, and can be found on the [Releases Page](https://github.com/rumblesan/improviz/releases). Download one and then jump straight to the [relevant section in Running](#pre-built-binary).

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


## Running

This differs slightyly depending on wether you've built from source or downloaded a binary.

### Pre-built binary

After downloading the zip file
* Unzip it to somewhere on your machine
* Open a terminal and browse to the folder (double clicking on it doesn't work currently because the paths need fixing)
* Run the `improviz` or `improviz.exe` command (depending on your platform)
* If a white window appears in the top left of your screen then you're ready to go!
* If it doesn't then have a look at the errors that have appeared in the terminal

### Run from source

Improviz can be run directly from the project folder by using stack.

```bash
stack exec improviz
```

This will present you with a blank and quite uninteresting window. You may get a pop-up window asking if you want to accept incoming network connections. This is because Improviz runs a webserver to allow for communication between the server and an editor. The **improviz.yaml** file in the root folder of the project will be loaded as a config file.

You can install the fully built binary onto your system so it's not necessary to run it from the project folder. `stack install` will build Improviz and install it to the stack _local-bin-path_ which by default on OSX is **~/.local/bin**. (Unsure what it is on Linux or Windows).

With the installed folder added to your path you can just run `improviz`.


## Clients

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
There is a very basic client avaialable for [Emacs](https://github.com/rumblesan/improviz/tree/master/editor/emacs).


## Language

Please check the [language](docs/language.md) document for details of the language itself.


## Configuration

Please check the [configuration](docs/configuration.md) document for details on how to configure Improviz.


## Interacting

Improviz runs a web server on *localhost:3000* which currently exposes four endpoints.

`/read`
POST request
Any text sent to this will be compiled and interpreted by Improviz. If there is a failure then it will continue running the last working program.

`/toggle/text`
POST request
Turn the text display on and off

`/editor`
Browse to this endpoint in a browser to get a very basic form to submit programs to Improviz. Mainly just useful for testing.

`/vars/edit/:name`
POST request
Can be used to set the value of variables within Improviz.

In the *examples* folder there is a bash script called **send.sh** which can be used to run any of the example programs.

`./examples/send.sh ./examples/simple1.pz`


### OSC

Improviz can accept OSC messages to set and change the value of variables within the system. The format for this is very simple.

`"/vars/<varname>" <int|float>`

The OSC server must be enabled by setting the flag in the configuration. The default port is *5510* but can also be changed in the config.

Multiple variables can be set by sending message bundles.


## Contact

Drop me an email at guy@rumblesan.com


## License

BSD License.

