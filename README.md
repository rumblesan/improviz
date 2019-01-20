# Improviz [![Build Status](https://travis-ci.org/rumblesan/improviz.svg?branch=master)](https://travis-ci.org/rumblesan/improviz)

An offshoot of LiveCodeLab


## Description

Improviz is built in Haskell and interacts directly with OpenGL. It's very much a work in progress but it should be about usable.


## Building

[Stack](https://docs.haskellstack.org/en/stable/README/) is the build tool for the project.

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

More investigation needed on cross platform building currently.


## Running

Improviz can be run directly from the project folder by using stack.

```bash
stack exec improviz
```

This will present you with a blank and quite uninteresting window. You may get a pop-up window asking if you want to accept incoming network connections. This is because Improviz runs a webserver to allow for communication between the server and an editor. The **improviz.yaml** file in the root folder of the project will be loaded as a config file.

It may be preferable to install the fully built binary onto your system so it's not necessary to run it from the project folder. `stack install` will build Improviz and install it to the stack _local-bin-path_ which by default is **~/.local/bin**.

With the installed folder added to your path you can just run `improviz`.


## Configuration

Please check the [configuration](docs/configuration.md) document for details.


## Interacting

Improviz runs a web server on *localhost:3000* which currently exposes three endpoints.

`/read`
POST request
Any text sent to this will be compiled and interpreted by Improviz. If there is a failure then it will continue running the last working program.

`/toggle/text`
POST request
Turn the text display on and off

`/editor`
Browse to this endpoint in a browser to get a very basic form to submit programs to Improviz. Mainly just useful for testing.

In the *examples* folder there is a bash script called **send.sh** which can be used to run any of the example programs.

`./examples/send.sh ./examples/simple1.pz`

See the *editor* folder for more information.

### OSC

Improviz can accept OSC messages to set and change the value of variables within the system. The format for this is very simple.

`"/vars" <string> <int|float>`

The OSC server must be enabled by setting the flag in the configuration. The default port is *5510* but can also be changed in the config.

It is possible to set multiple variables within a single message, just add more name/value pairs.

`"/vars" foo 0 bar 255 baz 0.8`

## Contact

Drop me an email at guy@rumblesan.com


## License

BSD License.

