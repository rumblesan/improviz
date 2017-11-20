# Improviz [![Build Status](https://travis-ci.org/rumblesan/improviz.svg?branch=master)](https://travis-ci.org/rumblesan/improviz)

An offshoot of LiveCodeLab


## Description

Improviz is built in Haskell and interacts directly with OpenGL. It's very much a work in progress but it should be about usable.

## Building

[Stack](https://docs.haskellstack.org/en/stable/README/) is the build tool for the project.

With that installed, it should just be a matter of cloning the repository, entering the directory and running

```bash
stack build
stack exec improviz
```

which will present you with a blank and quite uninteresting window. You may get a pop-up window asking if you want to accept incoming network connections. This is because Improviz runs a webserver to allow for communication between the server and an editor.

Depending on your platform it may be necessary to install some or all of the following packages.

* libgmp-dev
* libxrandr-dev
* libxi-dev
* libxinerama-dev
* libxcursor-dev

More investigation needed on cross platform building currently.

## Interacting

Improviz runs a web server on *localhost:3000* which currently exposes two endpoints.

`/read`
POST request
Any text sent to this will be compiled and interpreted by Improviz. If there is a failure then it will continue running the last working program.

`/toggle/text`
POST request
Turn the text display on and off

In the *examples* folder there is a bash script called **send.sh** which can be used to run any of the example programs.

`./examples/send.sh ./examples/simple1.pz`

There is also a very basic [vim plugin](https://github.com/rumblesan/improviz-client.vim).

## Contact

Drop me an email at guy@rumblesan.com


## License

BSD License.

