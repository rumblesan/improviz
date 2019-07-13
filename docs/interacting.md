# Interacting

Improviz does not have a user interface, but instead runs a server you can use to interact with it. This runs as a basic web server on *localhost:3000* which currently exposes four endpoints.

These are really designed to be used via a plugin in a suitable text editor. Check the editors section in the [Getting Started](./getting-started.md#editors) document for examples.

### read

`/read`
POST request
Any text sent to this will be compiled and interpreted by Improviz. If there is a failure then it will continue running the last working program.
In the *examples* folder there is a bash script called **send.sh** which can be used to run any of the example programs.

`./examples/send.sh ./examples/simple1.pz`


### toggle text

`/toggle/text`
POST request
Turn the text display on and off

### editor

`/editor`
Browse to this endpoint in a browser to get a very basic form to submit programs to Improviz. Mainly just useful for testing.

### vars edit

`/vars/edit/:name`
POST request
Can be used to set the value of external variables within Improviz which can then be accessed via the [ext](./reference.md#ext) function.


### OSC

Improviz can accept OSC messages to set and change the value of external variables within the system which can then be accessed via the [ext](./reference.md#ext) function. The format for this is very simple.

`"/vars/<varname>" <int|float>`

The OSC server must be enabled by setting the flag in the configuration. The default port is *5510* but can also be changed in the config.

Multiple variables can be set by sending message bundles.

