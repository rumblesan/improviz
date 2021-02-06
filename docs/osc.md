# OSC

Improviz can accept OSC messages to set and change the value of external variables within the system which can then be accessed via the [ext](./reference.md#ext) function. The message format for this is very simple.

`/vars/<varname> <int|float|string>`

The OSC server must be enabled by setting the flag in the configuration. The default port is *5510* but can also be changed in the config.

Multiple variables can be set by sending message bundles.

## Example

Lets say we want to control the red value in the colour of a shape in Improviz, and we're going to send a single number to do this.

We have another program (PureData or SuperCollider for example) send the number 100 to the OSC path `/vars/red`.

What Improviz does is keep a dictionary of names to values inside it, that gets updated when the messages are received. So when it gets this message it will store the number 100 against the symbol `red`.
The `ext` function will then get the current value stored for it to use in the program.

```
r = ext(:red, 77)
fill(r, 0, 0)
cube(2)
```

In this case, if the `:red` variable hasn't had a value set yet, then the default 77 value will be used.

The OSC messages that are being sent to Improviz are actually independent of the program being run. It will keep track of what it's been sent, but it's up to the program to go and get those values.

This also means that if we try to send OSC messages quicker than Improviz is running the program, then it won't necessarily get all the values.

## Toggling Text

Displaying the code overlay can also be toggled with OSC, and just requires sending an empty message with the path

`/toggle/text`
