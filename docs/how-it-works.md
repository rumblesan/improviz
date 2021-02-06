# How It Works

This document is going to try and outline what Improviz is actually doing when it's running your code. It's not necessarily important to understand this, but if you find yourself diving deeper into how things work then it's useful to have some understanding for what goes on.

## Basics

Say we have a really simple program, that just has a single cube :-

```
cube(2)
```

When you send this program to Improviz, it will take the code, parse it to make sure it's all valid, and turn it into it's internal representation of the program.
It will then run this program every time it needs to render a single frame, which by default it tries to do 60 times a second.

The cube isn't going to be moving, because we've not told Improviz to animate it in any way, but it's still being redrawn each frame.

## Time

By default, the only thing that actually changes in Improviz is the `time` variable, which tracks the number of seconds that have passed since the program was started. This is a floating point number, so after ten and a half seconds it would have a value of 10.5.

Let's change our program so we use `time`.

```
cube(time % 2)
```

When we send this program to Improviz, it will parse the new code to make sure it's all ok again, turn it into it's internal representation, and then it will start running this new program every frame.

You should now see a cube that starts from nothing, increases in size, then suddenly starts back from nothing again. This is because each time the program is run, the `time` variable has a new, increasing value, and the `%` modulo operator keeps it between 0 and 2.

### Independent Frame rate and Time

One thing that's important to note here is that the increase `time` is not linked to the frame rate. So if we slowed the frame rate down, then the cube would still take two seconds to reach it's maximum size.

## Default Values

If we start using the `rotate` function, then we don't actually specify the `time` variable any more.

```
rotate()
cube(2)
```

We still see the cube moving though, because if we don't give the `rotate` function any arguments, then it will use default values that use the time variable. You can see these default values in the standard library functions in the stdlib folder of Improviz, or look at them [on the GitHub repo](https://github.com/rumblesan/improviz/blob/main/stdlib/transformations.pz#L2)

The important thing to remember here, is that this scene is still being redrawn every frame, and the rotation is happening because the `time` variable value is just a little bigger than it was when the last frame was drawn.

## OSC Variables

The place that this understanding is probably most important to have is around using OSC messages with Improviz. The [interacting](../interacting.md#osc) document explains how you can send OSC messages to Improviz and then use the [ext](./reference.md#ext) function to use them but doesn't really explain what's going on very well.

Lets say we want to control the red value in the colour of a shape in Improviz, and we're going to send a single number to do this.

We have another program send the number 100 to the OSC path `/vars/red`.

Improviz essentially keeps a dictionary of names to values inside it, that gets updated when the messages are received. So when it gets this message it will store the number 100 against the symbol `red`.
If we modify our program like so :-

```
red = ext(:red, 77)
fill(red, 0, 0)
cube(2)
```

Then each frame Improviz will run the program, each time it runs the `ext` function, and each time that function runs it goes to the dictionary, sees if there is an entry for `:red` and if there is then it gets that value. If there isn't an entry (because we've not yet sent a message for example) then it will use the 77 value it was given.

In this way, the OSC messages that are being sent to Improviz, are actually independent of the program being run. It will keep track of what it's been sent, but it's up to the program to go and get those values.

This also means that if we try to send OSC messages quicker than Improviz is running the program, then it won't necessarily get all the values.
