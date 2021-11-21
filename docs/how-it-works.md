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
