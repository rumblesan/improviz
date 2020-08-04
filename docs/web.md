# Improviz Web

The browser based version of Improviz can be found at [https://improviz-web.rumblesan.com/](https://improviz-web.rumblesan.com/) and shares most of the functionality but with a few differences.

The code is available at [https://github.com/rumblesan/improviz-web](https://github.com/rumblesan/improviz-web).

## Missing Features

Currently there is no loading custom textures, geometries or materials. This will hopefully be added in the future, but for the time being there's only what's built in (and which probably needs to be expanded on).

There is also no way to send values to the program over OSC, though this too is on the roadmap.

## Language Differences

The only difference to the language is that loops must use the keyword `loop` before the looping expression.

```
loop 10 times
  rotate()
  cube()
```

This is is currently optional in the native version of Improviz but will likely become mandatory in the future.

## Editor Experience

The browser version has an integrated editor with built-in error notifications and code highlighting. It does support Vim bindings as well.

## Sharing

The sharing feature makes it possible to share Improviz programs (up to a size limit) by generating a URL from the program.
