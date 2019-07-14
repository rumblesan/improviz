# Development

The simplest way to use Improviz is by getting one of the pre-built binaries. If you want to make deeper changes, or contribute to development, then it will be necessary to build the project locally yourself.

## Cloning

The [main repository](https://github.com/rumblesan/improviz) contains all the code and can be found on GitHub.

## Building

Improviz uses the [stack](https://www.haskellstack.org/) tool to build the project. Once you have that installed, it should be possible to just browse to the cloned repository directory, and run `stack build`

## Running

Improviz can be run directly from the project folder with stack.

```bash
stack exec improviz
```

This should present you with the white starting window. The **improviz.yaml** file in the root folder of the project will be loaded as a configuration file.

You can install the fully built binary onto your system so it's not necessary to run it from the project folder. `stack install` will build Improviz and install it to the stack _local-bin-path_ which by default on OSX is **~/.local/bin**. (I'm unsure what it is on Linux or Windows).

With the installed folder added to your path, you should just be able to run `improviz` from the command line.

## Testing

Improviz has a suite of tests that can be run using `stack test`
