# OSC Examples

These example patches aim to show how to get other languages and environments sending OSC messages to Improviz. They're very basic but ideally make everything clear.

## Interacting

The [Interacting](../../docs/interacting.md) documentation is where the explanation of how everything should work lives. This doc is just going to be any information about getting these examples running.

## Initial Setup

Make sure OSC is enabled in the *improviz.yaml* configuration, then start Improviz and get the *simple-green.pz* patch from this folder running on it. You can use the basic [built-in editor](https://improviz.rumblesan.com/getting-started.html#interacting) or an external editor, it shouldn't matter how you run it.

You should see just a black, rotating cube.

## SuperCollider

The SuperCollider example is a really basic patch that plays sine waves at random frequencies, and sends that frequency value to Improviz as a variable called *freq*.

The *run-supercollider-interaction-osx.sh* file is just a helper script that will run SuperCollider from the command line on OSX. All it's doing is running the *sclang* executable and passing it the path to the code file.

`/path/to/SuperCollider/sclang ./supercollider-osc.scd`

The example should still work fine if loading it through the SC editor, but please refer to the SuperCollider docs for more information on that.

When you run the example, you should see that when the frequency of the tone changes, the cube moves between various shades of green.

## Pure Data

The Pure Data example is simpler and a bit more reactive. All the patch has is a slider that outputs a value between 0 and 1000 which gets sent to the same *freq* variable in Improviz.
