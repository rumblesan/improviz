# Getting Started

This document is designed to be a basic intro to Improviz.

Ideally it should get you up and running and introduces to some of the basic concepts.


## Running

If you're reading this then you should have already downloaded and unzipped the release for your system.

* Open a terminal
* Browse to the unzipped folder
* Run the `improviz` or `improviz.exe` file depending on your platform

If you get a white screen appearing in the top-left then things are looking good.

If not, go have a read in the [Troubleshooting](#troubleshooting) section.


## Interacting

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


## Documentation

Currently the best docs are those that live on the [Improviz website](https://improviz.rumblesan.com).
