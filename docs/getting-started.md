# Getting Started

This document is designed to be a basic introduction to Improviz.

Ideally it should get you up and running and familiar with some of the basic concepts.


## Running

### Pre-built Binary

The simplest way to start is to downloaded and un-archive the pre-built release for your system from the [GitHub Releases](https://github.com/rumblesan/improviz/releases) page.
Improviz needs to be run from the terminal because of the way it's built, but this is easy to do.

* Download the zip file from GitHub, and then make sure you unzip it somewhere.
* On OSX you'll want to open the *Terminal* app, whilst on Windows you want to open the Command Prompt.
* You need to browse to the unzipped folder in the terminal, which will require using the `cd` command. For example, if on OSX you downloaded and unzipped it to your *Downloads* folder, then use the command `cd /Users/<your user name>/Downloads/improviz-osx`.
* Run the `improviz` or `improviz.exe` file depending on your platform. On OSX or Linux you will probably need to specify it's a file in the folder you want to run, in which case use the command `./improviz` without any file extension. On windows you don't need to do this, but you do need the file extension, so run `improviz.exe`.

If you get a white screen appearing in the top-left then things are looking good.

Depending on the system you're running on, you may get a prompt asking `Do you want the application “improviz” to accept incoming network connections?` This is because Improviz runs an HTTP service that's used for communications between the program and the editor, so you need to allow this.

If you're on OSX, then you may also get a security warning that won't let you run Improviz. This isn't anything to worry about, it's just because I'm not willing to pay Apple to code sign the app. Open up *Security & Privacy* settings, go to the *General* tab, and you should have the option to let Improviz run. Allow this, and then try restarting it from the command line.

If you don't get a screen appearing, then please [raise an issue](https://github.com/rumblesan/improviz/issues) on the repository and include any error messages that may have been printed out in the console.

### Building from Source

If you'd prefer to build from source, then the [development](./development.md) documentation should cover all the details.

## Interacting

Once Improviz is running, you need to send code for it to run. The simplest way to do this to test is to use the (very basic) built in editor.

Open your browser and go to [http://localhost:3000/editor](http://localhost:3000/editor), then try entering the following. Indentation is done using a single tab.

```
background(255, 0, 0)

fill(0, 255, 255)
loop 10 times
	rotate(time, 3, time)
	cube(4)
```

You should see ten light-blue cubes rotating on a red background, with the black and yellow text in the top left.

## Editors

The built in editor is fine for quick tests, but it's much nicer to work with one that's a bit more friendly.

There are better clients available for [Vim](https://github.com/rumblesan/improviz-vim) and [Atom](https://github.com/rumblesan/improviz-atom).
There is a very basic client available for [Emacs](https://github.com/rumblesan/improviz/tree/main/editor/emacs).


## Documentation

If you downloaded the release archive, then you should find there's a *documentation* folder with a number of files in it. This should cover everything you need to get going.

You can also find the same documents available on the [Improviz website](https://improviz.rumblesan.com).
