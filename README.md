# Sigma16

Sigma16 is a computer architecture designed for research and teaching
in computer systems.  This application provides a complete environment
for experimenting with the architecture.

*Note: The software is currently being revised; this is a development
version.  There are several earlier releases but those are available
only on a protected Moodle server.  The new version is expected to be
available on github around February 2020.*

## Quick start

You can run the software from the Internet without downloading or
installing anything.

  * [Launch the app](./app/Sigma16.html)  

  * [Run any of the past releases](https://jtod.github.io/S16) This
    web page contains links to the latest version, as well as past
    releases: https://jtod.github.io/S16

## About the software

The software includes an editor, assembler, linker, emulator, and an
integrated development environment.  There is a digital circuit that
implements the architecture, which is specified using the Hydra
functional hardware description language.  Hydra can simulate the
circuit, and machine language programs can run on both the emulator
and the circuit.

## Documentation

The User Guide is displayed in the running application, and you can
also read it here without launching the program:

  * [User Guide (html)](app/doc/html/userguide-index.html) The user
    manual is available as html in datafiles/doc/html/index.html.

  * [User Guide (plain text)](app/doc/src/userguide-index.md) The
    markdown source for the user guide is in
    app/doc/src/userguide-index.md.

## Download

If you want to run the app when Internet is inaccessible, you can
download the software and run it locally on your machine.

  * [Download to run in browser without Internet
    access](https://github.com/jtod/Sigma16) The project page contains the
    source code which you can download and run on your local machine.
    You don't need any special installation; simply store the Sigma16
    folder anywhere on your computer.  This page is actually the
    index.html file in the Sigma16 folder.

  * Download executable to run on local machine.  There is a
    standalone executable program that doesn't run in a browser, and
    doesn't require internet access (except to download the software
    one time).  If the download page contains an executable for your
    platform, you can download this and run it.  Alternatively
	
  * Download the source code and compile an executable for your
    platform.  See app/makefile

### Compiling the software

See the section Launching Sigma16.

* Running as a standalone app on your computer

    npm install
    npm start

* Launch the executable file Sigma16.exe.

If you don't have the executable file, you can build the software.
You need only download and install Haskell Tool Stack; this will
automatically download and install the rest of the software you need.
The installation will require several GB of secondary storage.

* Visit https://docs.haskellstack.org/en/stable/install_and_upgrade/

* This page gives instructions on how to install Stack on Linux,
  Macintosh, and Windows, along with download links.
  
* Open a shell, change to the Sigma directory, and enter this command:

```
stack build Sigma16
```

The following tools are required:

    git
    javascript
    html5
    css

### Standalone executable using electron

The following software needs to be installed in order to build the
executable using electron

    node.js
    npm

The following files are required

    src/main.js
    src/package.json
    src/package-lock.json
    src/preload.js
    src/renderer.hs
    src/node_modules/

### Compiling a standalone executable;  requires Node.js and npm

    cd src
	npm install
	npm start

    # Clone this repository
    git clone https://github.com/electron/electron-quick-start
    # Go into the repository
    cd electron-quick-start
    # Install dependencies
    npm install
    # Run the app
    npm start

## About the software

The author is John O'Donnell, john.t.odonnell9@gmail.com

This is free software; see the LICENSE file
