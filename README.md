# Sigma16 source directory

Sigma16 is a computer architecture designed for research and teaching
in computer systems.  This application provides a complete environment
for experimenting with the architecture.

*Note (autumn 2019): The architecture, software, and documentation are
currently being revised; this is a development version and it is not
ready for general use.  There are several earlier releases, as well as
extensive documentation, but those are available only on a protected
Moodle server.  The new version is expected to be available on github
around the beginning of February 2020.  The notes below, as well as
the user guide, are incomplete and refer to previous versions, so the
current documentation is inconsistent with the current software.*

* [Launch Sigma16 to run in your
  browser](https://jtod.github.io/S16/dev/app/Sigma16.html).  This
  runs the latest release, and requires Internet access.
* [The Sigma16 Home Page](https://jtod.github.io/S16/) contains
  further information, documentation, and executable versions.  The
  URL is https://jtod.github.io/S16/
* [The User
  Guide](https://jtod.github.io/S16/dev/app/doc/html/userguide.html)
  contains documentation of the architecture, software tools,
  programming, and installation.
* [The source repository](https://github.com/jtod/Sigma16).  allows
  you to view the source and download it.  This the README page for
  the source repository.  The URL is https://github.com/jtod/Sigma16
* [Launch from files in this directory](./app/Sigma16.html) If you
  have copied this directory onto a local machine, this link will run
  it, with some limitations on file access (the User Guide won't be
  visible, and you have to copy examples to the editor manually).  See
  the Installation section in the User Guide for further ways to run
  the software.
* [Up to the top directory](./) Show the listing of files in the
  directory containing this version.

## Download

It isn't necessary to download, compile, and install the software: you
can just run it in a browser by clicking the Quick Start link above.

If you want to run the app when Internet is inaccessible, you can
download the software and run it locally on your machine.  You don't
need any special installation; simply store the Sigma16 folder
anywhere on your computer.  Then there are several options:

### Run in a browser

Open the file *index.html* in a browser; this contains a link to
launch the app, and there may be contain release notes.  Note that a
few features won't work, because browsers restrict some operations
when a web page executes from a local file (as opposed to an https
server).  In particular, when you open an example program (in the
Examples tab), the button "Copy example to editor" won't work.  But
you can select the text of the example with your mouse, right-click
and Copy, then Paste it into the editor.

### Run with npm

See app/makefile for notes on how to run the software as a standalone
program, without using a browser.


The following software needs to be installed in order to build the
executable using electron

    node.js
    npm

### Compile using npm

    Clone this repository
    git clone https://github.com/electron/electron-quick-start
    cd electron-quick-start        # Go into the repository
    npm install     # Install dependencies
    npm start         # Run the app

The following files are required for compilation with npm, but they
are not needed just to run the source program in a browser.

    src/main.js
    src/package.json
    src/package-lock.json
    src/preload.js
    src/renderer.hs
    src/node_modules/

## About the software

This is free software; see the LICENSE file.

The author is John O'Donnell, john.t.odonnell9@gmail.com
