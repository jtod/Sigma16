# Sigma16 source directory

This is the README file for the Sigma16 source directory.

Sigma16 is a computer architecture designed for research and teaching
in computer systems.  This application provides a complete environment
for experimenting with the architecture.

## Internet links

* For up to date information, and to run the software, visit the
  [Sigma16 Home Page](https://jtod.github.io/home/Sigma16/).  That
  page also contains links to documentation, past releases, and the
  source repository.  It's best to bookmark the home page, which
  always points to the latest version.  Don't bookmark a specific
  release.

* The URL for the [source
  repository](https://github.com/jtod/Sigma16/) is
  https://github.com/jtod/Sigma16/. 

## Links to local files

If you copy the Sigma16 source directory onto your computer, the
following links will run it locally without requiring further access
to the Internet.  However, these links will not work if you're reading
this page on the source repository on GitHub via the Internet.

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
can just run it in a browser by clicking the Quick Start link in the
Sigma16 Home Page.

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
program, without using a browser.  The following software needs to be
installed in order to build the executable using electron

    node.js
    npm

### Compile using npm

    Clone this repository
    git clone https://github.com/electron/electron-quick-start
    cd electron-quick-start  # Go into the repository
    npm install   # Install dependencies
    npm start     # Run the app

The following files are required for compilation with npm, but they
are not needed just to run the source program in a browser.

    src/main.js
    src/package.json
    src/package-lock.json
    src/preload.js
    src/renderer.hs
    src/node_modules/

## About the software

The Sigma16 app (the integrated development environment -- i.e. the
GUI) is implemented in JavaScript, html 5, and css.  The digital
circuit is implemented in Hydra, which requires Haskell.  Additional
software tools, including a high speed emulator, are in progress and
expected to be available in late 2020.

### Author

The architecture is designed by, and the software tools are written
by, [John O'Donnell](https://jtod.github.io/index.html).

~~~~
Email: john.t.odonnell9@gmail.com
Web: https://jtod.github.io/index.html
~~~~

### Copyright and license

~~~~
Copyright (C) 2019, 2020 John T. O'Donnell
License: GNU GPL Version 3 or later
See Sigma16/LICENSE.txt
~~~~

Sigma16 is free software: you can redistribute it and/or modify it
under the terms of the GNU General Public License as published by the
Free Software Foundation, either version 3 of the License, or (at your
option) any later version.

Sigma16 is distributed in the hope that it will be useful, but WITHOUT
ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
FITNESS FOR A PARTICULAR PURPOSE.  See the GNU General Public License
for more details.

You should have received a copy of the GNU General Public License
along with Sigma16.  If not, see <https://www.gnu.org/licenses/>.
