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

* The URL for the source code repository is
  [https://github.com/jtod/Sigma16/](https://github.com/jtod/Sigma16/).

## Download and links to local files

To run the IDE (the graphical user interface) there is no need to
download or install the software: you can just run it in a browser by
clicking the *Launch Sigma16* link in the [Sigma16 Home
Page](https://jtod.github.io/home/Sigma16/).

The command line tools do require downloading and installing the
software.  These include a standalone version of the IDE, circuit
simulator, and more.  See the Installation section of the User Guide.

If you copy the Sigma16 source directory onto your computer, the
following links will run it locally without requiring further access
to the Internet.  However, these links will not work if you're reading
this page on the source repository on GitHub via the Internet.

* [Launch from files in this directory](./app/Sigma16.html) If you
  have copied this directory onto a local machine, this link will run
  it.  The advantage is that you won't need access to the Internet.
  However, browsers have restricted access to your file system, to
  ensure security.  One consequence of this is that the *Select
  example* button in the *Examples* page won't do anything.  But you
  can select the text of the example with your mouse, right-click and
  Copy, then Paste it into the Editor page.  It's also possible to
  install Sigma16 as an app on your computer; that gets rid of the
  restrictions on file access.  See the Installation section in the
  User Guide.

* [Up to the top directory](./) Show the listing of files in the
  directory containing this version.

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
