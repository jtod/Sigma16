% Sigma16 home page

Sigma16 is a computer architecture designed for research and teaching
in computer systems.  This application provides a complete environment
for experimenting with the architecture.

This is the Sigma16 home page
([https://jtod.github.io/S16/](https://jtod.github.io/S16/)).  It
contains general information, pointers to various resources, and an
executable version.  To view and download the source code, see the
Sigma16 source repository
([https://github.com/jtod/Sigma16](https://github.com/jtod/Sigma16)).

Quick start:

* [**Launch Sigma16 to run in your
  browser**](https://jtod.github.io/S16/dev/app/Sigma16.html)
* [Read the User
  Guide](https://jtod.github.io/S16/dev/app/doc/html/userguide.html)
  This includes documentation of the architecture, programming, and
  installation.

This architecture is simpler than most commercial products making it a
good vehicle for teaching and experimentation.

* A 16-bit architecture with a simple yet powerful instruction set.

* A small subset of the architecture is suitable for introductory
  teaching, but more advanced features support systems programming.

* Software tools include an assembler, linker, emulator, and IDE.

* Examples are provided that illustrate a variety of programming
  techniques, data structures, compilation patterns, concurrency and
  mutual exclusion, and more.

* There is a digital circuit that implements Sigma16, which includes
  several microarchitecture implementations as well as logic designs
  specified at the level of logic gates and flip flops.  These
  circuits are specified using Hydra, a functional computer hardware
  description language.  Sigma16 programs can be executed using the
  emulator, and they can also be executed by simulating the digital
  circuit using the Hydra tools.

* The User Guide is displayed in the running application, and you can
  also read it here without launching the program: [User
  Guide](app/doc/html/userguide-index.html) The user manual is
  available as html in datafiles/doc/html/index.html.

*Note (autumn 2019): The architecture, software, and documentation are
currently being revised; this is a development version and it is not
ready for general use.  There are several earlier releases, as well as
extensive documentation, but those are available only on a protected
Moodle server.  The new version is expected to be available on github
around the beginning of February 2020.  The notes below, as well as
the user guide, are incomplete and refer to previous versions, so the
current documentation is inconsistent with the current software.*

## Running the app

The software is implemented in JavaScript and can be executed directly
in a web browser simply by clicking the "Quick start" link at the top
of this page.

Alternatively, you can download the source files from the [source
repository.](https://github.com/jtod/Sigma16)
	
ownload the files and visit index.html in a browser. After the
download, you won't need to be connected to the Internet, but a few
features won't work.  When you open one of the example programs, you
need to copy it and paste it into the Editor tab (the button "Copy
example to editor" won't work).

  * *Build an executable* compiled for your platform.  The app will
    run faster, it has better ability to save files, and it doesn't
    need access to the Internet.

### Releases

  * Version 3.1 is planned for release in February 2020.
  
  * Releases before the Version 3 series are no longer supported.

### Development versions

The following versions are intended for development and testing of the
software, and are not recommended for general use.  There will be
incomplete and experimental features, as well as inconsistencies
between the code and the documentation.

  * [Run the latest development version](./dev/app/Sigma16.html) This
    is a pre-release for the next version.  It is intended for
    development only, may be unstable, and may not be suitable for
    general users.

  * [3.0.32](./releases/3.0.32/index.html) pre-release testing version

  * [3.0.31](./releases/3.0.31/index.html) pre-release testing version

  * [3.0.30](./releases/3.0.30/index.html) pre-release testing version

  * [3.0.29](./releases/3.0.29/index.html) pre-release testing version

  * [3.0.28](./releases/3.0.28/index.html) pre-release testing version

  * [version 3.0.27](./releases/Sigma16-3.0.27/index.html) Test version

## About the software

This is free software.  The author is [John
O'Donnell](https://jtod.github.io/index.html).  Email:
john.t.odonnnell9@gmail.com

The Sigma16 app is implemented in JavaScript, html 5, and css. A fast
emulator written in C will be available soon.  The digital circuit is
implemented in Hydra, which requires Haskell.
