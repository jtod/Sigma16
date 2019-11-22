# Sigma16

## Finding the documentation

* For brief release notes, see index.html, Readme.md, and LICENSE.
  These files are in the Sigma folder.

* The user manual is available as html in
  datafiles/doc/html/index.html.  If that file doesn't exist, you can
  read the markdown source in src/docsrc.index.md.

# Required software tools

The following tools are required:

    git
    javascript
    html5
    css

# Standalone executable using electron

The following software needs to be installed in order to build the executable

    node.js
    npm

The following files are required

    src/main.js
    src/package.json
    src/package-lock.json
    src/preload.js
    src/renderer.hs
    src/node_modules/



# Compiling a standalone executable;  requires Node.js and npm

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


## Launching Sigma16

* Running as a web page

* Running as a standalone app on your computer

    npm install
    npm start



* Launch the executable file Sigma16.exe.

* If the executable doesn't exist you can build the software from source.

## To build the software

If your Sigma folder already contains the executable file Sigma16.exe.
See the section Launching Sigma16.

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

