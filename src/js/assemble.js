// Sigma16: assemble.js
// Copyright (C) 2020 John T. O'Donnell
// email: john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

// This file is part of Sigma16.  Sigma16 is free software: you can
// redistribute it and/or modify it under the terms of the GNU General
// Public License as published by the Free Software Foundation, either
// version 3 of the License, or (at your option) any later version.
// Sigma16 is distributed in the hope that it will be useful, but
// WITHOUT ANY WARRANTY; without even the implied warranty of
// MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
// General Public License for more details.  You should have received
// a copy of the GNU General Public License along with Sigma16.  If
// not, see <https://www.gnu.org/licenses/>.

"use strict";

const assembler = require("./assembler");
const mod = require ("./module");
const fs = require ("fs");

let m;    // holds module data structure
let src;  // holds source code

//-----------------------------------------------------------------------------
// assemble.js is a command line interface to the assembler
//-----------------------------------------------------------------------------

// Usage: node assemble.js myprog
//   Reads source from myprog.asm.txt
//   Writes object code to myprog.obj.txt
//   Writes metadata to myprog.md.txt
//   Writes listing to standard output (or myprog.lst.txt?)

// It may be helpful to make some definitions.

// Example: cygwin bash on Windows.  Copy the following lines into
// your ~/.bashrc file.  Edit the fist line, defining SIGMA16, to give
// the location where you copied the Sigma16 folder.

//   # Sigma16: define aliases for commands
//   SIGMA16=/path/to/myinstallation/Sigma16
//   export SIGMA16
//   alias assemble="node ${SIGMA16}/app/assemble.js"

// ------------
// Testing import


// foobar.sayfoobar ("calling the module foobar");
//foobar.sayfoobar ("calling the module foobar");
// foobar.sayfoobar ("calling the module foobar");
// foobar.sayfoobar ("calling the module foobar");

// let m = mkModuleAsm ();


//-------------------------------------------------------------------------------

// Modules to control application life and create native browser window
// const path = require('path')


main ();

// Command line args:
//   argv[0] = node.js binary
//   argv[1] = this program
//   argv[2] = Sigma16 module file basename

function main () {
    // argv[0] = node exe
    // argv[1] = this program
    // argv[2] = first command line argument = file base name
    if (process.argv.length !== 3) {
        console.log ("usage: node assemble.js filebasename");
    } else {
        let baseName = process.argv[2];
        console.log (`assemble: base name = ${baseName}`);
        let srcFile = `${baseName}.asm.txt`;
        let objFile = `${baseName}.obj.txt`;
        let lstFile = `${baseName}.lst.txt`;
        let mdFile = `${baseName}.md.txt`;
        console.log (`source file = ${srcFile}`);
        console.log (`object file = ${objFile}`);
        console.log (`listing file = ${lstFile}`);
        console.log (`metadata file = ${mdFile}`);
        src = "";
        try {
            src = fs.readFileSync(srcFile, 'utf8')
        } catch (err) {
            console.error(`Cannot read file ${srcFile}`);
        }
        console.log ("Starting assembler");
        run (src, objFile, lstFile, mdFile);
        console.log ("Finished");
    }
}

function run (src, objFile, lstFile, mdFile) {
    console.log ("run");
    console.log (src);
    m = assembler.mkModuleAsm ();
    assembler.runAssembler ();
//    let objtext = "dummy object code";
//    writeObject (objFile, objtext);
    console.log ("run finished");
}

function writeObject (fname, obj) {
    try {
        const file = fs.writeFileSync(fname, obj);
    } catch (err) {
        // console.error(err)
        console.error(`Unable to write object code to ${fname}`);
    }
}
