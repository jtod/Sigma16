// Sigma16: assemble.js
// Copyright (c) 2020 John T. O'Donnell.  john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. Sigma16/ LICENSE.txt NOTICE.txt

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

//-------------------------------------------------------------------------------
// assemble.js is a command line main program that assembles a Sigma16
// source file.  Requires that node.js is installed: open a shell,
// then node --version

// Usage: node assemble.js myprog
//   Reads source from myprog.asm.txt
//   Writes object code to myprog.obj.txt
//   Writes metadata to myprog.md.txt

// It may be helpful to define:  alias assemble="node assemble.js"

//-------------------------------------------------------------------------------

// Modules to control application life and create native browser window
// const path = require('path')

const fs = require ("fs");
main ();

// Command line args:
//   argv[0] = node.js binary
//   argv[1] = this program
//   argv[2] = Sigma16 module file basename

function main () {
    console.log ("assemble");
    console.log (process.argv); // display command line arguments
    let sourcefile = `${process.argv[2]}.asm.txt`;
    let objfile = `${process.argv[2]}.obj.txt`;
    console.log (`input file = ${sourcefile}`);
    console.log (`output file = ${objfile}`);
    try {
        const data = fs.readFileSync(sourcefile, 'utf8')
        runAssembler(sourcefile,objfile,data);
    } catch (err) {
        console.error(`Cannot read file ${sourcefile}`);
    }
}

function runAssembler (sourcefile,objfile,src) {
    console.log (src);
    let objtext = "bye bye";
    console.log ("assemble finished");
    writeObject (objfile,objtext);
}

function writeObject (fname,obj) {
    try {
        const file = fs.writeFileSync(fname,obj);
    } catch (err) {
        // console.error(err)
        console.error(`Unable to write object code to ${fname}`);
    }
}
