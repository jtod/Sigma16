// Sigma16.js
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

//-----------------------------------------------------------------------------
// Sigma16.mjs defines command line tools
// Usage: node Sigma16.mjs <command> <argument>
// Software requirements (install with npm): node.js, express
//-----------------------------------------------------------------------------

// Specify port to use for communicating with the browser
const port = 3000;  // this can be changed to avoid clash with any other app

// Summary of commands
//   - node Sigma16.mjs
//   - node Sigma16.mjs gui
//        Launch gui.  In browser visit http://localhost:3000/
//        Runs locally from downloaded files; it doesn't need Internet access.
//        Replace 3000 with the value of port defined above,
//   - node Sigma16.mjs assemble foo
//        Read foo.asm.txt and translate to machine language
//        Write foo.obj.txt, foo.lst.txt, foo.md.txt

//-----------------------------------------------------------------------------
// Initialization
//-----------------------------------------------------------------------------

// Standard packages
import express from 'express';
import path from 'path';
import * as fs from "fs";
import { fileURLToPath } from 'url';

// Components of Sigma16
import {runAssembler} from "./src/js/assembler.mjs";

// Find path to this module
const Sigma16directory = path.dirname(fileURLToPath(import.meta.url));


//-----------------------------------------------------------------------------
// Initialize server for running gui
//-----------------------------------------------------------------------------

// Run express
const app = express();

// Provide locations of static source files, docs, and examples
app.use( '/', express.static(path.join(Sigma16directory + "/src/js")));
app.use( '/docs', express.static(path.join(Sigma16directory + "/docs")));
app.use( '/examples', express.static(path.join(Sigma16directory + "/examples")));

// Deliver proper mime types as required by CORS
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});

// Respond to get requests by sending the requested files
app.get('/', (req, res) => res.sendFile(Sigma16directory + '/src/js/Sigma16.html'));
app.get('*', (req, res) => res.sendFile(Sigma16directory));

//-----------------------------------------------------------------------------
// Run gui in browser (visit http://localhost:<port>/)
//-----------------------------------------------------------------------------

// Start the server
function launchGUI () {
    app.listen(port, () => console.log(`Listening on port ${port}`));
    console.log (`Open http://localhost:${port}/ in your browser`);
}

//-----------------------------------------------------------------------------
// Assembler
//-----------------------------------------------------------------------------

// Usage: node Sigma16.mjs assemble myprog
//   Reads source from myprog.asm.txt
//   Writes object code to myprog.obj.txt
//   Writes metadata to myprog.md.txt
//   Writes listing to standard output (or myprog.lst.txt?)

function cmdAssembler () {
    console.log ("runAssembler");
    let m;    // holds module data structure
    let src;  // holds source code
}

// baseName is the name of the module; source is baseName.asm.txt
function asmMain (baseName) {
    console.log (`assemble: base name = ${baseName}`);
    let srcFile = `${baseName}.asm.txt`;
    let objFile = `${baseName}.obj.txt`;
    let lstFile = `${baseName}.lst.txt`;
    let mdFile = `${baseName}.md.txt`;
    console.log (`source file = ${srcFile}`);
    console.log (`object file = ${objFile}`);
    console.log (`listing file = ${lstFile}`);
    console.log (`metadata file = ${mdFile}`);
    let src = "";
    try {
        src = fs.readFileSync(srcFile, 'utf8')
    } catch (err) {
        console.error(`Cannot read file ${srcFile}`);
    }
    console.log ("Starting assembler");
    asmRun (src, objFile, lstFile, mdFile);
    console.log ("Finished");
}

function asmRun (src, objFile, lstFile, mdFile) {
    console.log ("run");
    console.log (src);
//    m = assembler.mkModuleAsm ();
//    assembler.runAssembler ();
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

//-----------------------------------------------------------------------------
// Main program
//-----------------------------------------------------------------------------

// for now, just gui. later, switch (command)

function main  () {
    // Obtain command line arguments
    //    process.argv[0] = path to node.exe
    //    process.argv[1] = path to this file
    let command = process.argv[2]; // command to execute; if undefined use gui
    let commandArg = process.argv[3]; // argument to the command, if any
    // For testing, print parameters
    console.log (`Sigma16: command=${command} arg=${commandArg}`);
    console.log (`Sigma16directory=${Sigma16directory}`);
    if (process.argv.length < 3 || command === "gui") {
        console.log ("Launching gui");
        launchGUI ();
    } else if (command === "assemble") {
        console.log ("running assembler");
        asmMain (commandArg);
    } else if (command === "link") {
        console.log ("running linker");
    } else if (command === "emulate") {
        console.log ("running emulator");
    } else {
        console.log ("bad command");
    }
}

// Run the main program
main ();
