// Sigma16.mjs
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

// Summary of commands
//   - node Sigma16.mjs or node Sigma16.mjs gui
//        Launch gui.  In browser visit http://localhost:3000/
//        Runs locally from downloaded files; it doesn't need Internet access.
//        Replace 3000 with the value of port defined above,
//   - node Sigma16.mjs assemble foo
//        Read foo.asm.txt and translate to machine language
//        Write foo.obj.txt, foo.lst.txt, foo.md.txt

//-----------------------------------------------------------------------------
// Configuration
//-----------------------------------------------------------------------------

// Specify port to use for communicating with the browser.  This
// should be between 1024 and 49151.  The default is defined to be
// 3000; this can be changed to avoid clash with any other
// application.

const port = 3000;

//-----------------------------------------------------------------------------
// Initialization
//-----------------------------------------------------------------------------

// Standard packages
import express from 'express';
import path from 'path';
import * as fs from "fs";
import { fileURLToPath } from 'url';

// Components of Sigma16
import {mkModule, mkModuleAsm} from "../gui/s16module.mjs";
import {assemblerCLI} from "../gui/assembler.mjs";

// Find paths to components of the software
const cliDir = path.dirname (fileURLToPath (import.meta.url));
const homeDir = path.join (cliDir, "/../../")
const guiDir = path.join (homeDir, "/src/gui/")
const docsDir = path.join (homeDir, "/docs/")
const examplesDir = path.join (homeDir, "/examples");

// Obtain command line arguments
//   process.argv[0] = path to node.exe
//   process.argv[1] = path to this file
let command = process.argv[2];     // command to execute; if undefined use gui
let commandArg = process.argv[3];  // argument to the command, if any

function showParameters () {
    console.log (`Sigma16: command=${command} arg=${commandArg}`);
    console.log (`homeDir=${homeDir}`);
    console.log (`cliDir=${cliDir}`);
    console.log (`guiDir=${guiDir}`);
    console.log (`docsDir=${docsDir}`);
    console.log (`examplesDir=${examplesDir}`);
}

//-----------------------------------------------------------------------------
// Main program
//-----------------------------------------------------------------------------

// Decide what operation is being requested, and do it
function main  () {
    if (process.argv.length < 3 || command === "gui") {
        launchGUI ();
    } else if (command === "assemble") {
        assemble (commandArg);
    } else if (command === "link") {
        console.log ("linker");
    } else if (command === "emulate") {
        console.log ("emulator");
    } else {
        console.log ("bad command");
    }
}

//-----------------------------------------------------------------------------
// Run gui in browser (visit http://localhost:<port>/)
//-----------------------------------------------------------------------------

// Run express
const app = express();

// Provide locations of static source files, docs, and examples
app.use( '/', express.static (guiDir));
app.use( '/docs', express.static (docsDir));
app.use( '/examples', express.static (examplesDir));

// Deliver mime types as required by CORS
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});

// Respond to get requests by sending the requested files
app.get('/', (req, res) => res.sendFile(guiDir + '/Sigma16.html'));
app.get('/docs/*', (req, res) => res.sendFile(docsDir));
app.get('*', (req, res) => res.sendFile(guiDir));

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

// baseName is the name of the module; source is baseName.asm.txt
function assemble (baseName) {
    const srcFileName = `${baseName}.asm.txt`;
    let src = "";
    let srcOk = false;
    try {
        src = fs.readFileSync (srcFileName, 'utf8');
        srcOk = true;
    } catch (err) {
        srcOk = false;
    }
    if (!srcOk) {
        console.error(`Cannot read file ${srcFileName}`);
        process.exitCode = 1;
    } else {
        let ma = assemblerCLI (src);
        if (ma.nAsmErrors === 0) {
            let obj = ma.objectCode;
            let lst = ma.asmListingPlain;
            let md = "dummy metadata\n";
            writeFile (`${baseName}.obj.txt`, obj.join("\n"));
            writeFile (`${baseName}.lst.txt`, lst.join("\n"));
            writeFile (`${baseName}.md.txt`, md);
        } else {
            console.log (`There were ${ma.nAsmErrors} assembly errors`);
            process.exitCode = 1;
        }
    }
}

function writeFile (fname, txt) {
    try {
        const file = fs.writeFileSync(fname, txt);
    } catch (err) {
        console.error(`Unable to write to file ${fname}`);
        process.exitCode = 1;
    }
}


//-----------------------------------------------------------------------------
// Run the main program
//-----------------------------------------------------------------------------

main ();
