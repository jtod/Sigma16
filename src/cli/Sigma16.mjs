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
//-----------------------------------------------------------------------------

// Usage: node Sigma16.mjs <command> <argument> ... <argument>
// See below for the commands

//-----------------------------------------------------------------------------
// Installation
//-----------------------------------------------------------------------------

// 1. Software requirements: install npm
// 2. Install dependencies: npm install (will install express)
// 3. Download the source directory from https://jtod.github.io/home/Sigma16/
//    put it anywhere in your file space:   /path/to/SigmaSystem/Sigma16
// 4. Add the following to your .profile or .bashrc:

/*
# Sigma system: https://jtod.github.io/home/Sigma16/
export SIGMASYSTEM="/path/to/SigmaSystem"
export SIGMA16=${SIGMASYSTEM}/Sigma16
alias sigma16="node ${SIGMA16}/src/cli/Sigma16.mjs"
*/

//-----------------------------------------------------------------------------
// Usage
//-----------------------------------------------------------------------------

// sigma16                     Launch gui, visit http://localhost:3000/
// sigma16 gui                 Launch gui, visit http://localhost:3000/
//                                 Replace 3000 with the value of port (see below)
// sigma16 assemble foo        Translate foo.asm.txt
//                                 Writes foo.obj.txt, foo.lst.txt, foo.md.txt
// sigma16 link exe m1 m2 ...  Link m1.obj.txt, m2.obj.txt, ...
//                                 Writes exe.obj.txt
// sigma16 test                Run batch test cases (for development only)

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
import * as smod from "../gui/s16module.mjs";
import * as asm from "../gui/assembler.mjs";
import * as link from "../gui/linker.mjs";
// import {mkModule, mkModuleAsm} from "../gui/s16module.mjs";
// import {assemblerCLI} from "../gui/assembler.mjs";

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
// Main program: dispatch on command
//-----------------------------------------------------------------------------

// Decide what operation is being requested, and do it
function main  () {
    if (process.argv.length < 3 || command === "gui") {
        launchGUI ();
    } else if (command === "assemble") {
        assembleCLI (commandArg);
    } else if (command === "link") {
        linkCLI ();
    } else if (command === "emulate") {
        console.log ("emulator");
    } else if (command === "test") {
        runtest ();
    } else {
        console.log ("bad command");
    }
}

//-----------------------------------------------------------------------------
// Launch gui in browser (visit http://localhost:<port>/)
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

function assembleCLI () {
    const baseName = process.argv[3]; // first command argument
    const srcFileName = `${baseName}.asm.txt`;
    const maybeSrc = readFile (srcFileName);
    if (!maybeSrc.ok) {
        console.log (`Unable to read assembly source file ${srcFileName}`);
    } else {
        let ma = asm.assemblerCLI (maybeSrc.input);
        if (ma.nAsmErrors === 0) {
            let obj = ma.objectCode;
            let lst = ma.asmListingText;
            let md = ma.metadata;
            writeFile (`${baseName}.obj.txt`, obj.join("\n"));
            writeFile (`${baseName}.lst.txt`, lst.join("\n"));
            writeFile (`${baseName}.md.txt`,  md.join("\n"));
        } else {
            console.log (`There were ${ma.nAsmErrors} assembly errors`);
            let lst = ma.asmListingText;
            writeFile (`${baseName}.lst.txt`, lst.join("\n"));
            process.exitCode = 1;
        }
    }
}

//-----------------------------------------------------------------------------
// Linker
//-----------------------------------------------------------------------------

// Usage: node Sigma16.mjs link <exe> <mod1> <mod2> ...
//   Reads object from <mod1>.obj.txt, ...     (required)
//   Reads metadata from <mod1>.md.txt, ...    (ok if they don't exist)
//   Writes executable to <exe>.obj.txt
//   Writes metadata to <exe>.md.txt
//   Writes linker listing to <exe>.lst.txt

function linkCLI () {
    const exeFile = process.argv[3]; // first command argument
    const modFiles = process.argv.slice(4); // subsequent command arguments
    console.log (`link: exe file = ${exeFile} from ${modFiles}`);
    let mods = [];
    for (let i = 0; i < modFiles.length; i++) {
        let baseName = modFiles[i];
        let obj = readFile (`${baseName}.obj.txt`);
        let md = readFile (`${baseName}.md.txt`);
        let objLines = obj.ok ? obj.input.split("\n") : [];
        let mdLines = md.ok ? md.input.split("\n") : [];
        let m = new smod.s16module (smod.ObjModule);
        mods[i] = m;
    }
    link.linker (exeFile, mods);
}

//-----------------------------------------------------------------------------
// File I/O via file names (runs in node but not in browser)
//-----------------------------------------------------------------------------

export function readFile (fname) {
    let input, ok;
    try {
        input = fs.readFileSync (fname, 'utf8');
        ok = true;
    } catch (err) {
        input = "";
        ok = false;
    }
    if (!ok) {
        console.error(`Cannot read file ${fname}`);
        process.exitCode = 1;
    }
    return {input, ok};
}

export function writeFile (fname, txt) {
    let ok;
    try {
        const file = fs.writeFileSync(fname, txt);
        ok = true;
    } catch (err) {
        console.error(`Unable to write to file ${fname}`);
        process.exitCode = 1;
        ok = false;
    }
    return ok;
}

//-----------------------------------------------------------------------------
// Test harness
//-----------------------------------------------------------------------------

function runtest () {
    console.log ("runtest");
    let v1 = new asm.Value (23, false);
    let v2 = new asm.Value (78, true);
    console.log (`v1 ${v1.show()}`);
    console.log (`v2 ${v2.show()}`);
    v1.update(987);
    console.log (`v1 ${v1.show()}`);

    let m1 = new smod.newS16module (smod.AsmModule, "load");
    let m2 = new smod.newS16module (smod.ObjModule, "834b");
    let s1 = m1.sym;
    let s2 = m2.sym;
    console.log (typeof s1);

    console.log (`m1.txt = ${m1.txt}`);
    console.log (`m2.txt = ${m2.txt}`);

    let aa = smod.news16modules.get(s1).txt;
    let bb = smod.news16modules.get(s2).txt;
    console.log (`s1 txt = ${aa}`);
    console.log (`s2 txt = ${bb}`);

    
    console.log ("runtest finished");
}

//-----------------------------------------------------------------------------
// Run the main program
//-----------------------------------------------------------------------------

main ();
