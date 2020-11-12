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

// Sigma16.mjs defines command line tools

//-----------------------------------------------------------------------------
// ***** Configuration *****
//-----------------------------------------------------------------------------

// Specify the port to use for communicating with the browser.  This
// should be between 1024 and 49151.  The default is defined to be
// 3000; this can be changed to avoid clash with any other
// application.

const port = 3000;

//-----------------------------------------------------------------------------
// Usage
//-----------------------------------------------------------------------------

/*
Usage: node Sigma16.mjs <command> <argument> ... <argument>

<command> is one of
  (empty)                   run gui using express, can view in browser
  gui (or no command)       run gui using express, can view in browser
  assemble ProgName         translate ProgName.asm.txt to object
  link ExeName ModName...   link the modules and produce executable
  parameters                display the parameters and file paths

** linker: node Sigma16.mjs link <exe> <mod1> <mod2> ...
  Reads object from <mod1>.obj.txt, ...     (required)
  Reads metadata from <mod1>.md.txt, ...    (ok if they don't exist)
  Writes executable to <exe>.obj.txt
  Writes metadata to <exe>.md.txt
  Writes linker listing to <exe>.lst.txt

Examples, assuming alias for sigma16 is defined (see Installation below)

  sigma16                   launch gui, visit http://localhost:3000/
  sigma16 gui               launch gui, visit http://localhost:3000/
  sigma16 assemble foo      read foo.asm.txt, write foo.obj.txt, .lst.txt, .md.txt
  sigma16 link foo m1 m2... read m1.obj.txt ..., write foo.obj.txt
  sigma16 parameters        display parameters and exit
  sigma16 test              for development only
*/

//-----------------------------------------------------------------------------
// Installation
//-----------------------------------------------------------------------------

/*
1. Software requirements: install npm, see https://nodejs.org/en/download/
2. Install dependencies: npm install (this will install express)
3. Download the source directory from https://jtod.github.io/home/Sigma16/
   put it anywhere in your file space:   /path/to/SigmaSystem/Sigma16
4. Add the following to your .profile or .bashrc:

# Sigma system: https://jtod.github.io/home/Sigma16/
export SIGMASYSTEM="/path/to/SigmaSystem"
export SIGMA16=${SIGMASYSTEM}/Sigma16
alias sigma16="node ${SIGMA16}/src/Sigma16/cli/Sigma16.mjs"

*/


//-----------------------------------------------------------------------------
// Initialization
//-----------------------------------------------------------------------------

// Standard packages
import express from 'express';
import path from 'path';
import * as fs from "fs";
import { fileURLToPath } from 'url';

// Components of Sigma16
import * as smod from "../base//s16module.mjs";
import * as asm  from "../base//assembler.mjs";
import * as link from "../base//linker.mjs";

// Find paths to components of the software, relative to this file
const cliDir = path.dirname (fileURLToPath (import.meta.url));
const homeDir = path.join (cliDir, "/../../")
const guiDir = path.join (homeDir, "/src/gui/")
const baseDir = path.join (homeDir, "/src/base/");
const datafilesDir = path.join (homeDir, "/src/datafiles/");
const docsDir = path.join (homeDir, "/docs/")
const examplesDir = path.join (homeDir, "/examples");

// Obtain command line arguments
//   process.argv[0] = path to node.exe
//   process.argv[1] = path to this file
let command = process.argv[2];     // command to execute; if undefined use gui
let commandArg = process.argv[3];  // argument to the command, if any

// Display all the parameters and file paths
function showParameters () {
    console.log (`Sigma16: command=${command} arg=${commandArg}`);
    console.log (`homeDir =\n${homeDir}`);
    console.log (`cliDir =\n${cliDir}`);
    console.log (`guiDir =\n${guiDir}`);
    console.log (`baseDir =\n${baseDir}`);
    console.log (`datafilesDir =\n${datafilesDir}`);
    console.log (`docsDir =\n${docsDir}`);
    console.log (`examplesDir =\n${examplesDir}`);
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
    } else if (command === "parameters") {
        showParameters ()
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
app.use( '/home', express.static (homeDir));
app.use( '/base', express.static (baseDir));
app.use( '/datafiles', express.static (datafilesDir));
app.use( '/docs', express.static (docsDir));
app.use( '/examples', express.static (examplesDir));

// Deliver mime types as required by CORS
express.static.mime.define({'application/javascript': ['js']});
express.static.mime.define({'text/css': ['css']});

// Respond to get requests by sending the requested files
app.get('/', (req, res) => res.sendFile(guiDir + '/Sigma16.html'));

app.get('/gui/*', (req, res) => res.sendFile(guiDir));
app.get('/base/*', (req, res) => res.sendFile(baseDir));

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
    let ma = asm.assemblerCLI (maybeSrc);
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

//-----------------------------------------------------------------------------
// Linker
//-----------------------------------------------------------------------------

// Usage: node Sigma16.mjs link <exe> <mod1> <mod2> ...
//   Reads object from <mod1>.obj.txt, ...     (required)
//   Reads metadata from <mod1>.md.txt, ...    (ok if they don't exist)
//   Writes executable to <exe>.obj.txt
//   Writes metadata to <exe>.md.txt
//   Writes linker listing to <exe>.lst.txt

// Command line interfact to the Linker.  Call it with the executable
// name and a list of object modules; each object module should be
// initialized with the module name, the object text, and the metadata
// (null if there is no metadata file).  The linkCLI function obtains
// the file names from the command line, reads the files, creates the
// object modules, and calls the linker.

function linkCLI () {
    const exeBaseName = process.argv[3]; // first command argument
    const modBaseNames = process.argv.slice(4); // subsequent command arguments
    let oms = []; // list of object modules
    for (const baseName of modBaseNames) {
        let objText = readFile (`${baseName}.obj.txt`);
        let objMd = readFile (`${baseName}.md.txt`);
        let om = new link.ObjectModule (baseName, objText, objMd);
        oms.push (om);
    }
    link.linker (exeBaseName, oms);
}

//-----------------------------------------------------------------------------
// File I/O via file names (runs in node but not in browser)
//-----------------------------------------------------------------------------

// Attempt to read file fname.  If file can't be read, give error
// message and return empty string; otherwise return the contents of
// the file.

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
    return (ok ? input : "");
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
