// Sigma16.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3
// See Sigma16/README, LICENSE, and https://jtod.github.io/home/Sigma16

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

//----------------------------------------------------------------------
// Usage
//----------------------------------------------------------------------

// Sigma16.mjs defines command line tools using node.js

/*
Usage: node Sigma16.mjs <command> <argument> ... <argument>

<command> is one of
  (empty)                   run gui using express, can view in browser
  gui (or no command)       run gui using express from dev directory
  run p                     run gui using path p
  assemble ProgName         translate ProgName.asm.txt to object
  link ExeName ModName...   link the modules and produce executable
  parameters                display the parameters and file paths

It's convenient to define "sigma16" as an alias for "node
/path/to/Sigma16.mjs" (see Installation below)

  sigma16                 same as "sigma16 gui"
  sigma16 gui             launch gui, visit http://localhost:3000/
  sigma16 run x           launch gui using version in path x
  sigma16 assemble x      read x.asm.txt, write .obj.txt, .lst.txt, .md.txt
  sigma16 link x m1 m2... read m1.obj.txt ..., write x.obj.txt
  sigma16 parameters      display parameters and exit
  sigma16 test            for development only

** linker: node Sigma16.mjs link <exe> <mod1> <mod2> ...
  Reads object from <mod1>.obj.txt, ...     (required)
  Reads metadata from <mod1>.md.txt, ...    (ok if they don't exist)
  Writes executable to <exe>.obj.txt
  Writes metadata to <exe>.md.txt
  Writes linker listing to <exe>.lst.txt
*/

//----------------------------------------------------------------------
// Installation
//----------------------------------------------------------------------

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


//----------------------------------------------------------------------
// Initialization
//----------------------------------------------------------------------

// Standard packages
// import express from 'express';
import path from 'path';
import * as fs from "fs";
import { fileURLToPath } from 'url';

// Components of Sigma16
// import * as cn from "../gui/config.mjs";
import * as smod from "../base/s16module.mjs";
import * as st from "../base/state.mjs";
import * as asm  from "../base/assembler.mjs";
import * as link from "../base/linker.mjs";
import * as serv from '../server/sigserver.mjs'

// Find paths to components of the software, relative to this file
const cliDir = path.dirname (fileURLToPath (import.meta.url));
const homeDir = path.join (cliDir, "/../../")
const guiDir = path.join (homeDir, "/src/gui/")
const baseDir = path.join (homeDir, "/src/base/");
const compatDir = path.join (homeDir, "/src/compatibility/")
const datafilesDir = path.join (homeDir, "/src/datafiles/");
const docsDir = path.join (homeDir, "/docs/")
const examplesDir = path.join (homeDir, "/examples");

// Obtain command line arguments
//   process.argv[0] = path to node.exe
//   process.argv[1] = path to this file
let command = process.argv[2]; // command to execute; if undefined use gui
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

//----------------------------------------------------------------------
// Main program: dispatch on command
//----------------------------------------------------------------------

// Decide what operation is being requested, and do it
function main  () {
    if (process.argv.length < 3 || command === "gui") {
        console.log ("calling StartServer")
        serv.StartServer ("gui", "")
    } else if (command === "run") {
        console.log (`calling StartServer with ${commandArg}`)
        serv.StartServer ("run", "")
    } else if (command === "version") {
        console.log (`calling StartServer with ${commandArg}`)
        serv.StartServer (command, commandArg)
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

//----------------------------------------------------------------------
// Assembler
//----------------------------------------------------------------------

// st is a global system state with module set container

// Usage: node Sigma16.mjs assemble Foo
//   Read source from Foo.asm.txt
//   Write object code to Foo.obj.txt
//   Write metadata to Foo.md.txt
//   Write listing to Foo.lst.txt

function assembleCLI () {
    const baseName = process.argv[3]; // first command argument
    console.log (`assembleCLI baseName=${baseName}`)
    const srcFileName = `${baseName}.asm.txt`
    const srcText = readFile (srcFileName)
//    st.env.moduleSet = new st.ModuleSet ()
//    let m = st.env.moduleSet.addModule ()
//    const ai = new st.AsmInfo (m) // ai points to m
//    m.asmInfo = ai // m points to ai
//    m.changeAsmSrc (srcText)
//    m.setModuleName (baseName)
    //    let dummy = asm.assembler (m)   // ai
    const ai = asm.assembler (baseName, srcText)
    console.log ("assembler finished, object =")
    let obj = ai.objectText
    let md = ai.mdText
    let lst = ai.metadata.listingPlain.join("\n")
    console.log ('assembleCLI has ai') // testing
    if (ai.nAsmErrors === 0) {
        console.log ('Successful assembly')
        console.log ("\n\n\nobj = ")
        console.log (obj)
        console.log ("end obj\n\n\n")
        console.log (md)
        writeFile (`${baseName}.obj.txt`, obj)
        writeFile (`${baseName}.md.txt`,  md)
        writeFile (`${baseName}.lst.txt`, lst)
    } else {
        console.log (`There were ${ai.nAsmErrors} assembly errors`)
        let lst = ai.asmListingText
        writeFile (`${baseName}.lst.txt`, lst.join("\n"))
        process.exitCode = 1
    }
}
// old asm...
//    console.log (ai.objectText)
//    m.asmInfo = ai
//    m.objMd = ai.objMd
        //        let md = ai.metadata.toText ()
    // set up call to asm.assembler
    //    let m = ... create new module
    //    const ai = asm.assembler (baseName, srcText)
//    let m = new st.Sigma16Module ()

    // create module container and insert the source text
//    const env = new st.SystemState () defined in state
//    env.moduleSet = new st.ModuleSet ()
    // see gui.mjs, selectExample () ...
    // see gui.mjs initializeGuiElements (gst)
//    st.handleSelect (m)
//    m.baseName = baseName
//    m.currentAsmSrc = srcText

//----------------------------------------------------------------------
// Command line interface to linker
//----------------------------------------------------------------------

// Usage: node sigma16.mjs link <exe> <mod1> <mod2> ...
//   Reads object from <mod1>.obj.txt, ...     (required)
//   Reads metadata from <mod1>.md.txt, ...    (ok if they don't exist)
//   Writes executable to <exe>.obj.txt
//   Writes metadata to <exe>.md.txt
//   Writes linker listing to <exe>.lst.txt

// Command line interface to the Linker.  Read the object code from
// the specified files, create an S16Module for each, and populate
// with the object code and the metadata (if it exists).  Create an
// S16Module for the executable, call the linker, and write the
// executable file and linker listing.

function linkCLI () {
    const exeBaseName = process.argv[3]; // first command argument
    const objBaseNames = process.argv.slice(4); // subsequent command arguments
    const exeMod = st.env.mkSelectModule (exeBaseName);
    let objMods = []; // containers for object modules to be linked
    for (const baseName of objBaseNames) {
        const objText = readFile (`${baseName}.obj.txt`);
        const objMd = readFile (`${baseName}.md.txt`);
        const mod = new st.S16Module (baseName);
        const objInfo= new link.ObjectInfo (mod); // object code info
        mod.objInfo = objInfo;
        objInfo.omText = objText;
        objInfo.omMd = objMd;
        objMods.push (mod);
    }
    const {exeCode, lnkTxt} = link.linker (exeMod, objMods);
    writeFile (`${exeBaseName}.exe.txt`, exeCode);
    writeFile (`${exeBaseName}.lnk.txt`, lnkTxt);
}

//----------------------------------------------------------------------
// File I/O via file names (runs in node but not in browser)
//----------------------------------------------------------------------

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

//----------------------------------------------------------------------
// Testing
//----------------------------------------------------------------------

function runtest () {
    console.log ("runtest");
/*
    const x = new st.Metadata ();
    console.log (x.toText());
    x.addMappingSrc (4,104, "fourplain", "fourdec");
    x.addMappingSrc (7,107, "sevenplain", "sevendec");
    x.addMappingSrc (1,101, "one", "one dec");
    x.addMappingSrc (2,102, "two", "two dec");
    x.addMappingSrc (3,103, "three", "three dec");
    x.addMappingSrc (8,104, "eight", "eight dec");
    x.addMappingSrc (9,109, "nine", "nine dec");

    console.log (x.toText());
    console.log (x.getSrcPlain(4));
    console.log (x.getSrcPlain(5));  // not defined
    console.log (x.getSrcDec(7));
    console.log (x.toText());
    console.log (x.getSrcPlain(3));
    console.log (x.getSrcDec(9));
    let a = x.fromText ("2,3,4\n5,6,7,8\n10,11\nsource\nalpha ");
*/

    console.log ("........................................");
    const foo = new st.Metadata ();
    foo.fromText("100,0,101,1,102,2\nsource\nPLalpha\nDEalpha\nPLbeta\nDEbeta\nPLgamma\nDEgamma\n");
    console.log (`pairs ${foo.pairs}`);
    console.log (`plain ${foo.plain}`);
    console.log (`dec ${foo.dec}`);
    console.log ("********************** final result foo ************");
    console.log (`pairs.len = ${foo.pairs.length}`);
    console.log (`mapArr.len = ${foo.mapArr.length}`);
    console.log (`plain.len = ${foo.plain.length}`);
    console.log (`dec.len = ${foo.dec.length}`);
    console.log (`100 ${foo.getSrcIdx(100)}`);
    console.log (`101 ${foo.getSrcIdx(101)}`);
    console.log (`102 ${foo.getSrcIdx(102)}`);
    console.log (`pl 101 ${foo.getSrcPlain(101)}`);
    console.log ("***************** final result foo.teText ************");
    console.log (foo.toText());
    
 /*    
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
*/
    
    console.log ("runtest finished");
}

//----------------------------------------------------------------------
// Run the main program
//----------------------------------------------------------------------

console.log ("Starting main")
main ();
