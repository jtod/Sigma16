// Sigma16: linker.mjs
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
// linker.mjs manipulates object code, including the functions of a
// linker and loader.  Services include combining a collection of
// object modules to form an executable module; performing address
// relocation; and loading an object module into memory.
//-----------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as st from './state.mjs';

// refactor
let curAsmap = [];

//-----------------------------------------------------------------------------
// Representation of object module
//-----------------------------------------------------------------------------

// An object module is the code for one module; it may contain linker
// statements and may comprise one or more blocks

class ModuleObj {
    constructor() {
        this.objLine = [];                 // lines of code split by newline
        this.objStmt = [];                 // array of statements in object code
        this.objMetadataLine = null;  // optional metadata lines
    }
}

// A Block is a sequence of words in adjacent memory locations.  The
// functional argument f emits the next word of object code.

class Block {
    constructor(f) {
        this.startAddr = 0;
        this.current = 0;
        this.words = [];
        this.f = f;
    }
    insert(w) {
        this.words.push(w);
    }
    relocate(x) {
        this.startAddr = x;
    }
    emit () {
        if (this.current < this.words.length) {
            this.f (this.current+this.startAddr, this.words[this.current]);
            this.current++;
        } else {
            console.log (`emit: out of data`);
        }
        return (this.current >= this.words.length);
    }
}

/* Usage
function fcn (a,w) { console.log (`emit ${a} ${w}`); }
function testBlock() {
    let t1 = new Block (fcn);
    t1.insert(12);
    t1.insert(34);
    t1.insert(56);
    console.log (t1.emit());
    console.log (t1.emit());
    console.log (t1.emit());
    console.log (t1.emit());
}
*/

//-------------------------------------------------------------------------------
// Operations on selected module
//-------------------------------------------------------------------------------


// Linker Object code button
export function linkShowSelectedObj () {
    console.log ("linkShowSelectedObj");
    let m = smod.s16modules[smod.selectedModule]; // get current module
    let objListing = "<pre class='HighlightedTextAsHtml'>"
        + "<span class='ExecutableStatus'>Module is "
        + (m.objIsExecutable ? "executable" : "not executable" )
        + "</span><br>"
        + m.objInfo.objLine.join('\n')
        + "</pre>";
    document.getElementById('LinkerText').innerHTML = objListing;
}

// Called by button in Linker tab; this is used when an object file is
// to be read from editor rather than being created by the assembler
export function readObjectFromEditor () {
    console.log ('readObjectFromEditor');
    let m = smod.s16modules[smod.selectedModule]; // get current module
    let mo = m.objInfo;
    if (mo) { // it exists, proceed
        let txt = document.getElementById("EditorTextArea").value;
        mo.objLine = txt.split("\n");
        document.getElementById('LinkerText').innerHTML = txt;
    } else { // doesn't exist, error
        console.log ('readObject error: no module');
    }
}

// Linker Metadata button
export function linkShowMetadata () {
    console.log ("linkShowMetadata");
    let m = smod.s16modules[smod.selectedModule]; // get current module
    let mo = m.objInfo;
    let md = "<pre class='HighlightedTextAsHtml'>"
        + (mo.objMetadataLine? mo.objMetadataLine.join("\n") : "no metadata")
        + "</pre>";
    document.getElementById('LinkerText').innerHTML = md;
}


function setCurrentObjectCode () {
    let objHeader = "Module " + selectedModule + " object code"
    let objText =
	"<pre class='HighlightedTextAsHtml'><span class='ListingHeader'>"
	+ objHeader + "</span>\n"
	+ smod.s16modules[smod.selectedModule].objectCode.join('\n')
	+ "</pre>";
    document.getElementById('LinkerText').innerHTML	= objText;

}

// Given source lines for the metadata, build the metadata object for emulator
function parseObjMetadata (mdLines) {
    console.log ("parseObjMetadata");
    console.log (mdLines);
    let mdAsMap = [];
    let mdPlainLines = [];
    let mdDecLines = [];
    let i = 0;
    while (i < mdLines.length & mdLines[i] != "Source") {
        console.log (`asmap entry ${mdLines[i]}`);
        i++
    }
    console.log (`source starts at line ${i} --${mdLines[i]--}`);
    let nSrcLines = mdLines[i];
    console.log (`nSrcLines = ${nSrcLines}`);
    return nSrcLines;
//    return {mdLines, mdAsMap, mdPlainLines, mdDecLines}
}

//-------------------------------------------------------------------------------
// Operations on list of modules
//-------------------------------------------------------------------------------


// Test stub.  Return the list of module numbers for the modules to be
// linked
function getLinkModuleList () {
    console.log ("getLinkModuleList");
    return [1,2]; // test stub
}

function showLinkerStatus () {
    console.log ('showLinkerStatus');
    let m = smod.s16modules[smod.selectedModule]; // get current module
}

export function setLinkerModules () {
    console.log ('setLinkerModules');
}


//-------------------------------------------------------------------------------
// Linker
//-------------------------------------------------------------------------------

// linkMods is a list of module numbers of the modules to be linked


/*
function collectLinkModules (linkMods) {
    let exeMod = smod.selectedModule; // stub
    console.log ("collectLinkModules");
    console.log (`exmod=${executable} linkmods=${}`);
    let mo = smod.s16modules[exeMod]; // module number of the executable
    let exeLines = mo.objLine;
    exeLines = [];
    mo.objLine = []; // clear object lines
    for (let i = 0; i < linkMods.length; i++) {
        let hasModuleName = false; // require module name
        for (let j = 0; j < smod.s16modules[i].objLine.length; j++) {
            
        }
    }
    
}
*/



// A line of object code contains a required operation code, white
// space, and a required operand which is a comma-separated list of
// fields that may contain letters, digits, and commas.

export function parseObjLine (xs) {
    const objLineParser = /^([a-z]+)\s+([\w,]+)$/;
    let splitLine = objLineParser.exec (xs);
    let operation = "";
    let operands = [];
    if (splitLine) {
        operation = splitLine[1];
        operands = splitLine[2].split(',');
      } else {
        console.log ('linker error: object line has invalid format: ' + xs);
    }
        return {operation, operands}
}

function mkObjStmt (i,srcLine,operation,operands) {
    return {
        objLineNo : i,
        objSrcLine : objSrcLine,
        objOperation : operation,
        objOperands : operands,
        objLocation : 0,
        objSize : 0,
        objOperandNames : [],
        objLine : []
    }
}

//-------------------------------------------------------------------------------
// Link modules
//-------------------------------------------------------------------------------

function link () {
    console.log ('link');
}

// objs = list of module numbers of modules to be linked
// exe = module number of executable module to be created
function linkWorker (objs,exe) {
    console.log (`linkWorker objs=${objs} exe=${exe}`);
}

