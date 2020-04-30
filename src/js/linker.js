// Sigma16: linker.js
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
// linker.js manipulates object code, including the functions of a
// linker and loader.  Services include combining a collection of
// object modules to form an executable module; performing address
// relocation; and loading an object module into memory.
//-----------------------------------------------------------------------------

"use strict";

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

// Clear the display of the object code in the linker pane
function clearObjectCode () {
    let listing = "<pre class='HighlightedTextAsHtml'>"
        + "</pre>"
    document.getElementById('LinkerText').innerHTML = listing;
}

// Linker Object code button
function linkShowSelectedObj () {
    console.log ("linkShowSelectedObj");
    let m = s16modules[selectedModule]; // get current module
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
function readObjectFromEditor () {
    console.log ('readObjectFromEditor');
    let m = s16modules[selectedModule]; // get current module
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
function linkShowMetadata () {
    console.log ("linkShowMetadata");
    let m = s16modules[selectedModule]; // get current module
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
	+ s16modules[selectedModule].objectCode.join('\n')
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
    let m = s16modules[selectedModule]; // get current module
}

function setLinkerModules () {
    console.log ('setLinkerModules');
}


//-------------------------------------------------------------------------------
// Linker
//-------------------------------------------------------------------------------

// linkMods is a list of module numbers of the modules to be linked


/*
function collectLinkModules (linkMods) {
    let exeMod = selectedModule; // stub
    console.log ("collectLinkModules");
    console.log (`exmod=${executable} linkmods=${}`);
    let mo = s16modules[exeMod]; // module number of the executable
    let exeLines = mo.objLine;
    exeLines = [];
    mo.objLine = []; // clear object lines
    for (let i = 0; i < linkMods.length; i++) {
        let hasModuleName = false; // require module name
        for (let j = 0; j < s16modules[i].objLine.length; j++) {
            
        }
    }
    
}
*/

//-------------------------------------------------------------------------------
// Booter
//-------------------------------------------------------------------------------

function checkObject () {
    console.log ("linker: checkObject");
}

function boot (es) {
    console.log ("boot");
    let m = s16modules[selectedModule]; // get current module
    let ma = m.asmInfo; // only if this module came from assembler
    let mo = m.objInfo;
    let xs = "";
    let fields = null;
    let ok = true; // will set to false if module isn't bootable
    let location = 0; // address where next word will be stored
    resetRegisters();
    memClearAccesses();
    document.getElementById('ProcAsmListing').innerHTML = "";

    // get listing from the assembler
    es.asmListingPlain = ma.asmListingPlain;  // interface
    es.asmListingDec = ma.asmListingDec;  // interface
    
    es.asmListingCurrent = [];
    for (let i = 0; i < es.asmListingDec.length; i++) { // copy the array
	es.asmListingCurrent[i] = es.asmListingDec[i];
    }
    initListing (m,es);
    es.nInstructionsExecuted = 0;
    document.getElementById("nInstrExecuted").innerHTML =
	es.nInstructionsExecuted;
    ioLogBuffer = "";
    refreshIOlogBuffer();
    getListingDims(es);
    
    for (let i = 0; i < mo.objLine.length; i++) {
        xs = mo.objLine[i];
        console.log (`boot: object line ${i} = ${xs}`);
        fields = parseObjLine (xs);
        console.log (`op=${fields.operation} args=${fields.operands}`);
        if (fields.operation == "module") {
            let modname = fields.operands[0];
            let safemodname = modname ? modname : "(unknown)";
            console.log (`boot: module ${safemodname}`);
        } else if (fields.operation == "data") {
            console.log ("boot: data");
            for (let j = 0; j < fields.operands.length; j++) {
                let val = hex4ToWord(fields.operands[j]);
                if (!val) {console.log(`boot: bad data (${val})`)};
                let safeval = val ? val : 0;
                memStore (location,safeval);
                console.log (`boot data mem[${location}]:=${val}`);
                location++;
            }
        } else if (fields.operation == "import") {
            console.log (`boot: import (${fields.operands})`)
            ok = false;
        } else if (fields.operation == "export") {
        } else if (fields.operation == "relocate") {
        } else {
            console.log (`boot: bad operation (${fields.operation})`)
            ok = false;
        }
    }
    if (ok) {
        memShowAccesses();
        memDisplay();
        es.curAsmap = ma.asmap;
        showAsmap (es.curAsmap);
        setProcStatus (es,Ready);
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot was successful"
            + "</span><br>"
            + "</pre>";
        document.getElementById('LinkerText').innerHTML = xs;
        console.log ("boot was successful")
    } else {
        setProcStatus (es,Reset);
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot failed: module is not executable"
            + "</span><br>"
            + "</pre>";
        document.getElementById('LinkerText').innerHTML = xs;
        console.log ("boot failed");
        modalWarning ("boot failed");
    }
    console.log ("boot returning");
}


// A line of object code contains a required operation code, white
// space, and a required operand which is a comma-separated list of
// fields that may contain letters, digits, and commas.

function parseObjLine (xs) {
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

//------------------------------------------------------------------------------
// Boot
//------------------------------------------------------------------------------

// Attempt to copy the executable code from the selected module into
// memory for execution.

// A module is executable if it contains object code but doesn't have
// any imports.  The object code can come from either the assembler,
// or it can be read in as an object file.  The presence of an
// assembly listing and asmap is optional.

// (Interface: called from gui) Boot from either the current module
// (without linking) or the load module (if the linker has been run).

function oldboot(es) {
    console.log (`boot: selected module = ${selectedModule}`);
    let m = s16modules[selectedModule];
//    let m = getCurrentModule ();
    let mo = m.objInfo;
    if (mo.isExecutable) {
	console.log ('Current module is executable: booting');
	resetRegisters ();
	memClearAccesses();
        document.getElementById('ProcAsmListing').innerHTML = "";
	copyExecutableToMemory (es,m);
	es.asmListingPlain = ma.asmListingPlain;
	es.asmListingDec = ma.asmListingDec;
        es.asmListingCurrent = [];
	for (let i = 0; i < es.asmListingDec.length; i++) { // copy the array
	    es.asmListingCurrent[i] = es.asmListingDec[i];
	}
	initListing (m,es);
	es.nInstructionsExecuted = 0;
	document.getElementById("nInstrExecuted").innerHTML =
	    es.nInstructionsExecuted;
	ioLogBuffer = "";
	refreshIOlogBuffer();
        getListingDims(es);
    } else {
	console.log ('cannot boot')
    }
}

// Copy a module's object code into memory.  Should use objectCode
// rather than codeWord, but objectCode will need to support org.  So
// far it's just a list of words.  This version doesn't yet support
// org and it requires the module to be assembled (rather than read
// in).

function copyExecutableToMemory (es,m) {
    console.log ('copyExecutableToMemory');
    let ma = m.asmInfo;
    let stmt = ma.asmStmt;
    let locationCounter = 0;
    for (let i = 0; i < stmt.length; i++) {
	console.log('bootCM ' + i + ' => ' + stmt[i].codeWord1
		    + ' ' + stmt[i].codeWord2 );
	if (stmt[i].codeWord1 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord1);
	    locationCounter++;
	}
	if (stmt[i].codeWord2 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord2);
	    locationCounter++;
	}
    }
    memShowAccesses();
    memDisplay();
    es.curAsmap = ma.asmap;
    showAsmap (es.curAsmap);
    setProcStatus (es,Ready);
    console.log ('copyExecutableToMemory done');
}

function parseCopyObjectModuleToMemory (es) {
    console.log('boot');
    let objText = document.getElementById("LinkerText").value;
    console.log('objText = ' + objText);
    let xs = objText.split("\n");
    console.log("linker boot: " + xs.length + " lines");
//    console.log(xs);
//    line1 = xs[0];
//    console.log('line1 = ' + line1);
//    fields = line1.split(" ");
//    console.log('fields = ' + fields);
//    console.log('field0 = ' + fields[0]);
//    console.log('field1 = ' + fields[1]);
    //    console.log('field2 = ' + fields[2]);
    bootCurrentLocation = 0;
    for (let i = 0; i < xs.length; i++) {
	linkerBootLine(es, i, xs[i]);
//	experiment(xs[i]);
    }
}

// Should check the operation, implement org, and provide suitable
// error messages, but that's for later.  For now, just assume it is
// hexdata with valid argument

function linkerBootLine (es,m,i,x) {
    let y = parseAsmLine (m,i,x);
//    printAsmLine (y);
    let w = y.fieldOperands;
    let n =  hex4ToWord(w);
//    console.log('linkerBootLine ' + w + ' = ' + n);
    console.log('linkerBootLine ' + i + ' ---' + x + '--- = ' + n);
    updateMem2(bootCurrentLocation,n);
    bootCurrentLocation++;
}

// Prepare assembly liating when executable is booted
function initListing (m,es) {
    es.curInstrAddr = 0;
    es.curInstrLineNo = -1;  // -1 indicates no line has been highlighted
    es.nextInstrAddr = 0;
    es.nextInstrLineNo = es.curAsmap[0];
    highlightListingLine (es, es.nextInstrLineNo, "NEXT");
    setProcAsmListing (es,m);
}
