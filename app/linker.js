// Sigma16: linker.js
// Copyright (C) 2019, 2020 John T. O'Donnell
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

//-------------------------------------------------------------------------------
// linker.js combines a collection of object modules to form an
// executable module, performing address relocation as needed.
//-------------------------------------------------------------------------------

// refactor
var exMod;           // the module that is executing
var curAsmap = [];

//-------------------------------------------------------------------------------
// Representation of object module
//-------------------------------------------------------------------------------

// Information about object code
function mkModuleObj () {
    return {
        objText : "",      // text of object code
        objCode  : [],     // lines of object code
        objStmt  : []      // array of statements in object code
    }
}

//-------------------------------------------------------------------------------
// Handle modules and display
//-------------------------------------------------------------------------------

// Clear the display of the object code in the linker pane
function clearObjectCode () {
    let listing = "<pre class='HighlightedTextAsHtml'>"
        + "</pre>"
    document.getElementById('LinkerText').innerHTML = listing;
}


// called when Linker: Selected module button is clicked
function linkShowSelectedObj () {
    console.log ("linkShowSelectedObj");
    let m = s16modules[selectedModule]; // get current module
    let objListing = "<pre class='HighlightedTextAsHtml'>"
        + "<span class='ExecutableStatus'>Module is "
        + (m.objIsExecutable ? "executable" : "not executable" )
        + "</span><br>"
        + m.objInfo.objCode.join('\n')
        + "</pre>";
    document.getElementById('LinkerText').innerHTML = objListing;
    // dev only
    linkerParseObject (m);
}

// Called by button in Linker tab; this is used when an object file is
// to be read in rather than being assembled now
function readObject () {
    console.log ('readObject');
    let m = s16modules[selectedModule]; // get current module
    let mo = m.objInfo;
    if (mo) { // it exists, proceed
    } else { // doesn't exist, error
        console.log ('readObject error: no module');
    }
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

function showLinkerStatus () {
    console.log ('showLinkerStatus');
    let m = s16modules[selectedModule]; // get current module
}

function setLinkerModules () {
    console.log ('setLinkerModules');
}


//-------------------------------------------------------------------------------
// Parse object code
//-------------------------------------------------------------------------------

function linkerParseObject (m) {
    console.log ("linkerParseObject");
    let mo = m.objInfo;
    for (let i = 0; i < mo.objCode.length; i++) {
        console.log (`line ${i}: ${mo.objCode[i]}`);
    }
    console.log ("linkerParseObject returning");
}

// A line of object code contains a required operation code, white
// space, and a required operand which is a comma-separated list of
// fields that may contain letters, digits, and commas.
const objLineParser = /^([a-z]+)\s+([\w,]+)$/;
const objOperandParser = /^$/;

function parseObjLine (i,s) {
    let splitLine = objLineParser.exec (s);
    if (splitLine) {
        let operationField = splitLine[1];
        let operandField = splitLine[2];
        console.log (`operationField=${operationField}`);
        console.log (`operandField=${operandField}`);
        let operands = operandField.split(',');
        console.log (`There are ${operands.length} operands`);
        console.log (`operands=${operands}`);
        console.log (`operand[0]=${operands[0]}`);
        console.log (`operand[1]=${operands[1]}`);
        console.log (`operand[2]=${operands[2]}`);
        objStmt.push(mkObjStmt(i,s,operationField,operands));
    } else {
        console.log ('object line has invalid format: ' + s);
    }
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
        objCode : []
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
