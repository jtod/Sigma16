// Sigma16: module.js
// Copyright (c) 2019,2020 John T. O'Donnell. john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. Sigma16/LICENSE.txt,NOTICE.txt

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
// module.js defines the representation of source and object modules
//-------------------------------------------------------------------------------

// The working program is a list of modules, one of which is the
// current working module visible in the editor and assembler.  A
// module data structure contains everything known about the module,
// whether it is source or object.

//------------------------------------------------------------------------------
// List of all modules

let s16modules = [];    // All the modules in the system
let nModules = 1;
let selectedModule = 0;

let currentFile = null;  // deprecated, Remember the current working file handle

// Create one initial (empty) module and make it the current module
function initModules () {
    s16modules = [mkModule()];
    let m = s16modules[0];
    m.mIndex = 0;
    
    selectedModule = 0;
}

// Get the full data structure for the current module
function getCurrentModule () {
    return s16modules[selectedModule]
}

//------------------------------------------------------------------------------
// Representation of a module
//------------------------------------------------------------------------------

// Make a new module with empty contents; this defines the fields of a module

function mkModule () {
    console.log('mkModule');
    return {
        mIndex : null,             // index in s16modules list
	mFile : null,              // file object associated with module, if any
        fileName : null,           // filename, if exists and is known
	fileReader : null,         // object to read the file
        fileRead : false,          // has file been completely read in yet?
        fileStale : false,         // contents have been changed in editor
	selected : false,          // this module is selected (for edit, asm)
	modName : null,            // name of module specified in module stmt
	modSrc : '',               // source code
	symbols : [],              // symbols used in the source
	symbolTable : new Map (),  // symbol table
	nAsmErrors : 0,            // number of errors in assembly source code
	locationCounter : 0,       // address where next code will be placed
	asmap : [],                // array mapping address to source statement
	isExecutable : true,       // until proven otherwise
	asmListingPlain : [],      // assembler listing
	asmListingDec : [],         // decorated assembler listing
	objectCode : [],           // string hex representation of object
        exports : [],
	asmStmt : []               // statements correspond to lines of source
    }
}

// Return a brief description of a module
function showModule (m) {
    let n = m.src ? m.src.length : 0;
    let xs = getModName(m) + ' (' +  n + ' characters)\n';
    return xs;
}

// Return the module name and file name, if they exist
function getModFileName (m) {
    if (m) {
        let mname = m.modName ? `${m.modName}` : '';
        let fname = m.mFile ? `(${m.mFile.name})` : '';
        let xs = (m.modName || m.mFile) ? mname + '  ' + fname : '<anonymous>';
        return xs;
    } else return "?"
}

function getModName (m) {
    return m.modName ? m.modName : "no module statement"
}

function getFileName (m) {
    return m.mFile ? m.mFile.name : "no file associated with module"
}

//-------------------------------------------------------------------------------
// Reading files
//-------------------------------------------------------------------------------

// When the user is in the Modules page and clicks Choose Files, a
// multiple file chooser widget is displayed, and its onchange event
// signals handleSelectedFiles with a list of file objects that were
// selected.  The function reads the files and creates entries in the
// modules list for them.

function handleSelectedFiles (flist) {
    console.log("handleSelectedFiles");
    let m;
    let idxFirstNewMod = nModules;
    let idxLastNewMod  = nModules + flist.length - 1;
    for (let i=0; i<flist.length; i++) {
	m = mkModule ();
        m.mIndex = nModules;
	m.mFile = flist[i];
        m.fileName = m.mFile.name;
	m.selected = false;
        m.fileRead = false;
	m.modSrc = "";
	m.fileReader = mkOfReader(m, nModules, idxFirstNewMod, idxLastNewMod);
	m.fileReader.readAsText(m.mFile);
	s16modules.push(m);
	nModules++;
    }
}

function mkOfReader (m, i,a,b) {
    console.log (`mkOfReader idx=${m.mIndex} ${i} ${a} ${b}`);
    let fr = new FileReader();
    fr.onload = function (e) {
        let idx = m.mIndex; // could change after close
	console.log (`ofReader ${idx} onload event a=${a} b=${b}`);
        //	s16modules[i].modSrc = e.target.result;
        m.modSrc = e.target.result;
        //	s16modules[i].fileRead = true;
        m.fileRead = true;
        let allOK = true;
//        for (let j=a; j<=b; j++) {
//            allOK = allOK && s16modules[j].fileRead;
//            console.log (`mkOfReader looper aok=${allOK} idx=${idx} j=${j}`);
//        }
//        console.log (`mkOfReader looper DONE aok=${allOK} idx=${idx}`);
        if (allOK) {
            console.log (`mkOfReader onload calling refresh idx=${idx}`);
            refreshModulesList() // do after all files are in
        } else {
            console.log (`mkOfReader onload NOT calling refresh idx${idx}`);
        }
    }
    return fr;
}

//-------------------------------------------------------------------------------
// Display list of modules
//-------------------------------------------------------------------------------

// Produce a formatted list of all open modules and display in Modules page

function refreshModulesList() {
    console.log ('refreshModulesList');
    let xs, ys = "\n<hr>";
    let m, sel, spanClass, mName, fName, mfName;
    for (let i=0; i<s16modules.length; i++) {
	m = s16modules[i];
        mfName = getModFileName (m);
	sel = selectedModule===i;
	spanClass = sel ? " class='SELECTEDFILE'" : " class='UNSELECTEDFILE'";
	ys += `&nbsp;`
	    +`<span${spanClass}>${i}${(sel ? '* ' : '  ')}. ${mfName}</span>`
	    + `<button onclick="modulesButtonSelect(${i})">Select</button>`
	    + `<button onclick="modulesButtonClose(${i})">Close</button>`
	    + `<button onclick="modulesButtonRefresh(${i})">Refresh</button>`
            + `<br>nAsmErrors=${m.nAsmErrors} isExecutable=${m.isExecutable}`
	    + '<br><pre>'
            + (m.fileStale ? "<br>Modified, needs to be saved<br>" : "<br>")
	    + m.modSrc.split('\n').slice(0,8).join('\n')
	    + '</pre>\n\n'
            + '<hr>\n\n';
    }
//    console.log (spanClass);
//    console.log (ys);
    let elt = document.getElementById('FilesBody');
    elt.innerHTML = ys;
}

function modulesButtonSelect (i) {
    console.log (`modulesButtonSelect ${i}`);
    let m = s16modules[i];
    selectedModule = i;
    m.selected = true;
    editorBufferTextArea.value = m.modSrc;
    document.getElementById("AsmTextHtml").innerHTML = "";
    refreshModulesList();
}
    //    console.log (`modulesButtonSelect ${m.mIndex}`);
    //    s16modules[selectedModule].selected = false;


// Need to be careful about this affecting refresh, as i has been
// baked into the file reader.  Should be fixed now...
function modulesButtonClose (i) {
    console.log (`filesButtonClose ${i}`);
    s16modules.splice(i,1);
    selectedModule = 0;
    refreshModulesList();
}

// need to set event handler to refresh the modules list
function modulesButtonRefresh (i) {
    let m = s16modules[i];
    console.log (`filesButtonRefresh ${m.mIndex}`);
    m.fileReader.readAsText(m.mFile);
    refreshModulesList();
}
//    let m = s16modules[selectedModule];
//    s16modules[i].fileReader.readAsText(s16modules[i].mFile);

