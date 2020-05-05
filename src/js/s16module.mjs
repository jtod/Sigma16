// Sigma16: module.mjs
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

//-------------------------------------------------------------------------------
// module.mjs defines the representation of source and object modules
//-------------------------------------------------------------------------------

// The working program is a list of modules, one of which is the
// current working module visible in the editor and assembler.  A
// module data structure contains everything known about the module,
// whether it is source or object.

import * as com from './common.mjs';

//-------------------------------------------------------------------------------
// Set of modules
//-------------------------------------------------------------------------------

let s16modules = [];    // All the modules in the system
let selectedModule = 0;

function setSelectedModule (i) {
    selectedModule = i;
    console.log (`Setting selected module to ${i}`);
}


// let currentFile = null;  // deprecated, Remember the current working file handle

//-----------------------------------------------------------------------------
// Assembler state:  information about a module produced by the assembler
//-----------------------------------------------------------------------------

export function mkModuleAsm () {
    console.log("mkModuleAsm");
    return {
	modName : null,       // name of module specified in module stmt
	modSrc : '',               // source code
        modAsmOK : false,
	symbols : [],              // symbols used in the source
	symbolTable : new Map (),  // symbol table
	nAsmErrors : 0,            // number of errors in assembly source code
	locationCounter : 0,       // address where next code will be placed
	asmListingPlain : [],      // assembler listing
	asmListingDec : [],        // decorated assembler listing
	objectCode : [],           // string hex representation of object
        metadata : [],             // lines of metadata code
        asMap : [],                // address/source map
        exports : [],              // list of exported identifiers
	asmStmt : []               // statements correspond to lines of source
    }
}

//-------------------------------------------------------------------------------
// Initialize modules
//-------------------------------------------------------------------------------

// Create one initial (empty) module and make it the current module
export function initModules () {
    console.log ("initModules");
    s16modules = [mkModule()];
    let m = s16modules[0];
    m.mIndex = 0;
    selectedModule = 0;
    refreshEditorBuffer();
    refreshModulesList();
}

// Get the full data structure for the current module
export function getCurrentModule () {
    return s16modules[selectedModule]
}

//------------------------------------------------------------------------------
// Representation of a module
//------------------------------------------------------------------------------

// Each module has a moduleType indicating whether it originates from
// the assembler, from reading in an object module, etc.  The type is
// determined by the user's choice of command, not from the filename
// (if any) or the contents.  For example, if the user Assembles a
// file, then it is deemed to be assembly text.

export const ModText = Symbol ("ModText");  // generic text with unknown role
export const ModAsm  = Symbol ("ModAsm");   // obtained assembly langauge from editor
export const ModObj  = Symbol ("ModObj");   // obtained object from editor/file
export const ModLink = Symbol ("ModLink");  // module text is set of linker commands

function showModType (t) {
    return t===ModText ? "Text"
        : t===ModAsm ? "Assembly language"
        : t===ModObj ? "Object code"
        : t===ModLink ? "Linker commands"
        : "module type is unknown"
}

// Make a new module with empty contents; this defines the fields of a module

function mkModule (i) {
    console.log('mkModule');
    return {
        modType : ModText,
        mIndex : i,             // index in s16modules list
	mFile : null,              // file object associated with module, if any
        fileName : null,           // filename, if exists and is known
	fileReader : null,         // object to read the file
        firstNewMod : 0,
        lastNewMod : 0,
        selectWhenReadsFinished : 0,
        fileReadComplete : false,          // has file been completely read in yet?
        fileStale : false,        // contents have been changed in editor
	selected : false,         // this module is selected (for edit, asm)
        objIsAssembled : false,   // module has been assembled
        modObjLoaded : false,     // module loaded directly from obj
        asmInfo : mkModuleAsm(),  // initialize assembler state
        objInfo : null,
        //        objInfo : mkModuleObj(),  // initialize object state
        //        objInfo : new ModuleObj(),  // initialize object state
        objIsExecutable : false    // can be executed without further linking
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
        let mname = m.asmInfo.modName ? `${m.asmInfo.modName}` : '';
        let fname = m.mFile ? `(${m.mFile.name})` : '';
        let xs = (m.asmInfo.modName || m.mFile) ? mname + '  ' + fname : '<anonymous>';
        return xs;
    } else return "?"
}

function getModName (m) {
//    return m.modName ? m.modName : "no module statement"
    if (m) {
        let mname = m.asmInfo.modName ? `${m.asmInfo.modName}` : '';
        return mname;
    } else return "module name is unknown"
}

function getFileName (m) {
    return m.mFile ? m.mFile.name : "no file associated with module"
}

export function newModule () {
    console.log ("newModule");
    selectedModule = s16modules.length;
    s16modules.push(mkModule(selectedModule));
    refreshEditorBuffer();
    refreshModulesList();
}

// Make new module, copy example text into it, and select it

export function selectExample() {
    console.log ('selectExample');
    let exElt = document.getElementById('ExamplesIframeId');
    let xs = exElt.contentWindow.document.body.innerHTML;
    console.log (`xs = ${xs}`);
    let skipPreOpen = xs.replace(com.openingPreTag,"");
    let skipPreClose = skipPreOpen.replace(com.closingPreTag,"");
    console.log (`skipPreOpen = ${skipPreOpen}`);
    let ys = skipPreClose;
    s16modules.push(mkModule());
    selectedModule = s16modules.length-1;
    let m = s16modules[selectedModule];
    m.asmInfo.modSrc = ys;
    refreshEditorBuffer();
    refreshModulesList();
}

//-------------------------------------------------------------------------------
// Reading files
//-------------------------------------------------------------------------------

// When the user is in the Modules page and clicks Choose Files, a
// multiple file chooser widget is displayed, and its onchange event
// signals handleSelectedFiles with a list of file objects that were
// selected.  The function reads the files and creates entries in the
// modules list for them.

export function handleSelectedFiles (flist) {
    console.log("handleSelectedFiles");
    console.log(`handleSelectedFiles flist=${flist}`);
    let m;
    let idxFirstNewMod = s16modules.length;
    let idxLastNewMod  = idxFirstNewMod + flist.length - 1;
    for (let i=0; i<flist.length; i++) {
	m = mkModule (s16modules.length);
        m.mIndex = s16modules.length;
	m.mFile = flist[i];
        m.fileName = m.mFile.name;
	m.selected = false;
        m.fileReadComplete = false;
        m.firstNewMod = idxFirstNewMod;
        m.lastNewMod = idxLastNewMod;
        m.selectWhenReadsFinished = m.lastNewMod;
	m.fileReader = mkOfReader(m, m.mIndex);
	m.fileReader.readAsText(m.mFile);
	m.asmInfo.modSrc = "";
	s16modules.push(m);
    }
    console.log ("handle, finished selecting");
}

function mkOfReader (m, i) {
    let fr = new FileReader();
    fr.onload = function (e) {
        let idx = m.mIndex; // could change after close
        let a = m.firstNewMod;
        let b = m.lastNewMod;
	console.log (`ofReader ${idx} onload event a=${a} b=${b}`);
        m.asmInfo.modSrc = e.target.result;
        m.fileReadComplete = true;
        refreshWhenReadsFinished (m,i); // if all files are in, then refresh
    }
    return fr;
}

// Set handler for s16modules/ open file operation
// html contained:  onchange="handleSelectedFiles(this.files);"

export function prepareChooseFiles () {
    console.log ("prepareChooseFiles");
    let elt = document.getElementById('FileInput');
    elt.addEventListener('change', event => {
        console.log ("prepareChooseFiles change listener invoked");
        handleSelectedFiles(elt.files);
    });
}

// If all the files selected in the file chooser have been read in,
// refresh the modules list.  The refresh should be called just one
// time.

function refreshWhenReadsFinished  (m,i) {
    console.log (`checkAllReadsFinished ${i}`);
    let allOK = true;
    let a = m.firstNewMod;
    let b = m.lasttNewMod;
    for (let j=a; j<=b; j++) {
        console.log (`refreshWhen i=${i} j=${j}`);
            allOK = allOK && s16modules[j].fileReadComplete;
            console.log (`check finished loop aok=${allOK} i=${i} j=${j}`);
        }
        console.log (`check finished loop DONE aok=${allOK} i=${i}`);
        if (allOK) {
            console.log (`check finished calling refresh i=${i}`);
            //            selectedModule = i;
            selectedModule = m.selectWhenReadsFinished;
            refreshEditorBuffer();
            refreshModulesList() // do after all files are in
        } else {
            console.log (`mkOfReader onload NOT calling refresh i${i}`);
        }
    console.log (`checkAllReadsFinished returning ${i}`);
}

//-------------------------------------------------------------------------------
// Display list of modules
//-------------------------------------------------------------------------------

// Use unique number for prefix name for each button
let buttonPrefixNumber = 0;

// Produce a formatted list of all open modules and display in Modules page

export function refreshModulesList() {
    console.log ('refreshModulesList');
    let xs, ys = "\n<hr>";
    let m, sel, spanClass, mName, fName, mfName;
    let selButton;
    for (let i=0; i<s16modules.length; i++) {
	m = s16modules[i];
        let ma = m.asmInfo;
        mfName = getModFileName (m);
	sel = selectedModule===i;
	spanClass = sel ? " class='SELECTEDFILE'" : " class='UNSELECTEDFILE'";
        let temp1 = 2;
	ys += `&nbsp;`;
        let temp2 = 2;
        let buttonIdSuffix = `Mod${i}`; // will it work if close?
        console.log (`buttonIdSuffix=${buttonIdSuffix}`);
        selButton = `select${buttonIdSuffix}`;
        console.log (`selButton=${selButton}`);
        let selButtonElt = "<button id='" + selButton +"'>Select</button>";
        console.log (`selButtonElt = ${selButtonElt}`);
        console.log (`refreshModulesList buttonIdPrefix=${buttonIdSuffix}`);
	ys += `&nbsp;`
	    +`<span${spanClass}>${i}${(sel ? '* ' : '  ')}. ${mfName}</span>`
//	    + `<button onclick="modulesButtonSelect(${i})">Select</button>`
//	    + `<button id="modulesButtonSelect${buttonIdSuffix}`>Select</button>
            + selButtonElt
	    + `<button onclick="modulesButtonClose(${i})">Close</button>`
            + ( m.mFile
                ? `<button onclick="modulesButtonRefresh(${i})">Refresh</button>`
                : "")
            + `<br>${showModType(m.modType)}`
            + "  "
            + (m.objIsExecutable ? "executable" : "not executable")
	    + '<br><pre>'
            + (m.fileStale ? "<br>Modified, needs to be saved<br>" : "<br>")
	    + ma.modSrc.split('\n').slice(0,8).join('\n')
	    + '</pre>\n\n'
            + '<hr>\n';

    };
    // Need to add the buttons to the document object ???????
    // Then use something like the following to catch the button clicks
//    document.getElementById(selButton)
//        .addEventListener('click', event => {
//            console.log (`clicked (event listener): ${selButton}`);
//        });

    ys += `<br>${s16modules.length} modules\n`
    ys += `<br>module #${selectedModule} currently selected\n`;
    ys += '<br><hr><br>';
    let elt = document.getElementById('FilesBody');
    elt.innerHTML = ys;
}
//            +  `nAsmErrors=${ma.nAsmErrors} isExecutable=${m.isExecutable}`
//            + `<br>TEMP mIndex=${m.mIndex}`
//            + (m.mFile ? `<br>TEMP Associated with file ${m.fileName}`
//               : "<br>TEMP Has no file, only in editor buffer")


// If there has been a change to selected module or its file contents,
// update the editor buffer as well as the assembler buffer
function refreshEditorBuffer () {
    console.log (`refreshEditorBuffer selectedModule=${selectedModule}`);
    let m = s16modules[selectedModule];
    let ma = m.asmInfo;
    document.getElementById("EditorTextArea").value = ma.modSrc;
    document.getElementById("AsmTextHtml").innerHTML = "";
}

function modulesButtonSelect (i) {
    console.log (`modulesButtonSelect ${i}`);
    let m = s16modules[i];
    selectedModule = i;
    m.selected = true;
    refreshEditorBuffer();
    refreshModulesList();
}

function modulesButtonClose (i) {
    console.log (`filesButtonClose ${i}`);
    if (s16modules.length == 1) { // closing the only module there is
        initModules() // there is just one new module, and select it
    } else {
        s16modules.splice(i,1);
        for (let j=i; j<s16modules.length; j++) {
            s16modules[j].mIndex = j;
        }
        selectedModule = 0;
        refreshEditorBuffer();
        refreshModulesList();
    }
}

// need to set event handler to refresh the modules list
function modulesButtonRefresh (i) {
    console.log (`modulesButtonRefresh i=${i}`);
    let m = s16modules[i];
    let ma = m.asmInfo;
    selectedModule = i;
    m.firstNewMod = i;
    m.lastNewMod = i;
    m.selectWhenReadsFinished = i;
    ma.modSrc = "*** reading file ***";
    console.log (`filesButtonRefresh ${m.mIndex}`);
    m.fileReader.readAsText(m.mFile);
}

export {
    s16modules, selectedModule, setSelectedModule
}
