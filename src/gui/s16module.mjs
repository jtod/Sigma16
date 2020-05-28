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

import * as fs from "fs";
import * as com from './common.mjs';

// Function invocation by gui controls in Module pane:
//   Choose Files button starts file chooser, its event calls 
//   MP_Refresh: refreshModulesList)

//------------------------------------------------------------------------------
// Representation of a module
//------------------------------------------------------------------------------


// A module may contain
//   - AsmInfo: assembly language source and information collected by assembler
//   - ObjInfo: object language source and information collected by linker
//   - LinkInfo: commands for the linker

// The text of a module may come from
//   - a file
//   - the editor pane

//------------------------------------------------------------------------------
// Set of modules
//------------------------------------------------------------------------------

// Each module is identified by a Symbol, and the set of modules is
// represented as a Map indexed by the module symbols.  The
// selectedModule is the symbol of the current module.  When a new
// module is created it is selected.

export let s16modules = new Map ();
export let selectedModule;

// Create and select an initial module

export function initModules () {
    com.mode.devlog ("initModules");
    let m = new s16module (AsmModule);
    refreshEditorBuffer();
    refreshModulesList();
}

// Get the full data structure for the current module

export function getCurrentModule () {
    return s16modules.get (selectedModule);
}

//------------------------------------------------------------------------------
// Representation of a module
//------------------------------------------------------------------------------

// Each module has a moduleType indicating whether it originates from
// the assembler, from reading in an object module, etc.  The type is
// determined by the user's choice of command or from the filename (if
// any) but not by parsing the contents.  For example, if the user
// Assembles a file, then it is deemed to be assembly text.

export const AsmModule = Symbol ("");  // assembly source, may have object
export const ObjModule = Symbol ("");  // object only
export const LnkModule = Symbol ("");  // commands for linking

function showModType (t) {
    return t === ModAsm ? "Assembly language"
        : t === ModObj ? "Object code"
        : t === ModLink ? "Linker commands"
        : "module type is unknown"
}

export class s16module {
    constructor (modType) {
        const s = Symbol ("");         // the key to this module in s16modules
        s16modules.set (s, this);   // record this module in the set
        selectedModule = s;         // select the module as it's created
        this.sym = s;                  // make the key available, given module
        this.type = modType;           // implies what to do with the text
        this.text = "";                // raw source text
        this.file = null;
        this.fileReader = null;
        this.fileReadComplete = false;
        this.asmInfo = null;           // to be filled in by assembler
        this.objInfo = null;           // to be filled in by linker
        return s;
    }
    show () {
        return "module"
                + showModType (this.type)
                + this.text
    }
    refresh () {
        
    }
}


//------------------------------------------------------------------------------
// Files
//------------------------------------------------------------------------------

export class fileInfo {
    constructor (fileName, fileObject) {
        this.file = null;              // handle for reading the file
        this.fileName = null;          // file name (null if from New button)
        this.fileReader = null;        // fs file reader ojbect
        this.fileReadComplete = false;
    }
}


//-----------------------------------------------------------------------------
// Assembly language state: m.asmInfo
//-----------------------------------------------------------------------------

/* do this soon
export class AsmInfo {
    constructor (xs) {
        this.asmText = xs;
        this.modName = null;
        this.asmSrcLines = [];
        this.symbols = [];
        this.symbolTable = new Map ();
        this.locationCounter = 0;
        this.asmListingPlain = [];
        this.asmListingDec = [];
        this.objectCode = [];
        this.metadata = [];
        this.asArrMap = [];
        this.exports = [];
        this.nAsmErrors = [];
    }
}
*/

export function mkModuleAsm () {
    com.mode.devlog("mkModuleAsm");
    return {
	modName : "anon",       // name of module specified in module stmt
        asmSrcLines : [],
	asmStmt : [],               // statements correspond to lines of source
	symbols : [],              // symbols used in the source
	symbolTable : new Map (),  // symbol table
	locationCounter : 0,       // address where next code will be placed
	asmListingPlain : [],      // assembler listing
	asmListingDec : [],        // decorated assembler listing
	objectCode : [],           // string hex representation of object
        metadata : [],             // lines of metadata code
        asArrMap : [],             // address-sourceline map
        exports : [],              // list of exported identifiers
        modAsmOK : false, // deprecated, use nAsmErrors===0
	nAsmErrors : 0             // number of errors in assembly source code
    }
}

//-------------------------------------------------------------------------------
// Object code state: m.objInfo
//-------------------------------------------------------------------------------

export class ObjectModule {
    constructor(objLines, mdLines) {
        this.objectLines = objLines;
        this.metadataLines = mdLines;
        this.name = "anonymous";
        this.imports = [];
        this.exports = [];
        this.origin = 0;
        this.size = 0;
        this.last = 0;
    }
}



//-------------------------------------------------------------------------------
// Initialize modules
//-------------------------------------------------------------------------------


// Make new module, copy example text into it, and select it

export function selectExample() {
    com.mode.devlog ('selectExample');
    let exElt = document.getElementById('ExamplesIframeId');
    let xs = exElt.contentWindow.document.body.innerHTML;
    com.mode.devlog (`xs = ${xs}`);
    let skipPreOpen = xs.replace(com.openingPreTag,"");
    let skipPreClose = skipPreOpen.replace(com.closingPreTag,"");
    com.mode.devlog (`skipPreOpen = ${skipPreOpen}`);
    let ys = skipPreClose;
    s16modules.push(mkModule());
    selectedModule = s16modules.length-1;
    let m = s16modules[selectedModule];
    m.modText = ys;
    m.asmInfo.modSrc = ys;   // deprecated ????????
    refreshEditorBuffer();
    refreshModulesList();
}

//-------------------------------------------------------------------------------
// File operations on existing modules
//-------------------------------------------------------------------------------

// When the user is in the Modules page and clicks Choose Files, a
// multiple file chooser widget is displayed, and its onchange event
// signals handleSelectedFiles with a list of file objects that were
// selected.  The function reads the files and creates entries in the
// modules list for them.

// Set handler for s16modules/ open file operation
// html contained:  onchange="handleSelectedFiles(this.files);"

export function prepareChooseFiles () {
    com.mode.devlog ("prepareChooseFiles");
    let elt = document.getElementById('FileInput');
    elt.addEventListener('change', event => {
        com.mode.devlog ("prepareChooseFiles change listener invoked");
        handleSelectedFiles(elt.files);
    });
}

// When the user clicks Choose files, the browser produces a FileList
// object.  This function traverses that list and creates a module for
// each file

export function handleSelectedFiles (flist) {
    for (f of flist) {
        console.log (`handleSelectedFiles fname=${f.fname}`);
        let type = AsmModule;  // should calculate based on filename ???????????
        let m = new s16module (type);
        m.file = f;
	m.fileReader = mkReader (m);
        m.fileReadComplete = false;
	m.fileReader.readAsText ();
    }
}

function mkReader (m) {
    let fr = new FileReader();
    com.mode.devlog (`Creating file reader ${m.file.fname}`);
    fr.onload = function (e) {
	com.mode.devlog (`File reader ${m.file.fname} onload event`);
        m.text = e.target.result;
        console.log (m.text);
        m.fileReadComplete = true;
        refreshWhenReadsFinished (m); // if all files are in, then refresh
    }
    fr.onerror = function (e) {
        console.log (`Error: could not read file ${m.file.fname}`
                     + ` (error code = ${e.target.error.code})`);
        m.text = "";
        m.fileReadComplete = true;
    }
    return fr;
}

// If all the files selected in the file chooser have been read in,
// refresh the modules list.  The refresh should be called just one
// time.

function refreshWhenReadsFinished  (m,i) {
    com.mode.devlog (`checkAllReadsFinished ${i}`);
    let allOK = true;
    let a = m.firstNewMod;
    let b = m.lasttNewMod;
    for (let j=a; j<=b; j++) {
        com.mode.devlog (`refreshWhen i=${i} j=${j}`);
            allOK = allOK && s16modules[j].fileReadComplete;
            com.mode.devlog (`check finished loop aok=${allOK} i=${i} j=${j}`);
        }
        com.mode.devlog (`check finished loop DONE aok=${allOK} i=${i}`);
        if (allOK) {
            com.mode.devlog (`check finished calling refresh i=${i}`);
            //            selectedModule = i;
            selectedModule = m.selectWhenReadsFinished;
            refreshEditorBuffer();
            refreshModulesList() // do after all files are in
        } else {
            com.mode.devlog (`mkOfReader onload NOT calling refresh i${i}`);
        }
    com.mode.devlog (`checkAllReadsFinished returning ${i}`);
}

//-------------------------------------------------------------------------------
// Display list of modules
//-------------------------------------------------------------------------------

// Use unique number for prefix name for each button
let buttonPrefixNumber = 0;

// Produce a formatted list of all open modules and display in Modules page

export function refreshModulesList() {
    com.mode.devlog ('refreshModulesList');
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
        com.mode.devlog (`buttonIdSuffix=${buttonIdSuffix}`);
        selButton = `select${buttonIdSuffix}`;
        com.mode.devlog (`selButton=${selButton}`);
        let selButtonElt = "<button id='" + selButton +"'>Select</button>";
        com.mode.devlog (`selButtonElt = ${selButtonElt}`);
        com.mode.devlog (`refreshModulesList buttonIdPrefix=${buttonIdSuffix}`);
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
        //	    + ma.modSrc.split('\n').slice(0,8).join('\n') // deprecated
            + m.modText.split('\n').slice(0,8).join('\n')
	    + '</pre>\n\n'
            + '<hr>\n';

    };
    // Need to add the buttons to the document object ???????
    // Then use something like the following to catch the button clicks
//    document.getElementById(selButton)
//        .addEventListener('click', event => {
//            com.mode.devlog (`clicked (event listener): ${selButton}`);
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
// update the editor buffer

function refreshEditorBuffer () {
    com.mode.devlog (`refreshEditorBuffer selectedModule=${selectedModule}`);
    let m = s16modules.get (selectedModule);
    let ma = m.asmInfo;
    document.getElementById("EditorTextArea").value = m.modText; // deprecated
}

function modulesButtonSelect (i) {
    com.mode.devlog (`modulesButtonSelect ${i}`);
    let m = s16modules[i];
    selectedModule = i;
    m.selected = true;
    refreshEditorBuffer();
    refreshModulesList();
}

function modulesButtonClose (i) {
    com.mode.devlog (`filesButtonClose ${i}`);
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
    com.mode.devlog (`modulesButtonRefresh i=${i}`);
    let m = s16modules[i];
    let ma = m.asmInfo;
    selectedModule = i;
    m.firstNewMod = i;
    m.lastNewMod = i;
    m.selectWhenReadsFinished = i;
//    ma.modSrc = "*** reading file ***";  // deprecated
    com.mode.devlog (`filesButtonRefresh ${m.mIndex}`);
    m.fileReader.readAsText(m.mFile);
}


// Deprecated

/* old deprecated
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
*/

//-------------------------------------------------------------------------------
// Set of modules - old version
//-------------------------------------------------------------------------------

/*
let s16modules = [];    // All the modules in the system
let selectedModule = 0;

function setSelectedModule (i) {
    selectedModule = i;
    com.mode.devlog (`Setting selected module to ${i}`);
}


// Make a new module with empty contents; this defines the fields of a module
// old version
export function mkModule (i) {
    com.mode.devlog('mkModule');
    return {
        modText : "",
//        modType : ModText,
//        mIndex : i,             // index in s16modules list
	mFile : null,              // file object associated with module, if any
        fileName : null,           // filename, if exists and is known
	fileReader : null,         // object to read the file
        firstNewMod : 0,
        lastNewMod : 0,
        selectWhenReadsFinished : 0,
        fileReadComplete : false,          // has file been completely read in yet?
        fileStale : false,        // contents have been changed in editor
	selected : false,         // this module is selected (for edit, asm)
//        objIsAssembled : false,   // module has been assembled
//        modObjLoaded : false,     // module loaded directly from obj
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
function oldgetModFileName (m) {
    if (m) {
        let mname = m.asmInfo.modName ? `${m.asmInfo.modName}` : '';
        let fname = m.mFile ? `(${m.mFile.name})` : '';
        let xs = (m.asmInfo.modName || m.mFile) ? mname + '  ' + fname : '<anonymous>';
        return xs;
    } else return "?"
}

function oldgetModName (m) {
//    return m.modName ? m.modName : "no module statement"
    if (m) {
        let mname = m.asmInfo.modName ? `${m.asmInfo.modName}` : '';
        return mname;
    } else return "module name is unknown"
}

function oldgetFileName (m) {
    return m.mFile ? m.mFile.name : "no file associated with module"
}

export function oldnewModule () {
    com.mode.devlog ("newModule");
    selectedModule = s16modules.length;
    s16modules.push(mkModule(selectedModule));
    refreshEditorBuffer();
    refreshModulesList();
}


*/

/* old version
export function handleSelectedFiles (flist) {
    com.mode.devlog("handleSelectedFiles");
    com.mode.devlog(`handleSelectedFiles flist=${flist}`);
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
	m.modText = "";
	m.asmInfo.modSrc = ""; // deprecated
	s16modules.push(m);
    }
    com.mode.devlog ("handle, finished selecting");
}
*/

/* old version
export function initModules () {
    com.mode.devlog ("initModules");
    s16modules = [mkModule()];
    let m = s16modules[0];
    m.mIndex = 0;
    selectedModule = 0;
    refreshEditorBuffer();
    refreshModulesList();
}
*/

//    const s = Symbol ("init module");
//    s16modules.set (s, m);
//    selectedModule = s;

    //    return s16modules[selectedModule]

/*
Strategy for list of modules

// A Sigma16 program may contain several modules, some or all of which
// may correspond to files.


The Modules pane contains a list of modules; each of those is created
by new S16Module().  When the list is refreshed, it shows the type of
the module (asm, obj, link), current metadata about the module
(e.g. for an asm module, whether object code exists and the number of
assembly errors), and buttons to select, refresh, and delete the
module.

To delete the module, the object needs a reference to it which can
also be used to find the module within the module list.  This should
not be an index into an array of modules, because deleting a module
could cause indices to change.

When a module is created, a new symbol to identify it is generated.
The list of modules is a Map, indexed by the symbol.  The symbol is
kept in a field within the module, allowing the buttons to refer to
the module's state.

*/
