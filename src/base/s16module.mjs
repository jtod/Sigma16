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
// module.mjs defines the representation of source and object modules for the gui
//-------------------------------------------------------------------------------

// import * as fs from "fs";
import * as com from './common.mjs';
import * as st from './state.mjs';

// Interface
//   Modules: Choose Files button -- "change" event calls handleSelectedFiles
//   Modules: Refresh button -- refreshModulesList

//------------------------------------------------------------------------------
// New approach
//------------------------------------------------------------------------------

/*
Nov 15 2020, new approach for handling modules and files

Previously, there is a list of files which are mostly .asm files.

However, it would be good to be able to read obj and md files, and lnk
files, as well as asm files.  This is needed for the gui interface to
the linker.  But this has a drawback: the connection between
foo.asm.txt and foo.obj.txt would be lost.  Furthermore, it will be
easier to click the check box to select all the eligible files in a
directory, rather than clicking each file individually, in the file
chooser.  This could bring in various .md .obj files as well as the
.asm file.

New idea: the state is a map from basename to module group, which is a
class containing an asm, an obj, a lnk.  Each of these may be null; if
not null, it is an object created by new.

What about the editor?  The user may make a new source file in the
editor, and there could be several of these.

These will have module group names *Editor1 *Editor2, etc.  Then, when
assembled, we would get *Editor.obj.txt, etc.  There are two feasible
ways for the user to give a better name: either a module statement, or
possibly a gui command (though that hardly seems worth doing).

There should be a display of each module that describes its status
  - does it have an assembly source module?
  - does it have an object module?
  - is it a link file?  This would be a main program

When files are read
  - get the basename and the file type (asm, obj, etc)
  - create an asmModule, objInfo, etc and put in the file text
  - from the system state map, look for a mapping for the basename, and
    create one if it doesn't exist
    - then put the newly created submodule (asm, obj or whatever) into
      the module group container

*/


//-------------------------------------------------------------------------------
// File record
//-------------------------------------------------------------------------------

export class FileRecord {
    constructor (f, baseName, stage, mod) { // f is file handle provided by html
        this.fileHandle = f;
        this.baseName = baseName; // base part of filename
        this.stage = stage; // asm, obj, lnk, exe
        this.mod = mod; // module corresponding to baseName
        this.fileName = f.name; // full filename
        this.type = f.type;
        this.size = f.size;
        this.moddate = f.lastModifiedDate;
        this.text = "";
        this.fileReader = mkFileReader (this);
        this.fileReadComplete = false;
        this.fileReader.readAsText (f);
    }
}

//-------------------------------------------------------------------------------
// Module pane buttons
//-------------------------------------------------------------------------------

// Handle the New button on Modules pane by creating a new anonymous
// module

export function newMod () {
    const x = st.env.mkSelectModule ();
    console.log (`newMod ${x}`)
    refreshModulesList();
}

// Handle a "Select" button click in the list of modules on the
// Modules page: select the module with basename bn.

function handleSelect (bn) {
    console.log (`handleSelect ${bn}`);
    let mod = st.env.mkSelectModule (bn);
    refreshModulesList ();
}

// Handle a "Close" button click in the list of modules on the Modules
// page: close the module with basename bn.

function handleClose (bn) {
    console.log (`handleClose ${bn}`);
    st.env.closeModule (bn);
    refreshModulesList ();
}

// Temporary testing code

export function test() {
    console.log ("smod.test");
    refreshModulesList ();
    console.log (`modules = ${st.moduleEnvironment.keys()}`);
    for (const k of st.moduleEnvironment.keys()) {
        console.log (`key = ${k}`);
    }
}

//------------------------------------------------------------------------------
// Set of modules
//------------------------------------------------------------------------------


//------------------------------------------------------------------------------
// Representation of a module
//------------------------------------------------------------------------------

// A module may come from either a file or the editor pane, and may contain
//   - AsmInfo: assembly language source and information collected by assembler
//   - ObjInfo: object language source and information collected by linker
//   - LinkInfo: commands for the linker

// Each module has a moduleType indicating whether it originates from
// the assembler, from reading in an object module, etc.  The type is
// determined by the user's choice of command or from the filename (if
// any) but not by parsing the contents.  For example, if the user
// Assembles a file, then it is deemed to be assembly text.


/*
export const AsmModule = Symbol ("");  // assembly source, may have object
export const ObjModule = Symbol ("");  // object only
export const LinkModule = Symbol ("");  // commands for linking

*/

//-------------------------------------------------------------------------------
// Initialize modules
//-------------------------------------------------------------------------------

// Create and select an initial module

export function initModules () {
    com.mode.trace = true;
    com.mode.devlog ("initModules");

    st.env.clearModules ();
    let foom1 = st.env.mkSelectModule ("foo");
    let foom2 = st.env.mkSelectModule ("bar");
    let foom3 = st.env.mkSelectModule ("baz");

    let m1 = st.env.getSelectedModule();
    console.log (`m1 selected module ${m1.baseName}`);
    let m2 = st.env.mkSelectModule ("bar");
    console.log (`m2 is ${m2.baseName}`);
    
    
//    s16modules = new Map (); // throw away any existing map, create new one
//    let m = new st.S16Module ();
//    com.mode.devlog ("initModules about to show map...");
//    showModules ();
//    com.mode.devlog ("initModules has just shown map...");
    //    let m = new s16module (AsmModule);
    refreshEditorBuffer();
    refreshModulesList();
}


//-------------------------------------------------------------------------------
// Reading files in the Sigma16 directories
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
    //    s16modules.push(mkSelectModule());
    //    selectedModule = new s16module(); // create module; its symbol is selected
//    let m = s16modules[selectedModule];
//    m.asmInfo.modSrc = ys;   // deprecated ????????
    let m = new st.S16Module ();
    m.modText = ys;
    refreshEditorBuffer();
    refreshModulesList();
}

//-------------------------------------------------------------------------------
// Reading files selected by user
//-------------------------------------------------------------------------------

// When the user is in the Modules page and clicks Choose Files, a
// multiple file chooser widget is displayed, and its onchange event
// signals handleSelectedFiles with a list of file objects that were
// selected.  The function reads the files and creates entries in the
// modules list for them.

// Initialize the Modules page by setting up an event handler for the
// FileInput element.

export function prepareChooseFiles () {
    com.mode.devlog ("prepareChooseFiles");
    let elt = document.getElementById('FileInput');
    elt.addEventListener('change', event => {
        com.mode.devlog ("prepareChooseFiles change listener invoked");
        handleSelectedFiles(elt.files);
    });
}

// Parse string xs and check that it's in the form basename.ftype.txt
// where basename is any string not containing "." and ftype is one of
// asm, obj, md, lst, lnk.

export function checkFileName (xs) {
    const components = xs.split(".");
    let errors = [];
    let basename = "";
    let filetype = "";
    if (components.length != 3 ) {
        errors = [`Filename ${xs} must have three components, e.g.`
                  + `ProgramName.asm.txt`];
    } else if (components[2] != "txt") {
        errors = [`Last component of flename ${xs} is ${components[2]}`
                  + ` but it must be ".txt"`];
    } else if (!["asm", "obj", "lst", "md", "lnk"].includes(components[1])) {
        errors = [`Second component of flename ${xs} is ${components[1]}`
                  + ` but it must be one of asm,obj,md,lnk`];
    } else {
        basename = components[0];
        filetype = components[1];
    }
    let result = {errors, basename, filetype};
    com.mode.devlog (`checkFileName ${xs}\n errors=${result.errors}\n `
                     + `basename=${result.basename}\n filetype=${result.filetype}`);
    return result;
}

// When the user clicks Choose files, the browser produces a FileList
// object.  This function traverses that list and creates a module for
// each file

let newFiles = []; // should move to st.env

// When the Choose Files button is clicked, a file chooser dialogue
// box appears.  If the user selects one or more files and clicks
// open, handleSelectedFiles is called with a list of file handles for
// the files chosen by the user.  This function obtains and records
// metadata for each file, creates a fileReader object, and initiates
// the file read.

export function handleSelectedFiles (flist) {
    com.mode.trace = true;
    com.mode.devlog (`handleSelectedFiles: ${flist.length} files`);
    newFiles = [];
    for (let f of flist) {
        com.mode.devlog (`>>> File fname=${f.name} ftype=${f.type} `
                     + `fsize=${f.size} lastModified=${f.lastModifiedDate}`);
        const {errors, baseName, stage} = checkFileName (f.name);
        console.log (`  er=${errors} bn=${baseName} stage=${stage}`);
        const mod = st.env.mkSelectModule (baseName);
        const fileRecord = new FileRecord (f, baseName, stage, mod);
        newFiles.push (fileRecord);
    }
}

function mkFileReader (fileRecord) {
    com.mode.trace = true;
    const fr = new FileReader();
    com.mode.trace = true;
    fr.onload = function (e) {
	com.mode.devlog (`File reader ${fileRecord.fileName} onload`);
        fileRecord.text = e.target.result;
        fileRecord.fileReadComplete = true;
    }
    fr.onerror = function (e) {
        com.mode.devlog (`Error: could not read file ${fileRecord.fileName}`
                     + ` (error code = ${e.target.error.code})`);
        fileRecord.fileReadComplete = true;
    }
    return fr;
}

// If all the files selected in the file chooser have been read in,
// refresh the modules list.  The refresh should be called just one
// time.

function refreshWhenReadsFinished  (m) {
    com.mode.devlog (`refreshWhenReadsFinished`);
    let allOK = true;
    for (let x of newFiles) {
        com.mode.devlog (`RWRF allOK=${allOK} x.frc=${x.fileReadComplete}`);
            allOK = allOK && x.fileReadComplete;
        }
        com.mode.devlog (`check finished loop DONE aok=${allOK}`);
        if (allOK) {
            com.mode.devlog (`check finished calling refresh`);
            refreshModulesList(); // do after all files are in
        } else {
            com.mode.devlog (`mkOfReader onload NOT calling refresh`);
        }
    com.mode.devlog (`checkAllReadsFinished returning`);
}

//-------------------------------------------------------------------------------
// Display list of modules
//-------------------------------------------------------------------------------

// Produce a formatted list of all open modules and display in Modules page

export function refreshModulesList() {
    let xs = "";
    for (const bn of st.env.modules.keys ()) {
        xs += st.env.modules.get(bn).showHtml();
    }
    document.getElementById('FilesBody').innerHTML = xs;
    for (const bn of st.env.modules.keys ()) {
        const m = st.env.modules.get (bn);
        const selElt = document.getElementById(m.selectId);
        selElt.addEventListener ("click", event => { handleSelect (bn) });
        const closeElt = document.getElementById(m.closeId);
        closeElt.addEventListener ("click", event => { handleClose (bn) });
    }
}

//-------------------------------------------------------------------------------
// Update editor buffer
//-------------------------------------------------------------------------------
    
// Copy text of the selected module to the editor buffer

function refreshEditorBuffer () {
    com.mode.devlog (`refreshEditorBuffer`);
    const mod = st.env.getSelectedModule ();
    const xs = mod.asmInfo ? mod.asmInfo.text : "";
    document.getElementById("EditorTextArea").value = xs;
}

//--------------------------------------------------
// Deprecated

//    com.mode.devlog (`Creating file reader ${m.file.name}`);
//        refreshWhenReadsFinished (m); // if all files are in, then refresh
//	com.mode.devlog (`File reader ${m.file.name} onload event finished`);
//        com.mode.devlog (m.text);
//        m.text = "";
//        m.fileReadComplete = true;

//        m.fileReadComplete = false;
// Check to see if ftype is text/plain ?
// com.mode.devlog (`handleSelectedFiles fname=${f.name} size=${f.size} bytes`);
//        let type = AsmModule;  // should calculate based on filename ???????????
//        let m = new s16module (type);
//        newFiles.push (m);
//        m.file = f;
//	m.fileReader = mkReader (m);
//        m.fileReadComplete = false;
//	m.fileReader.readAsText (f);

/*
// Each module is identified by a Symbol, and the set of modules is
// represented as a Map indexed by the module symbols.  The
// selectedModule is the symbol of the current module.  When a new
// module is created it is selected.

export let s16modules = null; // initModules gives it initial value
export let moduleGenSym = 0;  // give unique descriptor to each symbol
export let selectedModule; // symbol for the object representing selected module
*/

// selectedModule contains the symbol for the map entry in s16modules
// for the currently selected module.  There should always be at least
// one module in s16modules (after initialization) and selectedModule
// should always be defined as the symbol of one of these.

// Return the module correspondong to the symbol stored in selectedModule

/*
export function showModules () {
    com.mode.devlog (`Modules: selected = ${typeof selectedModule} ${selectedModule.description}`);
    s16modules.forEach ( (val,key,map) => {
        com.mode.devlog (`>>> sym=${val.sym.description} tl=${val.text.length}`);
    });
}
*/

// Find the symbol for the selectedModule and return the actual module object
/*
export function getSelectedModule () {
    com.mode.devlog ("getSelectedModule");
    showModules ();
    com.mode.devlog ("getSelectedModule about to get...");
    let m = s16modules.get (selectedModule);
//    com.mode.devlog (`getSelectedModule ${m.modName} ${m.text}`);
    return m;
}
*/

/*     oLD refreshModulesList
    com.mode.devlog ('refreshModulesList');
    let xs, ys = "\n<hr>";
    ys += `<br>${s16modules.size} modules<br><hr><br>`;
    let sel, spanClass, mName, fName, mfName;
    for (let x of s16modules.values()) {
	let spanClass = x.sym===selectedModule
            ? " class='SELECTEDFILE'"
            : " class='UNSELECTEDFILE'";
        let fileInfo = x.file ? `${x.file.name}, ${x.file.size} bytes.`
            : "<no file>";
	xs = `&nbsp;`
            + `<span ${spanClass}>`
//            + `${fileInfo}  ${showModType(x.type)}`
            + "</span>"
            + ` ${x.selectButton} ${x.closeButton}`
            + '<br><pre>'
            + x.text.split('\n').slice(0,8).join('\n')
	    + '</pre>\n\n'
            + '<hr>\n';
        ys += xs;
    }
    let elt = document.getElementById('FilesBody');
    elt.innerHTML = ys;
    for (let x of s16modules.values()) {
        document.getElementById(x.selectId)
            .addEventListener("click",
                              event => {com.mode.devlog(`${x.selectId}`);
                                        modSelect(x);
                                        });
        document.getElementById(x.closeId)
            .addEventListener("click",
                              event => {com.mode.devlog(`${x.closeId}`);
                                        modClose(x);
                                        });
    }
    refreshEditorBuffer ();
    com.mode.devlog ("refreshModulesList returning");
*/

//    document.getElementById("EditorTextArea").value = getSelectedModule().text;

/*
// Use unique number for prefix name for each button
let buttonPrefixNumber = 0;
*/

