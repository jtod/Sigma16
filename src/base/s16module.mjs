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


// Select the module with basename bn
function handleSelect (bn) {
    console.log (`handleSelect ${bn}`);
}

// Close the module with basename bn
function handleClose (bn) {
    console.log (`handleClose ${bn}`);
}


//------------------------------------------------------------------------------
// Set of modules
//------------------------------------------------------------------------------

// Each module is identified by a Symbol, and the set of modules is
// represented as a Map indexed by the module symbols.  The
// selectedModule is the symbol of the current module.  When a new
// module is created it is selected.

export let s16modules = null; // initModules gives it initial value
export let moduleGenSym = 0;  // give unique descriptor to each symbol
export let selectedModule; // symbol for the object representing selected module

// selectedModule contains the symbol for the map entry in s16modules
// for the currently selected module.  There should always be at least
// one module in s16modules (after initialization) and selectedModule
// should always be defined as the symbol of one of these.

// Return the module correspondong to the symbol stored in selectedModule

export function showModules () {
    com.mode.devlog (`Modules: selected = ${typeof selectedModule} ${selectedModule.description}`);
    s16modules.forEach ( (val,key,map) => {
        com.mode.devlog (`>>> sym=${val.sym.description} tl=${val.text.length}`);
    });
}

// Find the symbol for the selectedModule and return the actual module object

export function getSelectedModule () {
    com.mode.devlog ("getSelectedModule");
    showModules ();
    com.mode.devlog ("getSelectedModule about to get...");
    let m = s16modules.get (selectedModule);
//    com.mode.devlog (`getSelectedModule ${m.modName} ${m.text}`);
    return m;
}

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

export const AsmModule = Symbol ("");  // assembly source, may have object
export const ObjModule = Symbol ("");  // object only
export const LinkModule = Symbol ("");  // commands for linking

export class s16module {
    constructor () {
        const s = Symbol (`module_#${moduleGenSym}`);   // the key to this module in s16modules
        moduleGenSym++;
        s16modules.set (s, this);   // record this module in the set
        selectedModule = s;         // select the module as it's created
        com.mode.devlog (`NEW S16MODULE SETTING selectedModule ${selectedModule.description}`);
        this.sym = s;                  // make the key available, given module
        this.text = "init mod text";                // raw source text
        this.modName = null;
        this.file = null;
        this.fileReader = null;
        this.fileReadComplete = true;
        this.asmInfo = null;           // to be filled in by assembler
        this.objInfo = null;           // to be filled in by linker
        this.selectId = `select_${this.idNumber}`;
        this.closeId = `close_${this.idNumber}`;
        this.selectButton = `<button id='${this.selectId}'>Select</button>`;
        this.closeButton = `<button id='${this.closeId}'>Close</button>`;
    }
    show () {
        return "module"
//                + showModType (this.type)
                + this.text
    }
}

function modSelect (m) {
    com.mode.devlog ("modSelect");
    selectedModule = m.sym;
    refreshModulesList ();
    refreshEditorBuffer ();
    com.mode.devlog ("modSelect returning");
}


function modClose (m) {
    com.mode.devlog ("close");
    let s = m.sym;
    let closedSelected = s === selectedModule;
    s16modules.delete (s);
    if (s16modules.size === 0) {
        com.mode.devlog ("closed last module, reinitializing");
        initModules ();
    }
    if (closedSelected) {
        //        selectedModule = [...s16modules.keys()][0];
        com.mode.devlog ("closedSelected... FIX THIS ??????????");
    }
    refreshModulesList ();
}

//-------------------------------------------------------------------------------
// Initialize modules
//-------------------------------------------------------------------------------

// Create and select an initial module

export function initModules () {
//    com.mode.trace = true;
    com.mode.devlog ("initModules");

    let foom1 = new S16Module ("foo");
    let foom2 = new S16Module ("bar");
    let foom3 = new S16Module ("baz");
    showS16Module();
    
    s16modules = new Map (); // throw away any existing map, create new one
    let m = new s16module ();
    com.mode.devlog ("initModules about to show map...");
    showModules ();
    com.mode.devlog ("initModules has just shown map...");
    refreshEditorBuffer();
    refreshModulesList();
}
    //    let m = new s16module (AsmModule);

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
    //    s16modules.push(mkModule());
    //    selectedModule = new s16module(); // create module; its symbol is selected
//    let m = s16modules[selectedModule];
//    m.asmInfo.modSrc = ys;   // deprecated ????????
    let m = new s16module ();
    m.modText = ys;
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

let newModules = [];

// Parse string xs and check that it's in the form basename.ftype.txt
// where basename is any string not containing . and ftype is one of
// asm, obj, md, lst, lnk

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

export function handleSelectedFiles (flist) {
    com.mode.devlog (`handleSelectedFiles filst size = ${flist.length}`);
    newModules = [];
    for (let f of flist) {
        // Get properties of the file
        const fname = f.name;
        const ftype = f.type;
        const fsize = f.size;
        const fmoddate = f.lastModifiedDate;
        console.log ("--- File ---");
        
        com.mode.devlog (`>>> File fname=${fname} ftype=${ftype} `
                     + `fsize=${fsize} lastModified=${fmoddate}`);
        const fnameComponents = checkFileName (fname);
        console.log (`Filename ${fnameComponents}`);

        // Check to see if ftype is text/plain ?
// com.mode.devlog (`handleSelectedFiles fname=${f.name} size=${f.size} bytes`);
        
        let type = AsmModule;  // should calculate based on filename ???????????
        let m = new s16module (type);
        newModules.push (m);
        m.file = f;
	m.fileReader = mkReader (m);
        m.fileReadComplete = false;
	m.fileReader.readAsText (f);
    }
}

function mkReader (m) {
    let fr = new FileReader();
    com.mode.trace = true;
    com.mode.devlog (`Creating file reader ${m.file.name}`);
    fr.onload = function (e) {
	com.mode.devlog (`File reader ${m.file.name} onload event starting`);
        m.text = e.target.result;
        com.mode.devlog (m.text);
        m.fileReadComplete = true;
        refreshWhenReadsFinished (m); // if all files are in, then refresh
	com.mode.devlog (`File reader ${m.file.name} onload event finished`);
    }
    fr.onerror = function (e) {
        com.mode.devlog (`Error: could not read file ${m.file.fname}`
                     + ` (error code = ${e.target.error.code})`);
        m.text = "";
        m.fileReadComplete = true;
    }
    return fr;
}

// If all the files selected in the file chooser have been read in,
// refresh the modules list.  The refresh should be called just one
// time.

function refreshWhenReadsFinished  (m) {
    com.mode.devlog (`refreshWhenReadsFinished`);
    let allOK = true;
    for (let x of newModules) {
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

// Use unique number for prefix name for each button
let buttonPrefixNumber = 0;

// Produce a formatted list of all open modules and display in Modules page

export function refreshModulesList() {
    console.log ('refreshModulesList');
    const xs = showS16Module ();
    console.log (xs);
    document.getElementById('FilesBody').innerHTML = xs;
    for (const bn of moduleEnvironment.keys()) {
        const m = moduleEnvironment.get(bn);
        const selElt = document.getElementById(m.selectId);
        selElt.addEventListener ("click", event => { handleSelect (bn) });
        const closeElt = document.getElementById(m.closeId);
        closeElt.addEventListener ("click", event => { handleClose (bn) });
    }
}

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

    
// Copy text of the selected module to the editor buffer

function refreshEditorBuffer () {
    com.mode.devlog (`refreshEditorBuffer`);
    document.getElementById("EditorTextArea").value = getSelectedModule().text;
}
