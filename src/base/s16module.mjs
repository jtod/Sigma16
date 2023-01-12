// Sigma16: module.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3 or later
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

//-------------------------------------------------------------------------------
// module.mjs defines the representation of source and object modules for the gui
//-------------------------------------------------------------------------------

import * as com from "./common.mjs";
import * as st  from "./state.mjs";

//------------------------------------------------------------------------------
// Files and modules
//------------------------------------------------------------------------------

// Interface
//   Modules: Choose Files button -- "change" event calls handleSelectedFiles
//   Modules: Refresh button -- refreshModulesList

//-------------------------------------------------------------------------------
// File record
//-------------------------------------------------------------------------------

// A file record "fr" contains information about a file that has been
// selected and opened by the user.  It is constructed with "file", a
// file handle provided by html in response to a user open file
// request; the baseName of the file, and the stage (asm, obj, exe,
// lnk).  To extract metadata of a file from file record fr, use
// fr.f.type, fr.f.size, and fr.f.lastModifiedDate.

export class FileRecord {
    constructor (f, baseName, stage) {
        this.f = f;
        this.fileName = f.name; // full filename
        this.baseName = baseName; // base part of filename
        this.stage = stage; // asm, obj, lnk, exe
        this.text = "";
        this.fileReader = mkFileReader (this);
        this.fileReadComplete = false;
    }
}

//-------------------------------------------------------------------------------
// Module pane buttons
//-------------------------------------------------------------------------------

// Handle the New button on Modules pane by creating a new anonymous
// module

export function newMod () {
    const x = st.env.mkSelectModule ();
    com.mode.devlog (`newMod ${x}`)
    refreshModulesList();
}

// Handle a "Select" button click in the list of modules on the
// Modules page: select the module with basename bn.

function handleSelect (bn) {
    com.mode.devlog (`Select button for ${bn} invoked`);
    let mod = st.env.mkSelectModule (bn);
    refreshModulesList ();
}

// Handle a "Close" button click in the list of modules on the Modules
// page: close the module with basename bn.

function handleClose (bn) {
    com.mode.devlog (`Close button for ${bn} invoked`);
    st.env.closeModule (bn);
    refreshModulesList ();
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

//-------------------------------------------------------------------------------
// Initialize modules
//-------------------------------------------------------------------------------

// Create and select an initial module

export function initModules () {
//    com.mode.trace = true;
    com.mode.devlog ("initModules");
    st.env.clearModules ();
    refreshModulesList();
}

//-------------------------------------------------------------------------------
// Reading files in the Sigma16 directories
//-------------------------------------------------------------------------------

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
    com.mode.devlog ("prepareChooseFiles")
    let elt = document.getElementById('FileInput')
    elt.addEventListener ('change', event => {
        handleSelectedFiles (elt.files)
    })
}

// Parse string xs and check that it's in the form basename.ftype.txt
// where basename is any string not containing "." and ftype is one of
// asm, obj, md, lst, lnk.

export function checkFileName (xs) {
    const components = xs.split(".");
    let errors = [];
    let baseName = null;
    let stage = st.StageExe;
    if (components.length != 3 ) {
        errors = [`Filename ${xs} must have three components, e.g.`
                  + `ProgramName.asm.txt`];
    } else if (components[2] != "txt") {
        errors = [`Last component of flename ${xs} is ${components[2]}`
                  + ` but it must be ".txt"`];
    } else if (!["asm", "obj", "lst", "omd", "lnk", "exe", "xmd"]
               .includes(components[1])) {
        errors = [`Second component of flename ${xs} is ${components[1]}`
                  + ` but it must be one of asm,obj,md,lnk`];
    } else {
        baseName = components[0];
        stage = st.getStageSym (components[1]);
    }
    let result = {errors, baseName, stage};
    com.mode.devlog (`checkFileName ${xs}\n errors=${result.errors}`
                     + ` baseName=${result.baseName}`
                     + ` stage=${result.stage.description}`);
    return result;
}

// When the user clicks Choose files, the browser produces a FileList
// object.  This function traverses that list and creates a module for
// each file


// When the Choose Files button is clicked, a file chooser dialogue
// box appears.  If the user selects one or more files and clicks
// open, handleSelectedFiles is called with a list of file handles for
// the files chosen by the user.  This function obtains and records
// metadata for each file, creates a fileReader object, and initiates
// the file read.

let newFiles = [];

export function handleSelectedFiles (flist) {
//    com.mode.trace = true;
    com.mode.devlog (`*** handleSelectedFiles: ${flist.length} files`);
    console.log (`*** handleSelectedFiles: ${flist.length} files`);
    newFiles = [];
    for (let f of flist) {
        const {errors, baseName, stage} = checkFileName (f.name);
        const mod = st.env.mkSelectModule (baseName);
        const fileRecord = new FileRecord (f, baseName, stage);
        newFiles.push(fileRecord);
        switch (stage) {
        case st.StageAsm :
            mod.asmFile = fileRecord;
            break;
        case st.StageObj:
            mod.objFile = fileRecord;
            break;
        case st.StageExe:
            mod.exeFile = fileRecord;
            break;
        case st.StageLnk:
            mod.linkFile = fileRecord;
            break
        }
        fileRecord.fileReader.readAsText (f);
        console.log ("******** handleSelected Files: mod.showshort");
        console.log (mod.showShort());
    }
    console.log ("handleSelectedFiles at end");
    st.envSummary();
}

function mkFileReader (fileRecord) {
//    com.mode.trace = true;
    const fr = new FileReader();
//    com.mode.trace = true;
    fr.onload = function (e) {
	com.mode.devlog (`File reader ${fileRecord.fileName} onload`);
	console.log (`File reader ${fileRecord.fileName} onload`);
        fileRecord.text = e.target.result;
        console.log (`file onload bn=${fileRecord.baseName} `
                     + `fileName=${fileRecord.fileName} text=${fileRecord.text}`);
        fileRecord.fileReadComplete = true;
        let m = st.env.mkSelectModule (fileRecord.baseName)
        switch (fileRecord.stage) {
        case st.StageAsm: m.asmEdText = fileRecord.text
            console.log (`set asmEdText = ${m.asmEdText}`)
            break
        case st.StageObj: m.objEdText = fileRecord.text
            break
        case st.StageLnk: m.lnkEdText = fileRecord.text
            break
        case st.StageExe: m.exeEdText = fileRecord.text
            break
        default:
            console.log ("*** Error file read, unknown stage")
        }
        refreshWhenReadsFinished ();
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

// function refreshWhenReadsFinished  (m) {
function refreshWhenReadsFinished  () {
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
            let elt = document.getElementById('FileInput');
            elt.value = "" // clear file name(s) to allow re-reading same file(s)
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

export function refreshEditorBuffer () {
    com.mode.devlog (`refreshEditorBuffer`);
    console.log (`refreshEditorBuffer`);
    const mod = st.env.getSelectedModule ();
    const xs = mod.asmInfo ? mod.asmInfo.text : "";
    document.getElementById("EditorTextArea").value = xs;
}
