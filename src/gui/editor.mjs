// Sigma16: editor.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3
// See Sigma16/README, LICENSE, and https://github.com/jtod/Sigma16

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
// editor.mjs provides a minimal text editor for writing and modifying
// code, both assembly language and object code.
//-----------------------------------------------------------------------------

import * as com   from '../base/common.mjs';
import * as smod  from '../base/s16module.mjs';
import * as arch  from '../base/architecture.mjs';
import * as arith from '../base/arithmetic.mjs';
import * as st    from '../base/state.mjs';

//-----------------------------------------------------------------------------
// Operations on editor text buffer
//-----------------------------------------------------------------------------

export function getEditorBufferText () {
    const xs = document.getElementById("EditorTextArea").value
    return xs
}

// Get the text to edit from selected module
export function edGetTextToEdit () {
    const m = st.env.moduleSet.getSelectedModule ()
    const xs = m.currentSrc
    return xs
}

// Copy editor buffer contents to module set
export function saveEditorBufferText () {
    const m = st.env.moduleSet.getSelectedModule ()
    const xs = getEditorBufferText ()
    //    m.currentSrc = xs
//    console.log (`saveEditorBufferText m=${m.modKey} xs=${xs}`)
    m.changeAsmSrc (xs)
}

//--------------------------------------------------------------------------
// Entering and leaving the editor
//--------------------------------------------------------------------------

// Put source code of selected module in editor buffer
export function enterEditor () {
    com.mode.devlog ("enterEditor")
    let xs = edGetTextToEdit ()
    document.getElementById("EditorTextArea").value = xs
//    setEditorBufferText (xs)
    console.log (`enterEditor ${xs}`)
}

// Save contents of editor buffer into selected module
export function leaveEditor () {
    com.mode.devlog ('leaveEditor');
//    console.log (`leave editor, buffer=${getEditorBufferText()}`)
    saveEditorBufferText ()
}

//--------------------------------------------------------------------------
// Editor buttons
//--------------------------------------------------------------------------

export function edClear () {
    document.getElementById('EditorTextArea').value = "";
}

export function edNew () {
    console.log ("Editor new");
    saveEditorBufferText ()
    const m = st.env.moduleSet.addModule ()
    smod.handleSelect (m)
    document.getElementById("EditorTextArea").value = ""
}

// THE FOLLOWING IS NO LONGER TRUE IN CHROMIUM BROWSERS...
// RETAINING FOR COMPATIBILITY WITH SAFARI, FIREFOX
// In a browser, the CORS restrictions make it impossible to do a
// general file "Save As..." operation.  Instead, the user can
// "Download" which allows them to decide where to save a file, and
// what filename to use.  The default is to save the file in the
// default Downloads folder.

export function edDownload () {
    console.log ("editorPrepareDownload");
    let downloadElt = document.getElementById("editorDownloadAnchor");
    let edText = document.getElementById("EditorTextArea").value;
    downloadElt.href = makeTextFile(edText);  // provide text to download
    downloadElt.click();  // perform the download
}

let textFile = null

function makeTextFile (text) {
    let data = new Blob([text], {type: 'text/plain'})
// If a previously generated file is being replaced, it's necessary to
// manually revoke the object URL to avoid memory leaks.
    if (textFile !== null) {
      window.URL.revokeObjectURL(textFile);
    }
    textFile = window.URL.createObjectURL(data)
    return textFile
}

// Leave the editor but first check to see if the editor is showing a
// file and the text has been modified, in which case the file is
// "stale".  To determine whether a file text is stale, we can't just
// compare the strings because of different end of line conventions.
// Therefore we convert the old and new source into an array of lines
// and compare those arrays.

// Copy the contents of the editor buffer to the selected module text.
// This is the normal action when leaving the Editor page

export function copyEditorBufferToModule () {
    com.mode.devlog ("copyEditorBufferToModule");
    const m = st.env.moduleSet.getSelectedModule ()
    const xs = getEditorBufferText ()
    m.currentSrc = xs
    console.log (`copyEditorBufferToModule ${xs}`)
}

//--------------------------------------------------------------------------
// Legacy method: Reading files selected by user
// Should work on non-Chromium browsers: hopefully FireFox and Safari
// For Chrome and Edge, the new functions are better.
//
// This code is from v3.5.0 s16module.mjs, and is retained for the
// time being for backward compatibility with browsers that don't yet
// support FileSystemAccess API

//---------------------------------------------------------------------------
// File record
//---------------------------------------------------------------------------

// A file record "fr" contains information about a file that has been
// selected and opened by the user.  It is constructed with "file", a
// file handle provided by html in response to a user open file
// request; the baseName of the file, and the stage (asm, obj, exe,
// lnk).  To extract metadata of a file from file record fr, use
// fr.f.type, fr.f.size, and fr.f.lastModifiedDate.

export class FileRecord  {
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

// When the user is in the Modules (now Editor) page and clicks Choose
// Files, a multiple file chooser widget is displayed, and its
// onchange event signals handleSelectedFiles with a list of file
// objects that were selected.  The function reads the files and
// creates entries in the modules list for them.

// Initialize the Modules page by setting up an event handler for the
// FileInput element.

export function prepareChooseFiles () {
    com.mode.devlog ("prepareChooseFiles")
    let elt = document.getElementById('FileInput')
    elt.addEventListener ('change', event => {
        handleSelectedFiles (elt.files)
    })
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
    let baseName;
    let stage
    for (let f of flist) {
        baseName = f.name
        //        const mod = st.env.mkSelectModule (baseName);
        const mod = new smod.Sigma16Module ()
        mod.fileHandle = f
        mod.filename = baseName
        mod.baseName = baseName
        const fileRecord = new FileRecord (f, baseName, null);
        mod.fileRecord = fileRecord
        newFiles.push(fileRecord);
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
    fr.onload = function (e) {
	com.mode.devlog (`File reader ${fileRecord.fileName} onload`);
	console.log (`File reader ${fileRecord.fileName} onload`);
        fileRecord.text = e.target.result;
        console.log (`file onload bn=${fileRecord.baseName} `
                     + `fileName=${fileRecord.fileName} `
                     + `text=${fileRecord.text}`);
        fileRecord.fileReadComplete = true;
        let m = new smod.Sigma16Module ()
        m.currentSrc = fileRecord.text
        m.savedSrc = fileRecord.text
    }
    fr.onerror = function (e) {
        com.mode.devlog (`Error: could not read file ${fileRecord.fileName}`
                     + ` (error code = ${e.target.error.code})`);
        fileRecord.fileReadComplete = true;
    }
    return fr;
}
