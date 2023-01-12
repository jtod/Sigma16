// Sigma16: editor.mjs
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
// Operations on editor buffer
//-----------------------------------------------------------------------------

export function getEditorBufferText () {
    const xs = document.getElementById("EditorTextArea").value;
    return xs;
}

export function setEditorBufferText (xs) {
    document.getElementById("EditorTextArea").value = xs;
}

//-----------------------------------------------------------------------------
// Actions on entering and leaving the editor
//-----------------------------------------------------------------------------

// If there is a selected module, copy its text into the editor
// buffer; if not, then clear the editor buffer, and later when
// leaving the editor a module to hold it will be created and selected

export function enterEditor () {
//    com.mode.trace = true;
    com.mode.devlog ("ed.enterEditor");
    let m = st.env.getSelectedModule (); // module to be edited
    if (m) {
        console.log ("enterEditor, found selectedModule")
        let bn = m.baseName; // name of the module
        let stage = m.edCurrentStage; // stage of the module to edit
        let xs = `${bn} ${stage.description}`;
        document.getElementById("EDP_Selected").innerText = xs;
        edGetTextToEdit (m);
    } else {
        console.log ("enterEditor, no selectedModule")
        setEditorBufferText ("")
    }
}

// If there is a selected module, copy the text from the editor buffer
// back to that module.  If not, create a new module and copy the
// editor buffer text into it.

export function leaveEditor () {
    com.mode.devlog ('leaveEditor called');
    const m = st.env.getSelectedModule (); // module to be edited
    if (m) {
        saveEditorBufferText (m);
    } else {
        const y = findModName (getEditorBufferText ())
        const x = y ? y : "EditorText"
        console.log (`leaveEditor ${x}`)
        let m = st.env.mkSelectModule (y)
        saveEditorBufferText (m)
    }
}

export function findModName (xs) {
    const modNameParser = /\s*;\s*([a-zA-Z0-9_]+)/
    const q = modNameParser.exec (xs.split("\n")[0])
    let y
    if (q) {
        console.log (`findModName 0=<${q[0]}> 1=<${q[1]}> 2=<${q[2]}>`)
        y = q[1]
    } else {
        y = "EditorText"
    }
    return y
}


function isEditorTextHtml (xs) {
    const htmlDetector = /\s*<\?xml\sversion=/
    const q = htmlDetector.exec (xs.split("\n")[0])
    let isHtml
    if (q) { // xs looks like an html index
        isHtml = true
    } else { // xs looks like example program text
        isHtml = false
    }
}

// Get the text to edit from module m
export function edGetTextToEdit (m) {
    const s = m.edCurrentStage;
    const xs = s == st.StageAsm ? m.getAsmText ()
          : s == st.StageObj ? m.getObjText ()
          : s == st.StageLnk ? m.getLnkText ()
          : s == st.StageExe ? m.getExeText ()
          : "";
    setEditorBufferText (xs);    
}

export function saveEditorBufferText (m) {
    const xs = getEditorBufferText ()
    const s = m.edCurrentStage
    switch (s) {
    case st.StageAsm: m.asmEdText = xs; break;
    case st.StageObj: m.objEdText = xs; break;
    case st.StageLnk: m.lnkEdText = xs; break;
    case st.StageExe: m.exeEdText = xs; break;
    default: m.asmEdText = xs
    }
}

export function edSelectedButton () {
    console.log ("edSelectedButton clicked");
}


function setEditorText (stage, text) {
    document.getElementById('EditorTextArea').value = text;
    currentStage = stage;
}


//-----------------------------------------------------------------------------
// Editor buttons
//-----------------------------------------------------------------------------

export function edClear () {
    document.getElementById('EditorTextArea').value = "";
}

export function edNew () {
    console.log ("Editor new");
    st.env.selectedModule = null
    setEditorBufferText ("")
}

export function edRevert () {
    console.log ("Editor revert");
}

export function edAsm () {
    console.log ("Editor assembly language");
}

export function edObj () {
    console.log ("Editor object code");
}

export function edExe () {
    console.log ("Editor executable code");
}

export function edLink () {
    console.log ("Editor linker command");
}

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
    const m = st.env.getSelectedModule ();
    m.text = document.getElementById("EditorTextArea").value;
}

