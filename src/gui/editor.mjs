// Sigma16: editor.mjs
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

export function enterEditor () {
    com.mode.trace = true;
    com.mode.devlog ("ed.enterEditor");
    let m = st.env.getSelectedModule (); // module to be edited
    let bn = m.baseName; // name of the module
    let stage = m.edCurrentStage; // stage of the module to edit
    let xs = `${bn} ${stage.description}`;
    document.getElementById("EDP_Selected").innerText = xs;
    edGetTextToEdit (m);
}

// Get the text to edit
export function edGetTextToEdit (m) {
    const s = m.edCurrentStage;
    const xs = s == st.StageAsm ? m.getAsmText ()
          : s == st.StageObj ? m.getObjText ()
          : s == st.StageLnk ? m.getLnkText ()
          : s == st.StageExe ? m.getExeText ()
          : "";
    setEditorBufferText (xs);    
}

export function edSaveTextToEdit (m) {
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

// Check to see if the contents of the editor buffer have changed

export function leaveEditor () {
    com.mode.devlog ('leaveEditor called');
    const m = st.env.getSelectedModule (); // module to be edited
    edSaveTextToEdit (m);
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

