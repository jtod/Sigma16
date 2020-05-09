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

import * as com from './common.mjs';
import * as smod from './s16module.mjs';


export let editorBufferTextArea = document.getElementById("EditorTextArea");

export function editorDownload () {
    console.log ("editorPrepareDownload");
    let downloadElt = document.getElementById("editorDownloadAnchor");
    let edText = editorBufferTextArea.value;
    downloadElt.href = makeTextFile(edText);  // provide text to download
    downloadElt.click();  // perform the download
}

// Leave the editor but first check to see if the editor is showing a
// file and the text has been modified, in which case the file is
// "stale".  To determine whether a file text is stale, we can't just
// compare the strings because of different end of line conventions.
// Therefore we convert the old and new source into an array of lines
// and compare those arrays.

export function leaveEditor () {
    console.log ('Leaving the editor');
    console.log (`leave editor ${smod.selectedModule}`);
    let m = smod.s16modules[smod.selectedModule];
    let baz = smod.s16modules ? "yes" : "no";
    let foobar = m ? "yes" : "no";
    console.log (`leave editor len moduels = ${smod.s16modules.length}`);
    console.log (`leave editor baz ${baz}`);
    console.log (`leave editor foobar ${foobar}`);
    
    let ma = m.asmInfo;
    if (m) {
        let oldSrc = ma.modSrc;
        let oldSrcCanonical = oldSrc.replace (/\r\n/gm, '\n');
        let newSrc = document.getElementById("EditorTextArea").value;
        let newSrcCanonical = newSrc.replace (/\r\n/gm, '\n');
        if (m.mFile && (oldSrcCanonical != newSrcCanonical)) {
            m.fileStale = true;
        }
        ma.modSrc = newSrc;
        let srcHead = ma.modSrc.split('\n').slice(0,4).join('\n');
        console.log (`leaving editor, src=${srcHead}`);
    } else {
        console.log ("leaving editor, don't have module")
    }
//    hideTabbedPane("EditorPane");
}

export function editorClear () {
    document.getElementById('EditorTextArea').value = "";
}
