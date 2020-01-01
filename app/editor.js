// Sigma16: editor.js
// Copyright (c) 2019 John T. O'Donnell.  <john dot t dot odonnell9 at gmail.com>
// License: GNU GPL Version 3 or later. See Sigma6/ LICENSE.txt, LICENSE-NOTICE.txt

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
// editor.js provides a minimal text editor for writing and modifying
// code, both assembly language and object code.

//-------------------------------------------------------------------------------

let currentFile = null;  // Remember the current working file handle

function editorDownload () {
    console.log ("editorPrepareDownload");
    let downloadElt = document.getElementById("editorDownloadAnchor");
    let edText = editorBufferTextArea.value;
    downloadElt.href = makeTextFile(edText);  // provide text to download
    downloadElt.click();  // perform the download
}

function handleSelectedFiles (flist) {
    console.log("handleSelectedFiles");
    let m;
    for (let i=0; i<flist.length; i++) {
	m = mkModule ();
	m.mFile = flist[i];
	m.hasFile = true;
	m.selected = false;
	m.fileReader = mkOfReader(nModules);
	m.modSrc = "";
	m.fileReader.readAsText(m.mFile);
	s16modules.push(m);
	nModules++;
    }
}

function mkOfReader (i) {
    console.log (`ofReader ${i}`);
    let fr = new FileReader();
    fr.onload = function (e) {
	console.log (`ofReader ${i} onload event`);
	s16modules[i].modSrc = e.target.result;
    }
    return fr;
}

function showModules() {
    console.log ('showModules');
    let xs;
    let ys = "\n<hr>";
    let sel;
    let spanClass;
    let mName;
    let m;
    for (let i=0; i<nModules; i++) {
	m = s16modules[i];
	mName = getModName(m);
	sel = selectedModule===i;
	spanClass = sel ? " class='SELECTEDFILE'" : " class='UNSELECTEDFILE'";
	ys += `&nbsp;`
	    +`<span${spanClass}>${i}${(sel ? '* ' : '  ')}. Module ${mName}</span>`
	    + `<button onclick="modulesButtonSelect(${i})">Select</button>`
	    + `<button onclick="modulesButtonClose(${i})">Close</button>`
	    + `<button onclick="modulesButtonRefresh(${i})">Refresh</button>`
            + `<br>hasFile=${m.hasFile} fileName=${m.fileName} modName=${m.modName}`
            + `<br>nAsmErrors=${m.nAsmErrors} isExecutable=${m.isExecutable}`
	    + '<br><pre>'
	    + m.modSrc.split('\n').slice(0,8).join('\n')
	    + '</pre>\n\n'
            + '<hr>\n\n';
    }
    console.log (spanClass);
    console.log (ys);
    let elt = document.getElementById('FilesBody');
    elt.innerHTML = ys;
}

function modulesButtonSelect (i) {
    console.log (`modulesButtonSelect ${i}`);
    s16modules[selectedModule].selected = false;
    selectedModule = i;
    s16modules[i].selected = true;
    editorBufferTextArea.value = s16modules[i].modSrc;
}

// Need to be careful about this affecting refresh, as i has been
// baked into the file reader
function modulesButtonClose (i) {
    console.log (`filesButtonClose ${i}`);
}

// need to set event handler to refresh the modules list
function modulesButtonRefresh (i) {
    console.log (`filesButtonRefresh ${i}`);
    s16modules[i].fileReader.readAsText(s16modules[i].mFile);
}
