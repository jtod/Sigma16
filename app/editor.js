// Sigma16: editor.js

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
    let ys = "";
    let sel;
    let spanClass;
    let modName;
    let m;
    for (let i=0; i<nModules; i++) {
	m = s16modules[i];
	modName = "<anonymous>";
	sel = selectedModule===i;
	spanClass = sel ? " class='SELECTEDFILE'" : " class='UNSELECTEDFILE'";
	ys += (sel ? '* ' : '  ')
	    + `<span${spanClass}>` + `Module ${i} </span>`
	    + `<button onclick="modulesButtonSelect(${i})">Select</button>`
	    + `<button onclick="modulesButtonClose(${i})">Close</button>`
	    + `<button onclick="modulesButtonRefresh(${i})">Refresh</button>`
	    + '\n<pre>'
	    + m.modSrc.split('\n').slice(0,4).join('\n')
	    + '</pre>\n\n\n';
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
