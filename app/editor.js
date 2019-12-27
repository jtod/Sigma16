// editor.js


let currentFile = null;  // Remember the current working file handle

// save file in downloads


function editorDownload () {
    console.log ("editorPrepareDownload");
    let downloadElt = document.getElementById("editorDownloadAnchor");
    let edText = editorBufferTextArea.value;
    downloadElt.href = makeTextFile(edText);  // provide text to download
    downloadElt.click();  // perform the download
}

function handleSelectedFiles (flist) {
    console.log("handleSelectedFiles");
    for (let i=0; i<flist.length; i++) {
	console.log (`file ${openFilesCount}`);
	openFiles[openFilesCount] = flist[i];
	openFilesReaders[openFilesCount] = mkOfReader(openFilesCount);
	openFilesText[openFilesCount] = "";
	openFilesReaders[openFilesCount].readAsText(openFiles[openFilesCount]);
	openFilesCount++;
    }
}

function mkOfReader (i) {
    console.log (`ofReader ${i}`);
    let fr = new FileReader();
    fr.onload = function (e) {
	console.log (`ofReader ${i} onload event`);
	openFilesText[i] = e.target.result;
    }
    return fr;
}

function showFiles() {
    console.log ('showFiles');
    let xs;
    let ys = "";
    let sel;
    let spanClass;
    let modName;
    for (let i=0; i<openFiles.length; i++) {
	modName = "<anonymous>";
	sel = openFilesSelected===i;
	spanClass = sel ? " class='SELECTEDFILE'" : " class='UNSELECTEDFILE'";
	ys += (sel ? '* ' : '  ')
	    + `<span${spanClass}>` + `Module ${i} </span>`
	    + `<button onclick="filesButtonSelect(${i})">Select</button>`
	    + `<button onclick="filesButtonClose(${i})">Close</button>`
	    + `<button onclick="filesButtonRefresh(${i})">Refresh</button>`
	    + '\n<pre>'
	    + openFilesText[i].split('\n').slice(0,4).join('\n')
	    + '</pre>\n\n\n';
    }
    console.log (spanClass);
    console.log (ys);
    let elt = document.getElementById('FilesBody');
    elt.innerHTML = ys;
}

function filesButtonSelect (i) {
    console.log (`filesButtonSelect ${i}`);
    openFilesSelected = i;
    editorBufferTextArea.value = openFilesText[i];
}

function filesButtonClose (i) {
    console.log (`filesButtonClose ${i}`);
}

function filesButtonRefresh (i) {
    console.log (`filesButtonRefresh ${i}`);
    openFilesReaders[i].readAsText(openFiles[i]);
//	openFilesReaders[openFilesCount].readAsText(openFiles[openFilesCount]);
}

function refreshFiles () {
    openFilesCount = 0;
    refreshFilesLooper();
}

function refreshCurrentFile() {
    console.log ("refreshCurrentFile");
    fileContents = fileReader.readAsText(currentFile);
    console.log (fileContents);
}
