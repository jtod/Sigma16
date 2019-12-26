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
    console.log("handleSelectedFiles changed");
//    console.log(flist);
    for (let i=0; i<flist.length; i++) {
	console.log (`file ${i}`);
	openFiles[i] = flist[i];
	openFilesReaders[i] = mkOfReader(i);
	openFilesText[i] = "";
	openFilesReaders[i].readAsText(openFiles[i]);
    }
}

function mkOfReader (i) {
    console.log (`ofReader ${i}`);
    let fr = new FileReader();
    fr.onload = function (e) {
	console.log (`ofReader ${i} onload event`);
	openFilesText[i] = e.target.result.split('\n');
    }
    return fr;
}

function showFiles() {
    console.log ('showFiles');
    let xs;
    let ys = "";
    for (let i=0; i<openFiles.length; i++) {
	ys += `file ${i}\n`;
	xs = openFilesText[i];
	ys += xs.slice(0,4).join('\n');
	ys += "\n\n\n";
    }
    editorBufferTextArea.value = ys;
}

function refreshFiles () {
    openFilesIndex = 0;
    refreshFilesLooper();
}

function refreshCurrentFile() {
    console.log ("refreshCurrentFile");
    fileContents = fileReader.readAsText(currentFile);
    console.log (fileContents);
}

//	console.log (xs.slice(0,100));
//	console.log (`\n\n`);
//	console.log (xs);
//	console.log (ys);
	
