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


function handleSelectedFile (flist) {
    console.log("handleSelectedFile");
    console.log(flist);
    currentFile = flist[0];
    console.log("selected file = " + currentFile);
//    console.log("created fileReader" + fr);
    fileContents = fileReader.readAsText(currentFile);
    console.log (fileContents);
}

function refreshCurrentFile() {
    console.log ("refreshCurrentFile");
    fileContents = fileReader.readAsText(currentFile);
    console.log (fileContents);
}

// fileReader.onload is defined in gui.js during the onLoad event

// Save a file by downloading it to user default Downloads folder

