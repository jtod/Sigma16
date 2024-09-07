// Sigma16: s16module.mjs
// Copyright (C) 2024 John T. O'Donnell.  License: GNU GPL Version 3
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

//--------------------------------------------------------------------------
// S16module.mjs: represent S16 module and set of modules, handle files
//--------------------------------------------------------------------------

import * as com from "./common.mjs";
import * as st  from "./state.mjs";
import * as asm  from "./assembler.mjs";
// import * as file  from "./file.mjs";

const Unavailable = "Unavailable until the source code is assembled\n"



//--------------------------------------------------------------------------
// Testing and diagnostics
//--------------------------------------------------------------------------

export async function test1 () {
    console.log ("****** ModSet test1 ******")
    const xs = await readFile ()
    console.log (xs)
}

export function test2 () {
    console.log ("****** ModSet test2 ******")
    showModElts ()
}

export function test3 () {
    console.log ("****** ModSet test3 ******")
}

// Traverse Module Set Element and show modules.  Note that m.children
// gives all Element children, while m.childNodes gives all nodes in
// m, which includes whitespace and comments as well as elements.

export function showModElts () {
    console.log ("Elements of the Module Set")
    let m = document.getElementById("ModSetControls")
    //    let xs = m.children
    let xs = m.childNodes
    console.log (`Element ModSetControls has ${xs.length} child nodes`)
    for (let x of xs) {
        console.log (x)
    }
}

//--------------------------------------------------------------------------
// file.mjs: access files on local client computer
//--------------------------------------------------------------------------

// import * as com from "./common.mjs";
// import * as st  from "./state.mjs";

// There are two sets of functions: the old version using FileReader
// works on mlst browsers.  The new superior version uses
// FileReaderAccess API and works on Chrome and Edge, but not on
// Safari or Firefox.

//-------------------------------------------------------------------------
// New version: Files using File System Access API
// Supported on Chrome, Edge, but not Safari, Firefox
//-------------------------------------------------------------------------

// var base = new String(str).substring(str.lastIndexOf('/') + 1);

// For foo.asm.txt, return "foo".  Treat basename as all characters up
// the first period

export function getFileBaseName (fname) {
    const i = fname.indexOf (".");
    const baseName = fname.substring(0,i);
//    console.log (`getFileBaseName ${fname} ${i} ${baseName}`);
    return baseName;
}

// For foo.asm.txt, return ".asm.txt".  Treat extension as all
// characters from (and including) the first period

export function getFileExtension (fname) {
    const i = fname.indexOf (".");
    const extension = fname.substring(i);
//    console.log (`getFileExtension ${fname} ${i} ${extension}`);
    return extension;
}

export async function openFile () {
    console.log (`ModSet: open file`)
    const openOptions = {
        types: [
            {description: "Source file",
             accept: { "text/plain": [".asm.txt"] }},
            {description: "Object file",
             accept: { "text/plain": [".obj.txt"] }}
            ],
        excludeAcceptAllOption: false
    }
    const [fileHandle] = await window.showOpenFilePicker (openOptions)
    const file = await fileHandle.getFile ()
    await file.text ()
        .then (xs => {
//            console.log (`openFile lambda xs = ${xs}`)
            const fn = file.name
            const modName = getFileBaseName (fn)
            //            const m = st.env.moduleSet.addModule ()
            const m = st.env.moduleSet.addModule (modName,xs)            
            m.fileHandle = fileHandle
            st.handleSelect (m)
            m.changeSavedAsmSrc (xs)
            m.filename = fn
            console.log (`calling m.setModuleName ${modName}`)
            m.setModuleName (modName)
            console.log (`openFile module ${m.moduleName}`)
            document.getElementById("EditorTextArea").value = xs
            console.log ("openFile just changed asm src")
//            console.log (`openFile received fn=${fn} xs=${xs}`)
        }, () => {
            console.log ("failed to read file")
        })
}

export async function refreshFile () {
    console.log (`ModSet: refresh file`)
    const m = st.env.moduleSet.getSelectedModule ()
    const fileHandle = m.fileHandle
    if (fileHandle) {
        console.log ("refreshFile has a fileHandle")
        const file = await fileHandle.getFile()
        console.log ("refreshFile 2")
        await file.text ()
            .then (xs => {
                console.log (`refreshFile lambda xs = ${xs}`)
                m.changeAsmSrc (xs)
                 document.getElementById("EditorTextArea").value = xs
            }, () => {
                console.log ("refresh: failed to read file")
            })
    } else {
        console.log ("Cannot refresh: module is not associated with a file")
    }
}

export async function saveFile () {
    console.log ("Save")
    const m = st.env.moduleSet.getSelectedModule ()
    const fh = m.fileHandle
    const xs = document.getElementById("EditorTextArea").value
    const writable = await fh.createWritable()
    await writable.write(xs)
    await writable.close()
    m.currentAsmSrc = xs
    m.savedAsmSrc = xs
}

export async function saveAsFile () {
    console.log ("Save as...")
    const m = st.env.moduleSet.getSelectedModule ()
    const fh = await window.showSaveFilePicker ()
    const file = await fh.getFile ()
    const fn = file.name
    const xs = document.getElementById("EditorTextArea").value
    const writable = await fh.createWritable()
    await writable.write(xs)
    await writable.close()
    m.fileHandle = fh
    m.filename = fn
    m.currentAsmSrc = xs
    m.savedAsmSrc = xs
}

export async function openDirectory () {
    console.log (`ModSet: open directory`)
    // get a FileSystemDirectoryHandle
    const dirHandle = await window.showDirectoryPicker ()
    // fh is a fileHandle for an entry in the directory
    for await (const fh of dirHandle.values()) {
        if (fh.kind !== "file") {
            console.log (`skipping fh=${fh}`)
            continue
        }
        const file = await fh.getFile ()
        const xs = await file.text ()
        const fn = await file.name
        let basename = getFileBaseName (fn)
        let extension = getFileExtension (fn)
//        console.log (`file basename=${basename} extension=${extension}`)
        if (extension === ".asm.txt") { // handle a source file
            const m = st.env.moduleSet.addModule (basename, xs) // get basename
            st.handleSelect (m)
            document.getElementById ("EditorTextArea").value = xs
            m.fileHandle = fh
            m.filename = fn
            m.changeSavedAsmSrc (xs)
//            console.log (`  file ${fn} of size ${file.size}`)
        } else { // skip file, doesn't end in .asm.txt
//            console.log (`Skipping ${fn}, not .asm.txt source file`)
        }
    }
}

//-------------------------------------------------------------------------
// File access, old legacy version using FileReader (ok on most browsers)
//-------------------------------------------------------------------------

// Interface
//   Modules: Choose Files button -- "change" event calls handleSelectedFiles
//   Modules: Refresh button -- refreshModulesList

export function showFileShort (label, fr) {
    let xs = label;
    if (fr) {
        //        xs += `basename=${fr.baseName}`;
        xs += `moduleName=${fr.moduleName}`;
        xs += ` stage=${fr.stage.description}`;
        xs += ` fileReadComplete=${fr.fileReadComplete}\n`;
        xs += fr.text.split("\n").slice(0,5).join("\n");
    } else {
        xs += "null";
    }
    return xs;
}

export const TextPrefixLength = 6;

function showFilePrefix (fr, label) {
    if (fr) {
        return "<div class='HighlightedTextAsHtml'>"
            + `<span class="TEXTLABEL">${label}</span><br>`
            + fr.text.split("\n").slice(0,TextPrefixLength).join("<br>")
            + "<div>\n";
    } else {
        return "";
    }
}

function textPrefix (xs) {
    return "<div class='HighlightedTextAsHtml'>"
        + xs.split("\n").slice(0,4).join("<br>")
        + "<div>\n";
}

//--------------------------------------------------------------------------
// File record
//--------------------------------------------------------------------------

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
        selElt.addEventListener ("click", event => { st.handleSelect (bn) });
        const closeElt = document.getElementById(m.closeId);
        closeElt.addEventListener ("click", event => { handleClose (bn) });
    }
}
