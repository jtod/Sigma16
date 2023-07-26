// Sigma16: s16module.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3
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
// S16module.mjs: represent S16 module and set of modules, handle files
//-----------------------------------------------------------------------------

import * as com from "./common.mjs";
import * as st  from "./state.mjs";
import * as asm  from "./assembler.mjs";

//-----------------------------------------------------------------------------
// Testing and diagnostics
//-----------------------------------------------------------------------------

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

//-------------------------------------------------------------------------
// Files - new version using File System Access API
//-------------------------------------------------------------------------

export async function openFile () {
    console.log (`ModSet: open file`)
    const [fileHandle] = await window.showOpenFilePicker()
    const file = await fileHandle.getFile()
    await file.text ()
        .then (xs => {
            console.log (`openFile lambda xs = ${xs}`)
            const fn = file.name
            const m = st.env.moduleSet.addModule ()
            handleSelect (m)
            m.changeAsmSrc (xs)
            document.getElementById("EditorTextArea").value = xs
            console.log ("openFile just changed asm src")
            m.fileHandle = fileHandle
            m.filename = fn
            console.log (`openFile received fn=${fn} xs=${xs}`)
        }, () => {
            console.log ("failed to read file")
        })
}

export async function refreshFile () {
    console.log (`ModSet: refresh file`)
    const m = st.env.moduleSet.getSelectedModule ()
    const fileHandle = m.fileHandle
    if (fileHandle) {
        const file = await fileHandle.getFile()
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
    m.currentSrc = xs
    m.savedSrc = xs
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
    m.currentSrc = xs
    m.savedSrc = xs
}

//----------------------------------------------------------------------------
// Sigma16 module
//----------------------------------------------------------------------------

// Container for all the data related to a specific source module.

export class Sigma16Module {
    constructor () {
         this.modKey = newModKey () // Persistent and unique gensym key
        this.modIdx = st.env.moduleSet.modules.length // Transient array index
        this.fileHandle = null
        this.filename = "(no file)"
        this.baseName = "(no file)"
        this.fileInfo = null // can hold instance of FileInfo
        this.isMain = true // may be changed by assembler
        this.currentSrc = "" // master copy of (possibly edited) source code
        this.savedSrc = "" // source code as last saved/read to/from file
        this.asmInfo = new asm.AsmInfo (this)
        this.displayElt = null // DOM element for module display on page
        this.displaySrcLineElt = null
        this.selectId = `SELECT-${this.modKey}`
        this.closeId = `CLOSE-${this.modKey}`
        this.upId = `UP-${this.modKey}`
        this.selectElt = null // set when addModule
        this.closeElt = null // set when addModule
        this.upElt = null // set when addModule
        this.setHtmlDisplay ()
    }
    getAsmText () {
        return this.asmInfo.asmSrcText
    }
    changeAsmSrc (txt) {
        console.log (`Module ${this.modKey} changeAsmSrc ${txt}`)
        this.currentSrc = txt
        document.getElementById("EditorTextArea").value = txt
//        this.asmInfo.objText = Unavailable
//        this.asmInfo.asmListingText = Unavailable
//        this.asmInfo.mdText = Unavailable
        this.displaySrcLineElt.textContent = txt.split("\n")[0]
        
    }
    setSelected (b) {
        let selTxt = b ? "Selected" : ""
        this.selectElt.textContent = selTxt
        st.env.moduleSet.previousSelectedIdx = this.modIdx
    }
    setHtmlDisplay () {
        const modPara = document.createElement ("p")
        const t1 = document.createElement ("span")
        t1.innerHTML = `<b>Module key=${this.modKey}.</b> `
        modPara.appendChild (t1)

        const t2 = document.createElement ("span")
        this.selectElt = t2
        t2.setAttribute ("id", `MODULE-${this.modKey}-SEL-FLAG`)
        const t2text = document.createTextNode ("selected")
        t2.appendChild (t2text)
        modPara.appendChild (t2)
        
        const bSelect = document.createElement ("button")
        bSelect.textContent = "Select"
        modPara.appendChild (bSelect)
        const bUp = document.createElement ("button")
        bUp.textContent = "Up"
        modPara.appendChild (bUp)
        const bClose = document.createElement ("button")
        bClose.textContent = "Close"
        modPara.appendChild (bClose)
        const br = document.createElement ("br")
        modPara.appendChild (br)

        const spanSrc = document.createElement ("span")
        spanSrc.setAttribute ("id", `MODULE-${this.modKey}-SRCLINE`)
        this.displaySrcLineElt = spanSrc
        const tSrcText = document.createTextNode (
            this.currentSrc.split("\n")[0])
            //            this.asmInfo.asmSrcText.split("\n")[0])
        spanSrc.appendChild (tSrcText)
        modPara.appendChild (spanSrc)

        const containerElt = document.getElementById("ModSetControls")
        containerElt.appendChild (modPara)
        this.displayElt = modPara
        bSelect.addEventListener (
            "click", event => handleSelect(this))
        bUp.addEventListener (
            "click", event => handleModUp (this))
        bClose.addEventListener (
            "click", event => handleClose (this))
    }
    // Use refreshInEditorBuffer when the text is changed from an
    // ourside source, such as reading a file.  Don't use this when
    // the user edits the text in the editor.
    refreshInEditorBuffer () {
        const xs = this.asmInfo.asmSrcText;
        console.log (`refreshInEditorBuffer xs=${xs}`);
        document.getElementById("EditorTextArea").value = xs;
    }
}

const Unavailable = "Unavailable until the source code is assembled\n"

// Generate a fresh module key
export let nextModKey = 0
export function newModKey () {
    let i = nextModKey
    nextModKey++
    return i
}

//----------------------------------------------------------------------------
// Handle controls for individual modules
//----------------------------------------------------------------------------

// Select module m, put it into Editor pane

export function handleSelect (m) {
    //    const oldSelIdx = st.env.moduleSet.selectedModuleIdx
    const oldSelIdx = st.env.moduleSet.previousSelectedIdx
    const newSelIdx = m.modIdx
    st.env.moduleSet.modules[oldSelIdx].setSelected (false)
    st.env.moduleSet.modules[newSelIdx].setSelected (true)
    st.env.moduleSet.selectedModuleIdx = newSelIdx
    console.log (`Select module old=${oldSelIdx}, new=${newSelIdx}`)
}

// Move current module m up in the Module Set list

export function handleModUp (m) {
    console.log (`Move module ${m.modKey} up`)
    let mIdx = m.modIdx
    let mElt = m.displayElt
    let maElt = mElt.previousElementSibling
    if (maElt) {
        let maIdx = mIdx - 1
        let ma = st.env.moduleSet.modules[maIdx]
        console.log (`Module Up: move m ${mIdx} up to before ma ${maIdx}`)
        console.log (`mElt=${mElt}`)
        console.log (`maElt=${maElt}`)
        m.modIdx = maIdx
        ma.modIdx = mIdx
        const containerElt = document.getElementById("ModSetControls")
        containerElt.insertBefore (mElt, maElt)
        st.env.moduleSet.modules[maIdx] = m
        st.env.moduleSet.modules[mIdx] = ma
    } else {
        console.log ("handleModUp: nothing to do")
    }
}

// Close a module and remove it from the module set

export function handleClose (m) {
    console.log (`Close module ${m.modKey}`)
    const elt = m.displayElt
    elt.remove ()
    const a = st.env.moduleSet.modules
    const i = m.modIdx

    let si = st.env.moduleSet.selectedModuleIdx
    let pi = st.env.moduleSet.previousSelectedIdx
    console.log (`close before: si=${si} pi=${pi}`)
    if (si == i) {
        si = si - 1 // max 0 si-1
    } else if (si > i) {
        si = si - 1
    }
    if (pi == i) {
        pi = 0
    } else if (pi > i) {
        pi = pi - 1
    }
    st.env.moduleSet.selectedModuleIdx = si
    st.env.moduleSet.previousSelectedIdx = pi
    console.log (`close after: si=${si} pi=${pi}`)
    
    console.log (`close: before, len=${a.length}`)
    for (let x of a) {
        console.log (`idx=${x.modIdx} key=${x.modKey}`)
    }
    a.splice (i,1)
    let newSize = a.length
    for (let j = i; j < newSize; j++) {
        a[j].modIdx = j
    }
    console.log (`close: after, len=${a.length}`)
    for (let x of a) {
        console.log (`idx=${x.modIdx} key=${x.modKey}`)
    }
}

//----------------------------------------------------------------------------
// Sigma16 module set
//----------------------------------------------------------------------------

// There is one ModuleSet object, st.env.moduleSet, which is an
// instance of class ModuleSet and is stored as a component of the
// global environment st.env.  It contains an array of all the extant
// modules, as well as keeping track of the currently selected module.
// Invariants: (1) at all times, there is at least one module, and (2)
// at all times, one module is selected, (3) the text of the selected
// module is in the editor buffer, and (4) at all times there is text
// for the object code, assembly listing, and metadata (if there's no
// valid data for these, there will be dummy text saying "Not
// available, the source needs to be assembled".  Usage example:
// st.env.moduleSet.getSelectedModule ()

export class ModuleSet {
    constructor () {
        console.log ('Initializing ModuleSet')
        this.modules = []
        this.selectedModuleIdx = 0
        this.previousSelectedIdx = 0
    }
    addModule () {
        const m = new Sigma16Module ()
        this.modules.push (m)
        this.selectedModuleIdx = this.modules.length - 1
        console.log (`addModule there are $(this.modules.length) modules\n`)
        return m
    }
    getSelectedModule () {
        return this.modules [this.selectedModuleIdx]
    }
    generateDisplay () {
        let xs = "<div class='HighlightedTextAsHtml'>\n"
        xs += "<h3>List of modules</h1>\n"
        this.modules.forEach ((m,i,a) => {
            let y = m.show();
            xs += `<b>Module.</b> (key=${m.modKey})`
            xs += (i==this.selectedModuleIdx ? ' Selected' : '')
            xs += `<button id='${this.selectId}'>Select</button>`;
            xs += `<button id='${this.closeId}'>Close</button>`;
            xs += `<br>\n`
            xs += m.show()
            xs += "<br>\n"
        })
        xs += "</div>\n"
        console.log (`\n*** Module Set (html)\n${xs}\n***`)
        return xs
        }
    refreshDisplay () {
         for (let i = 0; i < st.env.moduleSet.modules.length; i++) {
            console.log (`*** ${i} ${st.env.moduleSet.modules[i].modIdx} `)
            console.log (st.env.moduleSet.modules[i]
                         .asmInfo.asmSrcText.split("\n")[0])
            console.log ('\n')
        }
    }
}

export async function openDirectory () {
    console.log (`ModSet: open directory`)
    const dh = await window.showDirectoryPicker ()
    const promises = []
    let results = []
    let fs = []
    for await (const entry of dh.values()) {
        if (entry.kind !== "file") {
            continue
        }
        fs.push (entry.getFile())
    }
    console.log ("Open directory: the files")
    for await (const f of fs) {
        console.log (f.name)
        const xs = await f.text ()
        results.push ([f, xs])
    }
    console.log ("End of the filenames")
    console.log ("Here are the file sizes")
    for await (let r of results) {
        const [f,xs] = r
        console.log (f.size)
        const m = st.env.moduleSet.addModule ()
        handleSelect (m)
        m.changeAsmSrc (xs)
        document.getElementById ("EditorTextArea").value = xs
    }
}

//-------------------------------------------------------------------------
// Files - old version using FileReader
//-------------------------------------------------------------------------

// Interface
//   Modules: Choose Files button -- "change" event calls handleSelectedFiles
//   Modules: Refresh button -- refreshModulesList

export function showFileShort (label, fr) {
    let xs = label;
    if (fr) {
        xs += `basename=${fr.baseName}`;
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

// Deprecated...

//-----------------------------------------------------------------------------
// File record
//-----------------------------------------------------------------------------

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
        selElt.addEventListener ("click", event => { handleSelect (bn) });
        const closeElt = document.getElementById(m.closeId);
        closeElt.addEventListener ("click", event => { handleClose (bn) });
    }
}
