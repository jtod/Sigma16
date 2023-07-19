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

// testing and to do
// add id tag to module Element for (1) file base name and (2) src code line
// setAsmSrc - modify src code line in module Element
// module keeps track of original text and current text, changed/unchanged

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
    let x = document.getElementById ("MODULE-2-SEL-FLAG")
    let y = x.textContent
    console.log (`test3 ${y}`)
    x.textContent = "Ha I changed it"
//    st.env.moduleSet.refreshDisplay()
}

// Diagnostic function: traverse Module Set Element and show modules.
// Note that m.children gives all Element children, whild m.childNodes
// gives all nodes in m, which includes whitespace and comments as
// well as elements.

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

//----------------------------------------------------------------------------
// Sigma16 module
//----------------------------------------------------------------------------

// Container for all the data related to a specific source module.

export class Sigma16Module {
    constructor () {
        this.modKey = newModKey ()
        this.modIdx = st.env.moduleSet.modules.length
        this.baseName = "(no file)"
        this.fileInfo = null // can hold instance of FileInfo
        this.isMain = true // may be changed by assembler
        this.asmInfo = new asm.AsmInfo (this)
        this.displayElt = null
        this.selectId = `SELECT-${this.modKey}`
        this.closeId = `CLOSE-${this.modKey}`
        this.upId = `UP-${this.modKey}`
        this.selectElt = null // set when addModule
        this.closeElt = null // set when addModule
        this.upElt = null // set when addModule
    }
    getAsmText () {
        return this.asmInfo.asmSrcText
    }
    changeAsmSrc (txt) {
        // also: make sure the editor text and original file text (if
        // exists) are ok
        console.log (`Module changeAsmSrc ${txt}`)
        this.asmInfo.asmSrcText = txt;
        this.asmInfo.objText = Unavailable
        this.asmInfo.asmListingText = Unavailable
        this.asmInfo.mdText = Unavailable
        const srcLineElt =
              document.getElementById (`MODULE-${this.modKey}-SRCLINE`)
        //        srcLineElt.textContent = txt  ????????????????? later
        
    }
    setSelected (b) {
        let selTxt = b ? "Selected" : ""
        this.selectElt.textContent = selTxt
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
        const tSrcText = document.createTextNode (
            this.asmInfo.asmSrcText.split("\n")[0])
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
    const oldSelIdx = st.env.moduleSet.selectedModuleIdx
    const newSelIdx = m.modIdx
    st.env.moduleSet.modules[oldSelIdx].setSelected (false)
    st.env.moduleSet.modules[newSelIdx].setSelected (true)
    st.env.moduleSet.selectedModuleIdx = newSelIdx
    console.log (`Select module old=${oldSelIdx}, new=${newSelIdx}`)
}
//    console.log (`Select module ${m.modKey}, i=${i}`)
//    m.setSelected (true)

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

//-------------------------------------------------------------------------
// Files - new version using File System Access API
//-------------------------------------------------------------------------

export async function openFile () {
    console.log (`ModSet: open file`)
    const [fileHandle] = await window.showOpenFilePicker()
    const file = await fileHandle.getFile()
    const xs = await file.text ()
    console.log (xs)
    const m = st.env.moduleSet.getSelectedModule ()
    m.changeAsmSrc (xs)
}

export async function openDirectory () {
    console.log (`ModSet: open directory`)
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

//-----------------------------------------------------------------------------
// Module pane buttons
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Reading files in the Sigma16 directories
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Reading files selected by user
//-----------------------------------------------------------------------------

// When the user is in the Modules page and clicks Choose Files, a
// multiple file chooser widget is displayed, and its onchange event
// signals handleSelectedFiles with a list of file objects that were
// selected.  The function reads the files and creates entries in the
// modules list for them.

// Initialize the Modules page by setting up an event handler for the
// FileInput element.

export function prepareChooseFiles () {
    com.mode.devlog ("prepareChooseFiles")
    let elt = document.getElementById('FileInput')
    elt.addEventListener ('change', event => {
        handleSelectedFiles (elt.files)
    })
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

//-----------------------------------------------------------------------------
// Deprecated
//-----------------------------------------------------------------------------

//-----------------------------------------------------------------------------
// Display list of modules
//-----------------------------------------------------------------------------

// Produce a formatted list of all open modules and display in Modules page

/*
export function refreshModulesList() {
    console.log ('refreshModulesList')
    let xs = st.env.ModuleSet.show ()
    document.getElementById('FilesBody').innerHTML = xs;
}
*/

//    let xs = "";
//    for (const bn of st.env.modules.keys ()) {
//        xs += st.env.modules.get(bn).showHtml();
//    }
//    document.getElementById('FilesBody').innerHTML = xs;
//    for (const bn of st.env.modules.keys ()) {
//        const m = st.env.modules.get (bn);
//        const selElt = document.getElementById(m.selectId);
//        selElt.addEventListener ("click", event => { handleSelect (bn) });
//        const closeElt = document.getElementById(m.closeId);
//        closeElt.addEventListener ("click", event => { handleClose (bn) });
//    }

//-----------------------------------------------------------------------------
// Update editor buffer
//-----------------------------------------------------------------------------
    
// Copy text of the selected module to the editor buffer

// deprecated, see refreshInEditorBuffer method...
/*
export function refreshEditorBuffer () {
    com.mode.devlog (`refreshEditorBuffer`);
    console.log (`refreshEditorBuffer`);
    const m = st.env.getSelectedModule ();
    const xs = m.asmInfo.asmSrcText;
    document.getElementById("EditorTextArea").value = xs;
} */

/*
export function testModSet () {
    console.log ('start testModSet')
    console.log (st.env.moduleSet.show ())
    st.env.moduleSet.addModule ()
    console.log (st.env.moduleSet.show ())
    console.log ('end testModSet')
}
*/

// about S16module...
// All files associated with any of these objects must
// share the same basename.

// A module may come from either a file or the editor pane, and may contain
//   - AsmInfo: assembly language source and information collected by assembler
//   - ObjInfo: object language source and information collected by linker
//   - LinkInfo: commands for the linker

// Each module has a moduleType indicating whether it originates from
// the assembler, from reading in an object module, etc.  The type is
// determined by the user's choice of command or from the filename (if
// any) but not by parsing the contents.  For example, if the user
// Assembles a file, then it is deemed to be assembly text.

//        console.log ('moduleSet: refresh display')
//        console.log (`moduleSet.refreshDisplay: ${xs}\n`)
//        console.log ('module set generate display')
//            console.log (`i=${i} y=${y} `)
//        console.log ('module set generate display: ${xs}')

//-----------------------------------------------------------------------------
// Initialize modules
//-----------------------------------------------------------------------------

// Create and select an initial module
/* deprecated, use new ModuleSet
export function initModules () {
//    com.mode.trace = true;
com.mode.devlog ("initModules");
st.env.clearModules ();
refreshModulesList();
}
*/

//----------------------------------------------------------------------------
// Module set
//----------------------------------------------------------------------------

/*
export class ModuleSet {
    constructor () {
        const initModule = new Module ("anonymous", "", null)
        this.moduleList = null; // new 
    }
    showSelectedModuleName () {
        return this.selectedModule
            ? this.selectedModule : "No module selected";
    }
    clearModules () {
        this.modules = new Map ();
        this.anonymousCount = 0;
        this.selectedModule = null;
    }
    mkSelectModule (mname) {
        com.mode.devlog (`mkSelectModule ${mname}`);
        if (mname && this.modules.has (mname)) {
            this.selectedModule = mname;
        } else if (mname) {
            this.selectedModule = mname;
            this.modules.set (mname, new S16Module (mname));
        } else {
            this.anonymousCount++;
            const xs = `anonymous${this.anonymousCount}`;
            this.modules.set (xs, new S16Module (xs));
            this.selectedModule = xs;
        }
        const m = this.modules.get(this.selectedModule);
        return m;
    }
    closeModule (mname) {
        this.modules.delete (mname);
    }
    getSelectedModule () {
        com.mode.devlog ("getSelectedModule");
        if (env.modules.size == 0) { // no modules
//            this.anonymousCount++;
//            const xs = `anonymous${this.anonymousCount}`;
//            this.modules.set (xs, new S16Module (xs));
//            this.selectedModule = xs;
            this.selectedModule = null
        } else if (!this.selectedModule) { // nothing selected
            com.mode.devlog ("getSelectedModule, in nothing selected")
            this.selectedModule = [...this.modules.keys()][0].baseName;
        } else if (this.modules.get(this.selectedModule)) { // it exists
            com.mode.devlog ("getSelectedModule, found it")
        } else  { // it doesn't exist
            com.mode.devlog ("getSelectedModule, doesn't exist, making it")
            return this.mkModule (this.selectedModule);
        }
        return this.modules.get (this.selectedModule);
    }
}
*/

// Return a string giving a concise overfiew of Module Set
// change name to ModSetSummary
// export function envSummary () {
/*
export function modSetSummary () {
    com.mode.devlog ("Begin module set summary");
    for (const x of env.modules.keys()) {
        com.mode.devlog (`key = ${x}`);
        const m = env.modules.get(x);
        com.mode.devlog (`  bn=${m.baseName}`);
    }
    com.mode.devlog ("End module set summary");
}
*/

//        this.objText = 
//        this.asmListingText =
//        this.mdText = "Unavailable, the source must be assembled\n"

// The source text of module m is changed when a source file is read
// or the text is changed in the editor.  When this happens, the new
// source is saved in m.asmText, and a suitable message is placed in
// the object, listing, and metadata strings (m.objText,
// m.asmListingText, m.md.Text).  When the assembler is run, these
// strings are updated.

// The following information exists only after assembly, and is held in
// the asmInfo object: nAsmErrors, isExecutable

/*
// Old version
export class S16Module {
    constructor (baseName) {
        com.mode.devlog (`>>> new S16Module ${baseName}`);
// general properties of a module        
        this.baseName = baseName;
        env.modules.set (baseName, this);
         env.selectedModule = baseName
        this.selectId = `select_${baseName}`;
        this.closeId = `close_${baseName}`;
// files containing text for various stages of a module        
        this.asmFile = null; // assembly language source
        this.objFile = null; // object code
        this.omdFile = null; // object metadata
        this.lnkFile = null; // link command
        this.exeFile = null; // executable
        this.xmdFile = null; // executable metadata
        // editor text for various stages of a module
        this.edCurrentStage = StageAsm;
        this.asmEdText = "";
        this.objEdText = "";
        this.lnkEdText = "";
        this.exeEdText = "";
// containers for the stage functions        
        this.asmInfo = null;
        this.objInfo = null;
        this.linkInfo = null;
// containers for object code and metadata
        this.objMd = null;       // ObjMd created by assembler
        this.executable = null;  // ObjMd created by linker
    }
// In case a module has data for several stages, choose one to use    
    getPrimaryStage () {
        return this.asmEdText || this.asmFile ? StageAsm
            : this.objEdText || this.objFile ? StageObj
            : this.linkEdText || this.linkFile ? StageLink
            : this.exeEdText || this.exeFile ? StageExe
            : null
    } 

    // Obtain assembly language source code for assembler
    getAsmText () {
        console.log ('getAsmText')
        console.log (`This is getAsmText for ${this.baseName}`);
        com.mode.devlog (`This is getAsmText for ${this.baseName}`);
        let xs = this.asmEdText ? this.asmEdText
            : this.asmFile ? this.asmFile.text
            : "";
        console.log (`&&&&& ***** getAsmText ${this.asmEdText} ${this.asmFile}  ${this.asmFile.text} xs=${xs}`)
        com.mode.devlog (`getAsmText returning`
                         + ` basename=${this.baseName} <${xs}>`);
        return xs;
    }
    getObjText () {
        let xs = this.objEdText ? this.objEdText
            : this.objFile ? this.objFile.text
            : "";
        return xs;
    }
    getExeText () {
        let xs = this.exeEdText ? this.exeEdText
            : this.exeFile ? this.exeFile.text
            : "";
        return xs;
    }
    getLinkText () {
        let xs = this.linkEdText ? this.linkEdText
            : this.linkFile ? this.linkFile.text
            : "";
        return xs;
    }
    showShort () {
        let xs = `Module baseName=${this.baseName}\n`;
//        xs += `"stage=${this.stage.description}`;
        xs += showFileShort ("asm", this.asmFile);
        xs += showFileShort (" obj", this.objile);
        xs += showFileShort (" omd", this.omdFile);
        xs += showFileShort (" lnk", this.lnkFile);
        xs += showFileShort (" exe", this.exeFile);
        xs += showFileShort (" xmd", this.xmdFile);
        xs += this.asmInfo ? "Has assembly language code\n" : "";
        xs += this.objInfo ? "Has object code\n" : "";
        xs += this.linkInfo ? "Has linked code\n" : "";
        return xs;
    }
    showHtml () {
        let xs = "<ul>\n";
        xs += `<li> <b>`;
        xs += ((env.selectedModule == this.baseName)
               ? `<span class='SELECTEDFILE'>${this.baseName}</span>`
               : `${this.baseName}`);
        xs += `</b>\n`;
        xs += (env.selectedModule == this.baseName ? "Selected" : "" );
        xs += `<button id='${this.selectId}'>Select</button>`;
        xs += `<button id='${this.closeId}'>Close</button>`;
        xs += "<br>";
        xs += showFilePrefix (this.asmFile, "Assembly code:");
        xs += showFilePrefix (this.objFile, "Object code:");
        xs += showFilePrefix (this.exeFile, "Executable code:");
        xs += showFilePrefix (this.lnkFile, "Linker command:");
        xs += `</li>\n`;
        xs += "</ul>\n";
        return xs;
    }
}
*/
        //        return this.modules [this.selectedModuleIdx]

//        document.getElementById('EditorTextArea').value = txt;
//        st.env.moduleSet.refreshDisplay ()

/*
function handleSelect (bn) {
    console.log (`********** module ${bn} Select clicked **********`)
//    com.mode.devlog (`Select button for ${bn} invoked`);
//    let mod = st.env.mkSelectModule (bn);
//    refreshModulesList ();
}

// Handle a "Close" button click in the list of modules on the Modules
// page: close the module with basename bn.

function handleClose (bn) {
    com.mode.devlog (`Close button for ${bn} invoked`);
    st.env.closeModule (bn);
    refreshModulesList ();
}
*/

/*    show () {  // module show deprecated
        let xs = ""
//        xs += `(id=${this.modId}`
        xs += this.asmInfo.asmSrcText.split("\n")[0]
        xs += "<br>"
        xs += this.asmInfo.objectText
        xs += "<br>\n"
        return xs
        }
        */
//        const tEnd = document.createTextNode ("End of module")
//        modPara.appendChild (tEnd)
        //        xs += `<button id='${this.selectId}'>Select</button>`;
//        xs += `<button id='${this.closeId}'>Close</button>`;
//        xs += "<br>"
//        xs += "</p>\n"
//        xs += `${this.asmInfo.asmSrcText.split("\n")[0]}`
//        xs += `<p>${this.asmInfo.objectText}</p>\n`
//        console.log (`&&& ${containerElt ? 'ok' : 'nope'}`)
//        console.log (`***** setHtmlDisplay xs=${xs}*****`)
//        containerElt.innerHTML += xs

//        document.getElementById(this.selectId)
//            .addEventListener ("click", event => handleSelect(this.selectId))

//        containerElt.innerHTML += `<p>new material for ${this.modId}</p><br>`
//        let containerElt = document.getElementById("EditorTextArea")
//        let containerElt = document.querySelector ("#ModSetControls")
//        let para = document.createElement ('p')
//        para.textContent = "some new stuff"
//        containerElt.append (para)

        //        this.modId = st.env.moduleSet.modIdCounter;
        //        st.env.moduleSet.modIdCounter++;
//        this.changeAsmSrc ("; Initial asm source text\n")
//        this.setHtmlDisplay ()

/*    test2
    const containerElt = document.getElementById("ModSetControls")
    let a = st.env.moduleSet.modules[2]
    let aelt = a.displayElt
    let b = st.env.moduleSet.modules[3]
    let belt = b.displayElt
    let temp = document.createTextNode ("aardvark bat")
    containerElt.before (aelt, temp)
    //    let bb = containerElt.removeChild (b.displayElt)
    //    a.displayElt.insertBefore (b.displayElt)
    st.env.moduleSet.modules[2] = b
    st.env.moduleSet.modules[3] = a
*/

        //        maElt.before (m)
        //        containerElt.before (maElt, m)

//        this.modIdCounter = 0
//        this.addModule () 

//            let id = this.modId
            //            xs += `<b>Module ${i}.</b>`

//        let xs = this.generateDisplay ()
//        document.getElementById('FilesBody').innerHTML = xs
//        document.getElementById('ModuleSetDisplay').innerHTML = xs

// Handle the New button on Modules pane by creating a new anonymous
// module
// Deprecated, use Editor: New
/*
export function newMod () {
    const x = st.env.mkSelectModule ();
    com.mode.devlog (`newMod ${x}`)
    refreshModulesList();
}
*/

/* was test1
    let elt = document.querySelector ("#ModSetControls")
    let t1 = document.createTextNode ("This is some <b>preliminary</b> text. ")
    let t2 = document.createTextNode (" Ending with this. ")
    let middle = document.createElement ("span")
//    let i = st.env.moduleSet.modIdCounter++
    middle.innerHTML = `This number ${i} is <b>important</b> to see.  `
    let para = document.createElement ("p")
    para.appendChild (t1)
    para.appendChild (middle)
    para.appendChild (t2)
    elt.appendChild (para)
*/

//    console.log (`  ${x.modIdx} x.asmInfo.asmSrcText.split("\n")[0])`)
//    e = e.nextElementSibling
//    let e = m.firstElementChild
//    while (e) {

//    let maIdx = mIdx - 1
//    let ma = st.env.moduleSet.modules[maIdx]
//    let maElt = ma.displayElt
//    console.log (`Module Up: swap ${mIdx} ${maIdx}`)
//    m.modIdx = maIdx
//    ma.modIdx = mIdx
//    const containerElt = document.getElementById("ModSetControls")
//    containerElt.before (maElt, m)
    //    containerElt.removeChild (b.displayElt)
//    st.env.moduleSet.modules[maIdx] = m
//    st.env.moduleSet.modules[mIdx] = ma
//}
//    if (mIdx < 1) {
//        console.log (`modUp: nothing to do`)
//        return null
//    }

//        console.log ("HERE IS THE SPAN ELEMENT")
//        console.log (modDisplaySrcLineElt)
//        const xs = modDisplaySrcLineElt.textContent
//        console.log (`***** changeAsmSrc  ${xs}`)


/*
const pickerOpts = {
  types: [
    {
      description: "Sigma16 files",
        accept: { * },
    },
  ],
  excludeAcceptAllOption: true,
  multiple: false,
}

async function getTheFile() {
  // Open file picker and destructure the result the first handle
  const [fileHandle] = await window.showOpenFilePicker(pickerOpts);

  // get file contents
  const fileData = await fileHandle.getFile();
}
*/
