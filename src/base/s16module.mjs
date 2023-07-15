// Sigma16: module.mjs
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
// module.mjs defines the representation of source and object modules for the gui
//-----------------------------------------------------------------------------

import * as com from "./common.mjs";
import * as st  from "./state.mjs";

//----------------------------------------------------------------------------
// Sigma16 module set
//----------------------------------------------------------------------------

// There is one ModuleSet object that contains all the extant
// Sigma16modules.  Usage example:
//   st.env.moduleSet.refreshModulesList ()
//   st.env.moduleSet.getSelectedModule ()

export class ModuleSet {
    constructor () {
        console.log ('doing new ModuleSet')
        this.modules = []
        this.addModule ()
    }
    addModule () {
        const m = new Sigma16Module ()
        this.modules.push (m)
        this.selectedModuleIdx = this.modules.length - 1
        return this.modules [this.selectedModuleIdx]
    }
    getSelectedModule () {
        return this.modules [this.selectedModuleIdx]
    }
    refreshDisplay () {
        let xs = "";
        xs += `<p>There are ${this.modules.length} modules</p>\n`
        document.getElementById('FilesBody').innerHTML = xs;
    }
}

/*
export function testModSet () {
    console.log ('start testModSet')
    console.log (st.env.moduleSet.show ())
    st.env.moduleSet.addModule ()
    console.log (st.env.moduleSet.show ())
    console.log ('end testModSet')
}
*/

//----------------------------------------------------------------------------
// Sigma16 module
//----------------------------------------------------------------------------

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

// An Sigma16Module is a container for all the data related to a
// specific source module.

export class Sigma16Module {
    constructor () {
        this.baseName = "ModBasename"
        this.fileInfo = null // can hold instance of FileInfo
        this.asmInfo = null // can hold instance of asm.AsmInfo
        this.isMain = false
        this.asmText = "; initial asm text\n"
        this.objText = "not yet assembled"
        this.asmListingText = "not yet assembled"
        this.mdText = "not yet assembled"
    }
    getAsmText () {
        return this.asmText
    }
}

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

//-------------------------------------------------------------------------
// S16Module
//-------------------------------------------------------------------------

export class Executable {
    constructor (code, metadata) {
        this.code = code
        this.metadata = metadata
        com.mode.devlog (`new executable: ${this.showShort ()}`)
    }
    showShort () {
        let xs = `Executable: ${this.code.length} lines object code`
        xs += this.metadata ? `${this.metadata.length} lines metadata`
            : `no metadata`
        return xs
    }
}


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


//-------------------------------------------------------------------------
// Container for object code and metadata
//-------------------------------------------------------------------------

// const emptyExe = {objectCode : "", metadata : null};
const emptyExe = new Executable ("no object code", null);

// The assembler produces both an object text and a metadata text.
// For storage in a file, they are represnted as strings that are
// stored in an ObjMd container.  For use during emulation, they are
// stored in an AdrSrcMap object.  An ObjMd is used to serialize or
// populate an AdrSrcMap.

export class ObjMd {
    constructor (baseName, objText, mdText) {
        this.baseName = baseName;
        this.objText = objText;
        com.mode.devlog (this.objText);
        this.mdText = mdText;
    }
    hasObjectCode () {
        return this.objText ? true : false
    }
    showShort () {
        let xs = `Object/metadata (${this.baseName}): `
            + `${this.objText.split("\n").length} lines of object text,`
            + ` ${this.mdText.split("\n").length} lines of metadata`;
        return xs;
    }
}

//-------------------------------------------------------------------------
// Metadata
//-------------------------------------------------------------------------

// The emulator tries to display the source code line corresponding to
// the instruction currently executing.  The information it needs to
// do this is called Metadata.  This is optional: the emulator needs
// only the object code to run a machine language program.  If the
// optional Metadata is present, the emulator can also show the source
// code corresponding to the current and next instruction.

const eltsPerLineLimit = 4; // how many numbers in the mapping per line

export class Metadata {
    constructor () {
        this.clear ();
    }
    clear () {
        this.pairs = [];   // list of (a,i) pairs for serializing
        this.mapArr = [];  // mapArr[a] = i
        this.listingText = [];
        this.listingPlain = [];   // source lines
        this.listingDec = []; // src lines decorated with html span elements
        this.mdText = null;
        this.adrOffset = 0;
        this.srcOffset = 0; // convert a->i to a->i+srcLineOffset
    }
    addPairs (ps) {
        for (let p of ps) {
            this.mapArr [p.address] = p.index
            this.pairs.push (p)
        }
    }
    translateMap (adrOffset, srcOffset) {
        this.adrOffset = adrOffset
        this.srcOffset = srcOffset
        let xs = []
        this.mapArr = []
        for (const x of this.pairs) {
            const p = {address: x.address + adrOffset,
                       index: x.index + srcOffset}
            xs.push (p)
            this.mapArr [x.address + adrOffset] = x.index + srcOffset
        }
        this.pairs = xs
    }
    addMappingSrc (a, i, srcText, srcPlain, srcDec) { // add a->i plus src
        const p = {address: a, index: i}
        this.pairs.push(p)
        this.mapArr[a] = i;
        this.listingText[i] = srcText;
        this.listingPlain[i] = srcPlain;
        this.listingDec[i] = srcDec;
    }
    addMapping (a, i) { // add new mapping a->i
        const p = {address: a, index: i}
        this.pairs.push (p)
        this.mapArr[a] = i;
    }
    pushSrc (srcText, srcPlain, srcDec) { // add src line in three forms
        this.listingText.push (srcText);
        this.listingPlain.push (srcPlain);
        this.listingDec.push (srcDec);
    }
    unshiftSrc (srcText, srcPlain, srcDec) { // add src line at start
        this.listingText.unshift (srcText);
        this.listingPlain.unshift (srcPlain);
        this.listingDec.unshift (srcDec);
    }
    addSrc (i, srcText, srcPlain, srcDec) { // add src line in three forms
        this.listingText[i] = srcText;
        this.listingPlain[i] = srcPlain;
        this.listingDec[i] = srcDec;
    }
    getSrcIdx (a) { // find source line index corresponding to address a
        const i = this.mapArr[a];
        return i ? i : 0;
    }
    getSrcText (a) { // return text source line corresponding to address a
        const x = this.listingText[this.getSrcIdx(a)];
        return x ? x : `no text src for ${a}`
    }
    getSrcPlain (a) { // return plain source line corresponding to address a
        const x = this.listingPlain[this.getSrcIdx(a)];
        return x ? x : `no plain src for ${a}`
    }
    getSrcDec (a) { // return decorated src line corresponding to address a
        const x = this.listingDec[this.getSrcIdx(a)];
        return x ? x : `no decorated src for ${a}`
    }
    getMdText () {
        if (!this.mdText) { this.mdText = this.toText () }
        return this.mdText
    }
    addSrcLines (xs) {
        for (let i = 0; i < xs.length; i += 3) {
            this.listingText.push (xs[i]);
            this.listingPlain.push (xs[i+1]);
            this.listingDec.push (xs[i+2]);
        }
    }
    setMdText (xs) {
        this.mdText = xs;
    }
    fromText (x) { // parse text and populate the object
        this.clear ();
        this.mdText = x;
        const xs = x.split ("\n");
        let ns = [];
        let i = 0;
        while (i < xs.length && xs[i].substring(0,6) != "source" ) {
            let ys = xs[i].split(",");
            ns = ns.concat (ys.map ((q) => parseInt(q)));
            i++;
        }
        let j = 0;
        while (j < ns.length) {
            let a = ns[j] ? ns[j] : 0;
            let idx = ns[j+1] ? ns[j+1] : 0;
            this.addMapping (a, idx);
            j += 2;
        }
        i++; // skip "source"
        j = 0;
        while (i < xs.length) {
            if (xs[i] != "") {
                this.listingText[j]  = xs[i] ? xs[i] : "";
                this.listingPlain[j] = xs[i+1] ? xs[i+1] : "";
                this.listingDec[j]   = xs[i+2] ? xs[i+2] : "";
                j += 1;
                i += 3;
            } else {
                i++; //skip empty line
            }
        }
    }
    mapToTexts () {  // convert map to list of lines of text
        let xs = []; // flatten the pairs
        for (const p of this.pairs) {
            xs.push (p.address, p.index);
        }
        let ys = []; // list of length-limited lists
        while (xs.length > 0) {
            ys.push (xs.splice(0,eltsPerLineLimit))
        }
        let zs = [] // list of strings showing the map
        for (const y of ys) {
            zs.push (y.toString())
        }
        return zs
    }
    getSrcLines () {
        let xs = [];
        for (let i = 0; i < this.listingPlain.length; i++) {
            xs.push (this.listingText[i]);
            xs.push (this.listingPlain[i]);
            xs.push (this.listingDec[i]);
        }
        return xs;
    }
    getPlainLines () {
        let xs = [];
        for (let i = 0; i < this.listingPlain.length; i++) {
            xs.push (this.listingPlain[i]);
        }
        return xs;
    }
    toText () { // convert contents of object to text
        let xs = this.mapToTexts ()
        xs.push ("source")
        xs = xs.concat (this.getSrcLines ())
        return xs.join ("\n")
    }
}


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
export function modSetSummary () {
    com.mode.devlog ("Begin module set summary");
    for (const x of env.modules.keys()) {
        com.mode.devlog (`key = ${x}`);
        const m = env.modules.get(x);
        com.mode.devlog (`  bn=${m.baseName}`);
    }
    com.mode.devlog ("End module set summary");
}

//----------------------------------------------------------------------------
// Files
//----------------------------------------------------------------------------

// Interface
//   Modules: Choose Files button -- "change" event calls handleSelectedFiles
//   Modules: Refresh button -- refreshModulesList

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

// Handle the New button on Modules pane by creating a new anonymous
// module

export function newMod () {
    const x = st.env.mkSelectModule ();
    com.mode.devlog (`newMod ${x}`)
    refreshModulesList();
}

// Handle a "Select" button click in the list of modules on the
// Modules page: select the module with basename bn.

function handleSelect (bn) {
    com.mode.devlog (`Select button for ${bn} invoked`);
    let mod = st.env.mkSelectModule (bn);
    refreshModulesList ();
}

// Handle a "Close" button click in the list of modules on the Modules
// page: close the module with basename bn.

function handleClose (bn) {
    com.mode.devlog (`Close button for ${bn} invoked`);
    st.env.closeModule (bn);
    refreshModulesList ();
}

//-----------------------------------------------------------------------------
// Initialize modules
//-----------------------------------------------------------------------------

// Create and select an initial module

export function initModules () {
//    com.mode.trace = true;
    com.mode.devlog ("initModules");
    st.env.clearModules ();
    refreshModulesList();
}

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

export function refreshEditorBuffer () {
    com.mode.devlog (`refreshEditorBuffer`);
    console.log (`refreshEditorBuffer`);
    const mod = st.env.getSelectedModule ();
    const xs = mod.asmInfo ? mod.asmInfo.text : "";
    document.getElementById("EditorTextArea").value = xs;
}
