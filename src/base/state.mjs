// Sigma16: state.mjs
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
// state.mjs defines global state for the system, IDE, modules, and
// emulator
//-----------------------------------------------------------------------------

// The main components of the program avoid using global variables;
// instead the necessary state is organized into records and passed as
// needed to the functions.  This module defines those records.  The
// gui operations, defined in Sigma16.html, call the emulator's
// interface functios with the state, for example boot(emulatorState).

import * as com from './common.mjs';
import * as arch from './architecture.mjs';

//-----------------------------------------------------------------------------
// Stages
//-----------------------------------------------------------------------------

export const StageAsm = Symbol ("Asm");   // .asm.   assembly language source
export const StageObj = Symbol ("Obj");   // .obj.   object code
export const StageOmd = Symbol ("Omd");   // .omd.   metadata for obj
export const StageLnk = Symbol ("Lnk");   // .lnk.   link command
export const StageExe = Symbol ("Exe");   //.xmd.    metadata for exe
export const StageXmd = Symbol ("Xmd");   //.xmd.    metadata for exe

// Convert a string from filename into symbolic stage

export function getStageSym (xs) {
    return xs == "asm" ? StageAsm
        : xs == "obj" ? StageObj
        : xs == "omd" ? StageOmd
        : xs == "lnk" ? StageLnk
        : xs == "exe" ? StageExe
        : xs == "xmd" ? StageXmd
        : null
}

//-----------------------------------------------------------------------------
// Representation of system state
//-----------------------------------------------------------------------------

// The system state contains a map from a base name foo to an
// S16Module object, as well as the emulator state.

export class SystemState {
    constructor () {
        this.modules = new Map ();
        this.selectedModule = null;
        this.anonymousCount = 0;
        this.emState = null;
    }
    showSelectedModuleName () {
        return this.selectedModule ? this.selectedModule : "No module selected";
    }
    clearModules () {
        this.modules = new Map ();
        this.anonymousCount = 0;
        this.selectedModule = null;
    }
    mkSelectModule (mname) {
        console.log (`mkSelectModule ${mname}`);
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
        console.log ("getSelectedModule");
        if (env.modules.size == 0) { // no modules
            this.anonymousCount++;
            const xs = `anonymous${this.anonymousCount}`;
            this.modules.set (xs, new S16Module (xs));
            this.selectedModule = xs;
        } else if (!this.selectedModule) { // nothing selected
            this.selectedModule = [...this.modules.keys()][0].baseName;
        } else if (this.modules.get(this.selectedModule)) { // it exists
        } else  { // it doesn't exist
            return this.mkModule (this.selectedModule);
        }
        return this.modules.get (this.selectedModule);
    }
}

// Return a string giving a concise overfiew of system state

export function envSummary () {
    console.log ("Begin envSummary");
    for (const x of env.modules.keys()) {
        console.log (`key = ${x}`);
        const m = env.modules.get(x);
        console.log (`  bn=${m.baseName}`);
    }
    console.log ("End envSummary");
}

//-----------------------------------------------------------------------------
// Global state variable
//-----------------------------------------------------------------------------

// The environment is a global variable that contains all the system
// state.

export const env = new SystemState ();

//-----------------------------------------------------------------------------
// S16Module
//-----------------------------------------------------------------------------

export class Executable {
    constructor (code, metadata) {
        this.code = code;
        this.metadata = metadata;
        console.log (`*********** new Executable ${this.code}`);
        console.log (this.showShort ());
    }
    showShort () {
        let xs = `Executable: ${this.code.length} lines object code`;
        xs += this.metadata ? `${this.metadata.length} lines metadata`
            : `no metadata`;
        return xs;
    }
}


// An S16Module is a container for all the files and objects that
// share the same basename.

export class S16Module {
    constructor (baseName) {
        console.log (`>>> new S16Module ${baseName}`);
// general properties of a module        
        this.baseName = baseName;
        env.modules.set (baseName, this);
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
        this.asmEdText = null;
        this.objEdText = null;
        this.lnkEdText = null;
        this.exeEdText = null;
// containers for the stage functions        
        this.asmInfo = null;
        this.objInfo = null;
        this.linkInfo = null;
        this.executable = null;
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
        console.log (`This is getAsmText for ${this.baseName}`);
        let xs = this.asmEdText ? this.asmEdText
            : this.asmFile ? this.asmFile.text
            : "";
        console.log (`getAsmText is returning ${xs}`);
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
        + xs.split("\n").slice(0,4).join("<br>");
        + "<div>\n";
}


//-----------------------------------------------------------------------------
// Container for object code and metadata
//-----------------------------------------------------------------------------

// const emptyExe = {objectCode : "", metadata : null};
const emptyExe = new Executable ("no object code", null);


// The assembler produces both an object text and a metadata text.
// For storage in a file, they are represnted as strings that are
// stored in an ObjMd container.  For use during emulation, they are
// stored in an AdrSrcMap object.  An ObjMd is used to serialize or
// populate an AdrSrcMap.

export class ObjMd {
    constructor (objText, mdText) {
        this.objText = objText;
        this.mdText = mdText;
    }
}


//-----------------------------------------------------------------------------
// Metadata
//-----------------------------------------------------------------------------

// The emulator tries to display the source code line corresponding to
// the instruction currently executing.  The information it needs to
// do this is called Metadata.  This is optional: the emulator needs
// only the object code to run a machine language program.  If the
// optional Metadata is present, the emulator can also show the source
// code corresponding to the current and next instruction.

// NOTES during conversion...

// In assembler:
// see AsmInfo, emitMetadata, generateObjectWord
//    ma.asArrMap[a] = s.lineNumber; // ASMAP in generateObjectWord
//    xs = [...ma.asArrMap]; in emitMetadata
//    xs = [...ma.asArrMap]; // ASMAP in emitMetadata
//    ma.metadata.push (`asmap ${xs.length}`); // ASMAP in emitMetadata

// In emulator:
// See obtainObjectCode, showAsMap, highlightListingAfterInstr
// Examples of usage...
//    es.curInstrLineNo = es.asArrMap[es.curInstrAddr] + listingLineInitialOffset;
//    es.nextInstrLineNo = es.asArrMap[es.nextInstrAddr] + listingLineInitialOffset;
//        asArrMap : [],   in emulatorState

const eltsPerLineLimit = 16; // how many numbers in the mapping per line    }

export class Metadata {
    constructor () {
        this.clear ();
    }
    clear () {
        this.pairs = [];   // list of (a,i) pairs for serializing
        this.mapArr = [];  // mapArr[a] = i
        this.plain = [];   // source lines
        this.dec = [];     // source lines decorated with html span elements
    }
    addMappingSrc (a, i, srcPlain, srcDec) { // add a new entry with source
        this.pairs.push({a,i});
        this.mapArr[a] = i;
        this.plain[i] = srcPlain;
        this.dec[i] = srcDec;
    }
    addMapping (a, i) { // add new mapping
        this.pairs.push({a,i});
        this.mapArr[a] = i;
    }
    addSrc (i, srcPlain, srcDec) { // insert source at given index
        this.plain[i] = srcPlain;
        this.dec[i] = srcDec;
    }
    getSrcIdx (a) { // find source line index corresponding to address a
        const i = this.mapArr[a];
        return i ? i : 0;
    }
    getSrcPlain (a) { // return plain source line corresponding to address a
        const x = this.plain[this.getSrcIdx(a)];
        return x ? x : `no plain src for ${a}`
    }
    getSrcDec (a) { // return decorated source line corresponding to address a
        const x = this.dec[this.getSrcIdx(a)];
        return x ? x : `no decorated src for ${a}`
    }
    fromText (x) { // parse text and populate the object
        this.clear ();
        const xs = x.split ("\n");
        let ns = [];
        let i = 0;
        while (i < xs.length && xs[i].substring(0,6) != "source" ) {
            console.log (`from loop 1 i=${i} ${xs[i]}`);
            let ys = xs[i].split(",");
            ns.push(ys);
            console.log (`from loop 1 ns = ${ns}`);
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
            console.log (`from loop 3 i=${i} j=${j} xs[i]=${xs[i]}`);
            this.plain[j] = xs[i] ? xs[i] : "";
            this.dec[j]   = xs[i+1] ? xs[i+1] : "";
            i++;
            j++;
        }
    }
    toText () { // convert contents of object to text
        let ys = [];
        for (const {a,i} of this.pairs) {
            ys.push(a,i);
            console.log (`in loop 1, a=${a} i=${i}`);
            console.log (`in loop 1, ys = ${ys}`);
        }
        let xs = "";
        while (ys.length > 0) {
            xs += ys.splice(0,eltsPerLineLimit) + "\n";
        }
        xs += "source\n";
        for (const {a,i} of this.pairs) {
            xs += this.getSrcPlain (a) + "\n";
            xs += this.getSrcDec (a) + "\n";
        }
        return xs;
    }
}

