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
        this.emulatorState = null;
        this.linkerState = null;
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
            console.log ("getSelectedModule, in nothing selected")
            this.selectedModule = [...this.modules.keys()][0].baseName;
        } else if (this.modules.get(this.selectedModule)) { // it exists
            console.log ("getSelectedModule, found it")
        } else  { // it doesn't exist
            console.log ("getSelectedModule, doesn't exist, making it")
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
        this.asmEdText = null;
        this.objEdText = null;
        this.lnkEdText = null;
        this.exeEdText = null;
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
        console.log (`This is getAsmText for ${this.baseName}`);
        let xs = this.asmEdText ? this.asmEdText
            : this.asmFile ? this.asmFile.text
            : "";
        console.log (`getAsmText returning basename=${this.baseName} <${xs}>`);
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
    constructor (baseName, objText, mdText) {
        this.baseName = baseName;
        this.objText = objText;
        console.log (this.objText);
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


//-----------------------------------------------------------------------------
// Metadata
//-----------------------------------------------------------------------------

// The emulator tries to display the source code line corresponding to
// the instruction currently executing.  The information it needs to
// do this is called Metadata.  This is optional: the emulator needs
// only the object code to run a machine language program.  If the
// optional Metadata is present, the emulator can also show the source
// code corresponding to the current and next instruction.

// const eltsPerLineLimit = 16; // how many numbers in the mapping per line    }
const eltsPerLineLimit = 4; // how many numbers in the mapping per line    }

export class Metadata {
    constructor () {
        this.clear ();
    }
    clear () {
        this.pairs = [];   // list of (a,i) pairs for serializing
        this.mapArr = [];  // mapArr[a] = i
        this.listingText = [];
        this.listingPlain = [];   // source lines
        this.listingDec = [];     // source lines decorated with html span elements
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
            const p = {address: x.address + adrOffset, index: x.index + srcOffset}
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
    pushSrc (srcText, srcPlain, srcDec) { // add src line in three forms at end
        this.listingText.push (srcText);
        this.listingPlain.push (srcPlain);
        this.listingDec.push (srcDec);
    }
    unshiftSrc (srcText, srcPlain, srcDec) { // add src line at start
        this.listingText.unshift (srcText);
        this.listingPlain.unshift (srcPlain);
        this.listingDec.unshift (srcDec);
    }
    addSrc (i, srcText, srcPlain, srcDec) { // add src line in three forms at i
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
    getSrcDec (a) { // return decorated source line corresponding to address a
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


//-------------------------------------------------------------------------------
// Counter
//-------------------------------------------------------------------------------

export function getCount () {
    console.log ("entering getCount");    
    let xmlHttp = new XMLHttpRequest();
    xmlHttp.open('GET', 'https://hitcounter.pythonanywhere.com/count', false);
    xmlHttp.send(null);
    let count = xmlHttp.responseText;
    console.log (`getCount finishing: <${count}>`);
}

/*
s16module.mjs:80 [Deprecation] Synchronous XMLHttpRequest on the main
thread is deprecated because of its detrimental effects to the end
user's experience. For more help, check https://xhr.spec.whatwg.org/.
*/


//-------------------------------------------------------------------------------
// Emulator thread
//-------------------------------------------------------------------------------


// Local state for main gui thread

export let foo = 300 // dummy state belonging to main thread
export let foobar = 0

// Constant parameters

// Sizes of memory blocks (words)
export const EmSCBsize        =  16
export const EmRegFileSize    =  16
export const EmSysRegSize     =  16
export const EmRegBlockSize   =  EmRegFileSize + EmSysRegSize
export const EmMemSize        =  65536

// Size of the shared memory (words and bytes)
export const EmStateSizeWord  =  EmSCBsize + EmRegBlockSize + EmMemSize
export const EmStateSizeByte  =  2 * EmStateSizeWord

// Offsets of memory blocks (words)
export const EmSCBOffset      =  0
export const EmRegBlockOffset =  EmSCBsize
export const EmMemOffset      =  EmRegBlockOffset + EmRegBlockSize

// Codes for system control block

// The system control block is stored in shared memory and contains
// information that pertains to the entire system, including all
// running emulators.  Any information that is specific to a
// particular emulator (either the main gui thread or a worker thread)
// is kept in the EmulatorState belonging to that thread.

// SCB flag indices
export const SCB_nInstrExecutedMSB =  0  // count instructions executed
export const SCB_nInstrExecutedLSB =  1  // count instructions executed
export const SCB_status            =  2  // condition of the entire system
export const SCB_nInstrExecuted    =  3  // count instructions executed
export const SCB_cur_instr_addr    =  4  // address of instruction executing now
export const SCB_next_instr_addr   =  5  // address of next instruction
export const SCB_emwt_run_mode     =  6
export const SCB_emwt_trap         =  7
export const SCB_pause_request     =  8  // pause request is pending


// SCB_status specifies the condition of the entire system
export const SCB_reset             = 0 // after initialization or Reset command
export const SCB_ready             = 1 // after boot
export const SCB_running_gui       = 2 // running in main gui thread
export const SCB_running_emwt      = 3 // running in emulator worker thread
export const SCB_paused            = 4 // after Pause command
export const SCB_break             = 5 // after Pause command
export const SCB_halted            = 6 // after trap 0
export const SCB_blocked           = 7 // during blocking read trap
export const SCB_relinquish        = 8 // emwt relinquished control temporarily

export function showSCBstatus (es) {
    switch (readSCB (es, SCB_status)) {
    case 0: return "Reset"
    case 1: return "Ready"
    case 2: return "Running"  // run in gui
    case 3: return "Running"  // run in emwt
    case 4: return "Paused"
    case 5: return "Break"
    case 6: return "Halted"
    case 7: return "Blocked"
    case 8: return "Relinquish"  // relinquish
    default: ""
    }
}

export function readNinstrExecuted (es) {
    const upper = readSCB (es, SCB_nInstrExecutedMSB)
    const lower = readSCB (es, SCB_nInstrExecutedMSB)
    const n = upper << 16 | lower
    return n
}

export function writeNinstrExecuted (es, n) {
    const upper = n >>> 16
    const lower = n & 0x0000ffff
    writeSCB (es, SCB_nInstrExecutedMSB, upper)
    writeSCB (es, SCB_nInstrExecutedLSB, lower)
}

export function resetSCB (es) {
    writeSCB (es, SCB_nInstrExecutedMSB, 0)
    writeSCB (es, SCB_nInstrExecutedLSB, 0)
    writeSCB (es, SCB_status, SCB_reset)
    writeSCB (es, SCB_nInstrExecuted, 0)
    writeSCB (es, SCB_cur_instr_addr, 0)
    writeSCB (es, SCB_next_instr_addr, 0)
    writeSCB (es, SCB_emwt_run_mode, 0)
    writeSCB (es, SCB_emwt_trap, 0)
    writeSCB (es, SCB_pause_request, 0)
}

export function writeSCB (es, code, x) {
    let i = EmSCBOffset + code
    es.shm [i] = x
}

export function readSCB (es, code) {
    let i = EmSCBOffset + code
    let x = es.shm [i]
//    console.log (`st.readSCB code=${code} i=${i} x=${x}`)
    return x
}
export function incrSCB (es, code) {
    let i = EmSCBOffset + code
    let x = es.shm[i]
    x++
    es.shm[i] = x
    return x
}


// System state vector

export const sysStateBuf = new SharedArrayBuffer (EmStateSizeByte)
export const sysStateVec = new Uint16Array (sysStateBuf)

export class emflags {
    constructor (x) {
        this.whoami = x
    }
}
