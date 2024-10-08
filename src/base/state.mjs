// Sigma16: state.mjs
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

//-------------------------------------------------------------------------
// state.mjs defines global state for the system, IDE, modules, and
// emulator, including key data structures.
//-------------------------------------------------------------------------

// The main components of the program avoid using global variables;
// instead the necessary state is organized into records and passed as
// needed to the functions.  This module defines those records.  The
// gui operations, defined in Sigma16.html, call the emulator's
// interface functios with the state, for example boot(emulatorState).

import * as com from './common.mjs'
import * as arith from './arithmetic.mjs';
import * as arch from './architecture.mjs'
import * as ab from './arrbuf.mjs'

//-------------------------------------------------------------------------
// Stages
//-------------------------------------------------------------------------

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

//-------------------------------------------------------------------------
// System state
//-------------------------------------------------------------------------

// The system state contains separate components for the ModuleSet,
// the emulator state, and the linker state.

export class SystemState {
    constructor () {
        this.moduleSet = null
        this.emulatorState = null;
        this.linkerState = null;
    }
}

//-------------------------------------------------------------------------
// Global state variable
//-------------------------------------------------------------------------

// The environment is a global variable that contains all the system
// state.

export const env = new SystemState ();

//-------------------------------------------------------------------------
// Module keys
//-------------------------------------------------------------------------

// Each module has a unique key.  Generate a fresh module key when a
// module is created.  The key will never be reused.

export let nextModKey = 0
export function newModKey () {
    let i = nextModKey
    nextModKey++
    return i
}

//-------------------------------------------------------------------------
// Sigma16 module
//-------------------------------------------------------------------------

// Container for all the data related to a specific module.

// If assembly source exists, then m.asmSrcOrigin is either "file" or
// "editor".  If it is null, there is no assembly source.

// If object exists, then m.objOrigin is either "assembler", "file", or
// "editor".  If it is null, there is no object.

// If executable exists, then exeOrigin is either "linker", "file", or
// "editor". If it is null, there is no executable.

// When text is changed by reading a file or modifying editor buffer,
// one of the following setXxxCode functions is called.  This will
// update the necessary fields in the module, and will also set any
// stale fields to null.

// makeStageDisplay sets up the DOM elements to display a stage, and
// returns functions to update those elements
//   page is an element that will hold this display
//   stage is one of Assembly, Object, Executable
//   Returns functions to set the origin and the code

function makeStageDisplay (page,stage) {
//    console.log (`makeStageDisplay ${stage}`)
    const displaySpanElt = document.createElement ("span");
    displaySpanElt.appendChild (document.createTextNode (`${stage} code: `));
    const displayOriginElt = document.createTextNode ("none");
    const setOrigin = (x) => { displayOriginElt.textContent = x;};
    displaySpanElt.appendChild (displayOriginElt);
    page.appendChild (displaySpanElt);
    const displayCodeElt = document.createElement ("div");
    const displayCodeTextElt = document.createElement ("div");
    const setCode = (x) => {  // f extracts some or all of x to display
//        console.log (`makeStageDisplay setCode=${x}`)
        displayCodeTextElt.innerHTML =
            showInitialLines (DisplayInitLinesLength, x);
    };
    displayCodeElt.appendChild (displayCodeTextElt)
    page.appendChild (displayCodeElt);
    return {setOrigin, setCode}
}

export const DisplayInitLinesLength = 3;

// return the first few lines of a string in html
export function showInitialLines (k,txt) {
    const xs = txt.split ("\n");
    let ys = xs.slice(0,k);
    if (xs.length > k) {ys.push (`... ${xs.length-k} more lines ...`)};
    return "<pre class='LiteralCode'>" + ys.join("<br>") + "</pre>";
}

// Module name and text are supplied when module is created; these can
// be changed later
export class Sigma16Module {
    constructor (name,text) {
        // identify the module
        this.modKey = newModKey () // Persistent and unique gensym key
        this.modIdx = env.moduleSet.modules.length // Transient array index
        // Transient array index
        this.moduleName = name // from filename or module stmt
        // possible file associated with module
        this.fileHandle = null
        this.filename = "(no file)"
//        this.baseName = "anonymous"
        this.fileRecord = null
        this.fileInfo = null // can hold instance of FileInfo
        this.fileType = "none" // {none, asm, obj, exe}
        this.fileText = ""

        // assembly source code
        this.asmSrcCodeOrigin = "none"; // {none, file, example, editor}
        //        this.currentAsmSrc = null; // source code (possibly edited)
        this.currentAsmSrc = text; // source code (possibly edited)
        this.asmSrcLines = []
        this.savedAsmSrc = null; // code as last saved/read to/from file
        //        this.asmInfo = new AsmInfo (this); // filled in by assembler
        this.asmInfo = null // asmInfo created and returned by assembler
        // object code
        this.objCodeOrigin = "none"; // {none, file, assembler}
        this.objMd = null; // object holding object code and metadata
        // executable code
        this.exeCodeOrigin = "none"; // {none, file, linker}
        this.exeObjMd = null; // executable for linked main module
    }
    ident () { // shortcut for identifying module
        return `module ${this.modKey}`;
    }
    // Whenever addModule is called, and running in browser, then call
    // setModuleDisplay.  But don't call setModuleDisplay if running
    // in node.  setModuleDisplay refers to the document, which exists
    // in the browser but not in node.
    setModuleDisplay () {
        // display module name (can be changed by assembler)
        this.displayModuleNameElt =
            document.createTextNode (this.moduleName);
        this.displayModNameb = document.createElement ("b")
        this.displayModNameb.appendChild (this.displayModuleNameElt)
        this.displayModNameb.appendChild (document.createTextNode (" "));
        const setModName = (x) => {
//            console.log (`Setting module name := ${x}`);
            this.moduleName = x;
            displayModuleNameElt.textContent = x;
        };
        // displayModIdLine is span element for module name, id, buttons
        this.displayModIdLine = document.createElement ("span")
        this.displayModIdLine.appendChild (document.createTextNode
                                           ("Module "));
        this.displayModIdLine.appendChild (this.displayModNameb)

        // modPara is a p element holding entire module display
        this.modPara = document.createElement ("p")
        this.modPara.appendChild (this.displayModIdLine)

        // selectElt indicates whether the module is selected
        this.selectElt = document.createElement ("span")
        this.selectEltText = document.createTextNode ("selected")
        this.selectElt.setAttribute ("id", `MODULE-${this.modKey}-SEL-FLAG`)
        this.selectElt.appendChild (this.selectEltText)
        this.modPara.appendChild (this.selectElt)

        // select button
        this.bSelect = document.createElement ("button")
        this.bSelect.textContent = "Select"
        this.selectId = `SELECT-${this.modKey}`
        this.bSelect.addEventListener (
            "click", event => handleSelect(this))
        this.modPara.appendChild (this.bSelect)

        // up button
        this.bUp = document.createElement ("button")
        this.bUp.textContent = "Up"
        this.upId = `UP-${this.modKey}`
        this.bUp.addEventListener (
            "click", event => handleModUp (this))
        this.modPara.appendChild (this.bUp)

        // refresh button 2024-09-07
        this.bRefresh = document.createElement ("button")
        this.bRefresh.textContent = "Refresh"
        this.refresuId = `REFRESH-${this.modKey}`
        this.bRefresh.addEventListener (
            "click", event => handleModRefresh (this))
        this.modPara.appendChild (this.bRefresh)

        // close button
        this.bClose = document.createElement ("button")
        this.bClose.textContent = "Close"
        this.closeId = `CLOSE-${this.modKey}`
        this.bClose.addEventListener (
            "click", event => handleClose (this))
        this.modPara.appendChild (this.bClose)

        this.spanSrc = document.createElement ("span")
        this.spanSrc.setAttribute ("id", `MODULE-${this.modKey}-SRCLINE`)
        this.spanSrc.appendChild (document.createElement ("br"))
        this.modPara.appendChild (this.spanSrc)
        // Display asm, obj, exe code status
        this.asmDisplay = makeStageDisplay (this.modPara, "Assembly");
        this.objDisplay = makeStageDisplay (this.modPara, "Object");
        this.exeDisplay = makeStageDisplay (this.modPara, "Executable");
        // complete container for module display
        this.containerElt = document.getElementById("ModSetControls")
        this.containerElt.appendChild (this.modPara)
        this.displayElt = this.modPara
    }
    setModuleName (txt) {
//        console.log (`setModuleName ${txt}`)
        this.moduleName = txt
        if (com.runningBrowser) {
            this.displayModuleNameElt.textContent = txt
        }
//        console.log (`${this.ident} setting name=${txt}`)
    }
    staleAsmCode () { // asm src code is stale
//        console.log (`${ident} asm src code is stale`)
        if (com.runningBrowser) {
            this.asmDisplay.setOrigin ("none")
            this.asmDisplay.setCode ("")
        }
    }
    staleObjCode () { // obj code is stale
//        console.log (`${this.ident} obj code is stale`)
        if (com.runningBrowser) {
            this.objDisplay.setOrigin ("none")
            this.objDisplay.setCode ("")
        }
    }
    staleExeCode () { // exe code is stale
//        console.log (`${this.ident} exe code is stale`)
        if (com.runningBrowser) {
            this.exeDisplay.setOrigin ("none")
            this.exeDisplay.setCode ("")
        }
    }
    setAsmCode (txt, origin) {
//        console.log (`${this.ident} setAsmCode ${txt.substring(0,100)}`)
        this.asmSrcCodeOrigin = origin
        this.currentAsmSrc = txt
        this.asmSrcLines = txt.split ("\n")
        if (com.runningBrowser) {
            this.staleObjCode ()
            this.staleExeCode ()
            this.asmDisplay.setCode (txt)
            this.asmDisplay.setOrigin (origin)
        }
    }
    setObjCode (txt) {
//        console.log (`${this.ident} setObjCode ${txt.substring(0,100)}`)
        this.asmObjCodeOrigin = origin
        this.currentObjSrc = txt
        if (com.runningBrowser) {
            this.staleAsmCode ()
            this.staleExeCode ()
            this.objDisplay.setCode (txt)
            this.objDisplay.setOrigin (origin)
        }
    }
    setExeCode (txt) {
//        console.log (`${this.ident} setExeCode ${txt.substring(0,100)}`)
        this.asmExeCodeOrigin = origin
        this.currentExeSrc = txt
        if (com.runningBrowser) {
            this.exeDisplay.setCode (txt)
            this.exeDisplay.setOrigin (origin)
            this.staleAsmCode ()
            this.staleObjCode ()
        }
    }
    hasMetadata () { // deprecated
        return this.mdText !== ""
    }
    changeAsmSrc (txt) { // new text may not be saved in file
//        console.log (`Module ${this.modKey} changeAsmSrc`)
        this.currentAsmSrc = txt
        if (com.runningBrowser) {
            document.getElementById("EditorTextArea").value = txt
        }
        //        this.setAsmCode (txt.split("\n")[0])
        this.setAsmCode (txt)
    }
    changeSavedAsmSrc (txt) { // new text is saved in file
        this.savedAsmSrc = txt
        this.changeAsmSrc (txt)
    }
    setSelected (b) {
        let selTxt = b ? "Selected " : ""
        if (com.runningBrowser) {
            this.selectElt.textContent = selTxt
            env.moduleSet.previousSelectedIdx = this.modIdx
        }
    }
    // Use refreshInEditorBuffer when the text is changed from an
    // ourside source, such as reading a file.  Don't use this when
    // the user edits the text in the editor.
    refreshInEditorBuffer () {
        const xs = this.asmInfo.asmSrcText;
//        console.log (`refreshInEditorBuffer xs=${xs}`);
        if (com.runningBrowser) {
            document.getElementById("EditorTextArea").value = xs;
        }
    }
    showShort () { // for testing with console.log
        let xs = `Sigma16Module key=${this.modKey} name=${this.moduleName}`
        xs += ` {(this.fileHandle ? "has file" : "no file")}\n`
        xs += ` src=${this.currentAsmSrc.slice(0,200)}...\n`
//        xs += ` obj=${this.objText.slice(0,200)}...\n`
        xs += ` AsmInfo:\n`
        xs += this.asmInfo.showShort()
        xs += "End of module\n"
        return xs
    }
}

//-------------------------------------------------------------------------
// Handle controls for individual modules
//-------------------------------------------------------------------------

// Select module m, put it into Editor pane

export function handleSelect (m) {
    //    const oldSelIdx = st.env.moduleSet.selectedModuleIdx
    const oldSelIdx = env.moduleSet.previousSelectedIdx
    const newSelIdx = m.modIdx
    env.moduleSet.modules[oldSelIdx].setSelected (false)
    env.moduleSet.modules[newSelIdx].setSelected (true)
    env.moduleSet.selectedModuleIdx = newSelIdx
//    console.log (`Select module old=${oldSelIdx}, new=${newSelIdx}`)
}

// Move current module m up in the Module Set list

export function handleModUp (m) {
    console.log (`Move module ${m.modKey} up`)
    let mIdx = m.modIdx
    let mElt = m.displayElt
    let maElt = mElt.previousElementSibling
    if (maElt) {
        let maIdx = mIdx - 1
        let ma = env.moduleSet.modules[maIdx]
        console.log (`Module Up: move m ${mIdx} up to before ma ${maIdx}`)
        console.log (`mElt=${mElt}`)
        console.log (`maElt=${maElt}`)
        m.modIdx = maIdx
        ma.modIdx = mIdx
        const containerElt = document.getElementById("ModSetControls")
        containerElt.insertBefore (mElt, maElt)
        env.moduleSet.modules[maIdx] = m
        env.moduleSet.modules[mIdx] = ma
    } else {
        console.log ("handleModUp: nothing to do")
    }
}

export async function handleModRefresh (m) {
    console.log (`Refresh module ${m.modKey}`)
    const fileHandle = m.fileHandle
    if (fileHandle) {
        const file = await fileHandle.getFile ();
        await file.text ()
            .then (xs => {
                //            console.log (`openFile lambda xs = ${xs}`)
//                const fn = file.name
//                const modName = getFileBaseName (fn)
                //            const m = st.env.moduleSet.addModule ()
  //     const m = st.env.moduleSet.addModule (modName,xs)            
//                m.fileHandle = fileHandle
                handleSelect (m)
                m.changeSavedAsmSrc (xs)
//                m.filename = fn
//                console.log (`calling m.setModuleName ${modName}`)
//                m.setModuleName (modName)
                console.log (`refresh mod module ${m.moduleName}`)
//                document.getElementById("EditorTextArea").value = xs
                console.log ("refresh mod just changed asm src")
//  console.log (`refresh module received fn=${fn} xs=${xs}`)
            }, () => {
                console.log ("failed to read file")
            })
        
    } else {
        console.log ("Module refresh: no file handle")
    }
}

// Close a module and remove it from the module set

export function handleClose (m) {
    console.log (`Close module ${m.modKey}`)
    const elt = m.displayElt
    elt.remove ()
    const a = env.moduleSet.modules
    const i = m.modIdx

    let si = env.moduleSet.selectedModuleIdx
    let pi = env.moduleSet.previousSelectedIdx
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
    env.moduleSet.selectedModuleIdx = si
    env.moduleSet.previousSelectedIdx = pi
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


//-------------------------------------------------------------------------
// Sigma16 module set
//-------------------------------------------------------------------------

// There is one ModuleSet object, env.moduleSet, which is an
// instance of class ModuleSet and is stored as a component of the
// global environment env.  It contains an array of all the extant
// modules, as well as keeping track of the currently selected module.
// Invariants: (1) at all times, there is at least one module, and (2)
// at all times, one module is selected, (3) the text of the selected
// module is in the editor buffer, and (4) at all times there is text
// for the object code, assembly listing, and metadata (if there's no
// valid data for these, there will be dummy text saying "Not
// available, the source needs to be assembled".  Usage example:
// env.moduleSet.getSelectedModule ()

export class ModuleSet {
    constructor () {
        console.log ('Initializing ModuleSet')
        this.modules = []
        this.selectedModuleIdx = 0
        this.previousSelectedIdx = 0
    }
    addModule (name,text) {
        const m = new Sigma16Module (name,text)
        this.modules.push (m)
        this.selectedModuleIdx = this.modules.length - 1
//        console.log (`addModule there are $(this.modules.length) modules\n`)
        if (com.runningBrowser) {
//            console.log ("addModule: running browser, setting module display")
            m.setModuleDisplay ()
        } else {
//            console.log ("addModule: running node")
        }
        return m
    }
    getSelectedModule () {
        return this.modules [this.selectedModuleIdx]
    }
    refreshDisplay () { // this is used
         for (let i = 0; i < env.moduleSet.modules.length; i++) {
            console.log (`*** ${i} ${env.moduleSet.modules[i].modIdx} `)
//            console.log (env.moduleSet.modules[i]
//                         .asmInfo.asmSrcText.split("\n")[0])
            console.log ('\n')
        }
    }
}


//----------------------------------------------------------------------
// Assembler information record
//----------------------------------------------------------------------

// A ...Text fields is a single string that can be used for display,
// and a ...Lines field is a list of individual lines corresponding to
// the text string.

export class AsmInfo {
    //    constructor (m) { // m/asm remove m as parameter
    constructor (baseName, srcText) {
        //        this.asmSrcText = "; default asm src"; // raw source text
  //	this.asmModName = "anonymous";     // may be defined by module stmt
	this.asmModName = baseName;     // may be defined by module stmt        
        this.baseName = baseName
        this.asmSrcText = srcText // raw source text        
        this.asmSrcLines = this.asmSrcText.split ("\n")
//     this.asmSrcLines = [];               // list of lines of source text
        //        this.srcLines = splitLines (srcText)
	this.objectCode = [];                // array of hex strings
        this.objectText = "";       // object code as single string
        this.mdText = "";
        this.metadata = new Metadata ();  // address-source map
        this.asmListingText = "";

	this.asmStmt = [];                 // corresponds to source lines
	this.symbols = [];                 // symbols used in the source
	this.symbolTable = new Map ();     // symbol table
	this.locationCounter =             //  next code address
            new Value (0, Local, Relocatable);
        this.imports = [];                 // imported module/identifier
        this.exports = [];                 // exported identifiers
	this.nAsmErrors = 0;               // errors in assembly source code
        this.objMd = null;
    }
    showShort () {
        let xs = `AsmInfo`
        let showSrc = this.asmSrcLines.slice(0,4).join("\n")
        let showObj = this.objectCode.slice(0,4).join("\n")
        xs += ` asmModName=${this.asmModName}\n`
        xs += showSrc
        xs += showObj
        return xs
    }
}

//----------------------------------------------------------------------
// Symbol table
//----------------------------------------------------------------------

// The symbol table is a map from strings to Identifiers, where the
// string is the text of the identifier name, and the Identifier
// object is a record containing all the required information about
// the identifier.

// An Identifier is a symbol table entry: i.e. a record giving all the
// information about an identifier.  It is stored in the symbol table
// keyed by the identifier string.  The name is the identifier string,
// taken from the label field; v is its value, and defline is line
// number where the identifier was defined.

export class Identifier {
    constructor (name, mod, extname, v, defLine) {
        this.name = name;
        this.mod = mod;
        this.extname = extname;
        this.value = v;
        this.defLine = defLine;
        this.usageLines = [];
    }
}

export function displaySymbolTableHtml (ma) {
    ma.metadata.pushSrc ("", "", "");
    ma.metadata.pushSrc ("Symbol table",
                         "<span class='ListingHeader'>Symbol table</span>",
                         "<span class='ListingHeader'>Symbol table</span>");
    let symtabHeader = "Name        Val Org Mov  Def Used";
    ma.metadata.pushSrc
      (symtabHeader,
       `<span class='ListingHeader'>${symtabHeader}</span>`,
       `<span class='ListingHeader'>${symtabHeader}</span>`);
    let syms  =[ ...ma.symbolTable.keys() ].sort();
    com.mode.devlog (`Symbol table keys = ${syms}`);
    for (let symkey of syms) {
        let x = ma.symbolTable.get (symkey);
        let fullname = x.mod ? `${x.mod}.${x.name}`
            : `${x.name}`;
        let xs = fullname.padEnd(11)
            + x.value.toString()
    	    + x.defLine.toString().padStart(5)
            + '  '
            + x.usageLines.join(',');

        ma.metadata.pushSrc (xs, xs, xs);
    }
}

//-------------------------------------------------------------------------
// Value
//-------------------------------------------------------------------------

// A value is a 16-bit word represented as a natural number; it also
// has attributes (origin and movability) that affect its usage.
// Values are produced by evaluating an expression.  Values may be
// used to define instruction fields, and they may also be used as
// arguments to assembler directives.


// Origin attribute
export const Local = Symbol ("Loc");         // defined in this module
export const External = Symbol ("Ext");      // defined in another module

// Movability attribute
export const Fixed = Symbol ("Fix");         // constant
export const Relocatable = Symbol ("Rel");   // changes during relocation

export class Value {
    constructor (v, o, m) {
        this.word = v;
        this.origin = o;
        this.movability = m;
    }
    copy () {
        return new Value (this.word, this.origin, this.movability);
    }
    add (k) {
        this.word = this.word + k.word;
        this.movability =
            k.movability==Fixed ? this.movability
            : this.movability==Fixed ? k.movability
            : Fixed;
    }
    toString () {
        let xs =  `${arith.wordToHex4(this.word)}`
            + ` ${this.origin.description}`
            + ` ${this.movability.description}`
        return xs;
    }
}

export const ExtVal = new Value (0, External, Fixed);

export function mkConstVal (k) {
    return new Value (k, Local, Fixed);
}

export const Zero = mkConstVal (0);
export const One  = mkConstVal (1);
export const Two  = mkConstVal (2);

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

//-------------------------------------------------------------------------
// Imports and exports
//-------------------------------------------------------------------------

// Constructor arguments: module that exports a name, the name, the
// address/field where the imported value will be inserted.

export class AsmImport {
    constructor (mod, name, addr, field) {
        this.mod = mod;
        this.name = name;
        this.addr = addr;
        this.field = field;
    }
    show () {
        return `AsmImport mod=${this.mod} name=${this.name} `
        + `addr=${arith.wordToHex4(this.addr)} field=${this.field}\n`;
    }
}

export function showAsmImports (xs) {
    let r = "AsmImports...\n";
    for (const x of xs) { r += x.show() }
    return r;
}

// name (string) is the identifier being exported, val (number) is the
// value of the identifier, status (string) is either "relocatable" or
// "fixed".

export class AsmExport {
    constructor (name,val,status) {
        this.name = name;
        this.val = val;
        this.status = status;
    }
    show () {
        return `Export name=${this.name} val=${arith.wordToHex4(this.val)}`
            + ` status=${this.status}`;
    }
}

export function showModMap (m) {
    let r = "Module map...\n";
    for (const k of m.keys()) {
        r += `key ${k} -> ${m.get(k).objectLines.length}\n`;
    }
    return r;

}

export function showAsmExportMap (m) {
    //    let r = "AsmExport map...\n";
    let r = "";
    for (const k of m.keys()) {
        r += `key ${k} -> ${m.get(k).show()}`;
    }
    return r;
}

export function showAsmExports (xs) {
    let r = "AsmExports...\n";
    for (const x of xs) { r += x.show() }
    return r;
}

export function showBlocks (bs) {
    let xs = "";
    for (const b of bs) {
        xs += b.showBlock();
        console.log (b.xs);
    }
    return xs;
}

//-------------------------------------------------------------------------
// Container for object code and metadata
//-------------------------------------------------------------------------

// The assembler produces both an object text and a metadata text.
// For storage in a file, they are represnted as strings that are
// stored in an ObjMd container.  For use during emulation, they are
// stored in an AdrSrcMap object.  An ObjMd is used to serialize or
// populate an AdrSrcMap.

export class ObjMd {
    constructor (modName, objText, mdText) {
        this.modName = modName;
        this.objText = objText;
        this.objLines = objText.split("\n");
        this.mdText = mdText;
        this.mdLines = this.mdText.split("\n");
        this.isExecutable = this.checkExecutable (this);
    }
    // return true if this ObjMd is executable (i.e. can be booted)
    checkExecutable () {
//        console.log (`checkExecutalbe ${this.objText}`)
        let ok = true;
        ok &= this.objLines.length > 0;
        // If there is an import it isn't executable
        for (let i = 0; i < this.objLines.length; i++) {
            const xs = this.objLines[i];
            const fields = parseObjLine (xs);
            if (fields.operation == "import") {
                com.mode.devlog (`check executable: import`
                                 + ` (${fields.operands})`)
                console.log (`check executable: import`
                                 + ` (${fields.operands})`)
                ok = false;
            }
        }
//        console.log (`checkExecutable = ${ok}`)
        return ok;
    }
    hasObjectCode () {
        return this.objText ? true : false
    }
    showShort () {
        let xs = "Object/Metadata:\n"
            + `object module ${this.modName} with `
            + `${this.objLines.length} lines of object text\n`
            + this.objLines.slice(0,3).join("\n")
            + `\n${this.mdLines.length} lines of metadata text\n`
            + this.mdLines.slice(0,3).join("\n")
        return xs;
    }
}

//-------------------------------------------------------------------------
// Object code parser
//-------------------------------------------------------------------------

// A line of object code contains a required operation code, white
// space, and a required operand which is a comma-separated list of
// fields that may contain letters, digits, and commas.

export function parseObjLine (xs) {
    const objLineParser = /^([a-z]+)\s+([\w,]+)$/;
    const blankLineParser = /^\w*$/;
    let blankLine = blankLineParser.exec (xs);
    let splitLine = objLineParser.exec (xs);
    let operation = "";
    let operands = [];
    if (splitLine) {
        operation = splitLine[1];
        operands = splitLine[2].split(',');
    } else if (blankLine) {
//        console.log (`parseObjLine: found blank line <${xs}>`);
    } else {
        console.log ('linker error: object line has invalid format: ' + xs);
    }
        return {operation, operands}
}

//-------------------------------------------------------------------------
// Linker state
//-------------------------------------------------------------------------

// The linker state class encapsulates the linker's variables,
// avoiding a group of global variables.  Normally there will be only
// one object in the class, which can be a global variable or passed
// as an argument to user interface functions.  When a linker state is
// created it is given the file baseName of the executable to be
// created, and a list of text strings comprising the object code of
// the modules to be linked.

// exeMod is an S16Module in which an executable will be built in
// linkerInfo, and oms is a list of S15Modules that contain objInfo

export class LinkerState  {
    constructor (mainName, objMds) {
        // objMds is an ObjMd object with text for obj, md
        this.mainName = mainName;
        this.objMds = objMds;
        this.modMap = new Map ();
        this.oiList = [];
        this.mcount = 0; // number of object modules
        this.locationCounter = 0;
        this.metadata = new Metadata ();
        this.objectLines = [];
        this.srcLines = [];
        this.linkErrors = []; // error messages
        this.listing = "";
        this.exeObjMd = null; // result of link
    }
    showModMap () {
        console.log ("Linker state modMap:")
        console.log (this.modMap)
        console.log ("End of modMap")
    }
    show () {
        let xs = "Linker state:\n"
        xs += `Location counter = `
        xs += `${arith.wordToHex4(this.locationCounter)}\n`;
        xs += `${this.linkErrors.length} Error messages: `
        xs += `${this.linkErrors}\n`;
        xs += `metadata.pairs.length = ${this.metadata.pairs.length}`
        xs += this.exeCodeText;
        xs += this.exeMdText;
        return xs;
    }
}

//-------------------------------------------------------------------------
// Object Info
//-------------------------------------------------------------------------

// The ObjectInfo class collects information about each module being
// linked, as well as the executable.  Constructor arguments: modname
// is string giving base name of the module; omText is a string giving
// the object code text, and omMd is a string giving the metadata text
// (null if there is no metadata).

// Return just the first few lines of a (possibly long) text
export function takePrefix (xs) {
    return xs ? xs.split("\n").slice(0,3).join("\n") : xs
}

export class ObjectInfo {
    constructor (i, modName, objMd) {
        this.index = i; // position in array of object modules
        this.modName = modName;
        this.objMd = objMd;
        this.objMd = null; // ?????????
//        this.moduleName = "anonymous"; // this.obmdtext.baseName;
        this.objText = objMd.objText
        this.mdText = objMd.mdText
        this.objectLines = [];
        this.mdLines = this.mdText ? this.mdText.split("\n") : []
        this.metadata = null;
        this.startAddress = 0;
        this.srcLineOrigin = 0;
        this.dataBlocks = [new ObjectBlock (0)];
        this.relocations = [];
        this.asmImports = [];
        this.asmExportMap = new Map ();
        this.omAsmExports = [];
    }
    show () {
        let xs = `ObjectInfo\n`
//        const xs = `${om.omName}\n`
//              + `  lines of code = ${om.objectLines.length}\n`
//              + `  start address = ${om.startAddress}\n`
//              + `  end address = ${om.endAddress}\n`
//              + `  relocations = ${om.relocations}\n`
//              + showAsmImports (om.asmImports)
//              + showAsmExports (om.omAsmExports)
//              + showAsmExportMap (om.asmExportMap)
//              + showBlocks (om.dataBlocks)
//              + "\n";
        return xs;
    }
}

export class ObjectBlock {
    constructor (blockStart) {
        this.blockStart = blockStart;
        this.blockSize = 0;
        this.xs = [];
    }
    showBlock () {
        return `Block of ${this.blockSize} words from `
            + `${arith.wordToHex4(this.blockStart)}: `
            + `${this.xs.map(arith.wordToHex4)}`;
    }
    insertWord (x) {
        this.xs.push(x);
        this.blockSize++;
    } 
}
