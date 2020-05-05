// Sigma16: emulator.mjs
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
// emulator.mjs interprets machine language programs and displays
// effects in the gui.
//-----------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as st from './state.mjs';
import * as ed from './editor.mjs';
import * as asm from './assembler.mjs';
import * as link from './linker.mjs';



// Calculate the value of pxPerChar, which is needed to control the
// scrolling to make the current line visible.  The calculated value
// overrides the initialized value.  The method is to measure the
// height of the listing in pixels and divide by the number of lines
// in the listing.  Some other geometric parameters are also obtained,
// but aren't currently used.

function getListingDims (es) {
    let e = document.getElementById('ProcAsmListing');
    let x = e.getBoundingClientRect(); // dimensions of visible listing area
    let w = e.scrollWidth; // width of the content, not used
    let h = e.scrollHeight; // height of content (not of the window)
    es.asmListingHeight = h; // save in emulator state
    console.log (`h=${h} w=${w}`);
    let n = es.asmListingPlain.length;
    console.log(`getListingDims: n=${n}`);
    pxPerChar = h / n; // update this global variable, used for scrolling
    console.log (`getListingDims: pxPerChar = ${pxPerChar}`);
}

// asmScrollOffsetAbove specifies the preferred number of lines that
// should appear above the scroll target in the processor assembly
// listing

const asmScrollOffsetAbove = 8;

// pxPerChar is the height of characters used in the processor
// assembly listing.  This is needed to scroll the listing to keep the
// current line visible.  There doesn't appear to be a good way to
// measure this; the value is found by trial and error.  Measuring it
// or extracting it from font metadata would be far better.

let pxPerChar = 13.05;



function refreshIOlogBuffer() {
    console.log (`refreshIOlogBugfer ${ioLogBuffer}`);
    let elt = document.getElementById("IOlog");
    elt.innerHTML = "<pre>" + ioLogBuffer + "</pre>";
    elt.scrollTop = elt.scrollHeight;
}

export let ioLogBuffer = "";
export const procAsmListingElt = document.getElementById('ProcAsmListing');

// export let procAsmListingElt; // global variables for emulator

//------------------------------------------------------------------------------
// Booter 
//------------------------------------------------------------------------------
// Prepare assembly listing when executable is booted

export function initListing (m,es) {
    es.curInstrAddr = 0;
    es.curInstrLineNo = -1;  // -1 indicates no line has been highlighted
    es.nextInstrAddr = 0;
    es.nextInstrLineNo = es.curAsmap[0];
    com.highlightListingLine (es, es.nextInstrLineNo, "NEXT");
    setProcAsmListing (es,m);
}

//------------------------------------------------------------------------------
// Boot
//------------------------------------------------------------------------------

// Attempt to copy the executable code from the selected module into
// memory for execution.

// A module is executable if it contains object code but doesn't have
// any imports.  The object code can come from either the assembler,
// or it can be read in as an object file.  The presence of an
// assembly listing and asmap is optional.

// (Interface: called from gui) Boot from either the current module
// (without linking) or the load module (if the linker has been run).

function oldboot(es) {
    console.log (`boot: selected module = ${smod.selectedModule}`);
    let m = smod.s16modules[smod.selectedModule];
//    let m = getCurrentModule ();
    let mo = m.objInfo;
    if (mo.isExecutable) {
	console.log ('Current module is executable: booting');
	resetRegisters ();
	memClearAccesses();
        document.getElementById('ProcAsmListing').innerHTML = "";
	copyExecutableToMemory (es,m);
	es.asmListingPlain = ma.asmListingPlain;
	es.asmListingDec = ma.asmListingDec;
        es.asmListingCurrent = [];
	for (let i = 0; i < es.asmListingDec.length; i++) { // copy the array
	    es.asmListingCurrent[i] = es.asmListingDec[i];
	}
	initListing (m,es);
	es.nInstructionsExecuted = 0;
	document.getElementById("nInstrExecuted").innerHTML =
	    es.nInstructionsExecuted;
	ioLogBuffer = "";
	refreshIOlogBuffer();
        getListingDims(es);
    } else {
	console.log ('cannot boot')
    }
}

// Copy a module's object code into memory.  Should use objectCode
// rather than codeWord, but objectCode will need to support org.  So
// far it's just a list of words.  This version doesn't yet support
// org and it requires the module to be assembled (rather than read
// in).

function copyExecutableToMemory (es,m) {
    console.log ('copyExecutableToMemory');
    let ma = m.asmInfo;
    let stmt = ma.asmStmt;
    let locationCounter = 0;
    for (let i = 0; i < stmt.length; i++) {
	console.log('bootCM ' + i + ' => ' + stmt[i].codeWord1
		    + ' ' + stmt[i].codeWord2 );
	if (stmt[i].codeWord1 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord1);
	    locationCounter++;
	}
	if (stmt[i].codeWord2 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord2);
	    locationCounter++;
	}
    }
    memShowAccesses();
    memDisplay();
    es.curAsmap = ma.asmap;
    asm.showAsmap (es.curAsmap);
    setProcStatus (es,Ready);
    console.log ('copyExecutableToMemory done');
}

function parseCopyObjectModuleToMemory (es) {
    console.log('boot');
    let objText = document.getElementById("LinkerText").value;
    console.log('objText = ' + objText);
    let xs = objText.split("\n");
    console.log("linker boot: " + xs.length + " lines");
//    console.log(xs);
//    line1 = xs[0];
//    console.log('line1 = ' + line1);
//    fields = line1.split(" ");
//    console.log('fields = ' + fields);
//    console.log('field0 = ' + fields[0]);
//    console.log('field1 = ' + fields[1]);
    //    console.log('field2 = ' + fields[2]);
    bootCurrentLocation = 0;
    for (let i = 0; i < xs.length; i++) {
	linkerBootLine(es, i, xs[i]);
//	experiment(xs[i]);
    }
}

// Should check the operation, implement org, and provide suitable
// error messages, but that's for later.  For now, just assume it is
// hexdata with valid argument

function linkerBootLine (es,m,i,x) {
    let y = parseAsmLine (m,i,x);
//    printAsmLine (y);
    let w = y.fieldOperands;
    let n =  arith.hex4ToWord(w);
//    console.log('linkerBootLine ' + w + ' = ' + n);
    console.log('linkerBootLine ' + i + ' ---' + x + '--- = ' + n);
    updateMem2(bootCurrentLocation,n);
    bootCurrentLocation++;
}


//-------------------------------------------------------------------------------
// Booter
//-------------------------------------------------------------------------------

function checkObject () {
    console.log ("linker: checkObject");
}

export function boot (es) {
    console.log ("boot");
    let m = smod.s16modules[smod.selectedModule]; // get current module
    let ma = m.asmInfo; // only if this module came from assembler
    let mo = m.objInfo;
    let xs = "";
    let fields = null;
    let ok = true; // will set to false if module isn't bootable
    let location = 0; // address where next word will be stored
    resetRegisters();
    memClearAccesses();
    document.getElementById('ProcAsmListing').innerHTML = "";

    // get listing from the assembler
    es.asmListingPlain = ma.asmListingPlain;  // interface
    es.asmListingDec = ma.asmListingDec;  // interface
    
    es.asmListingCurrent = [];
    for (let i = 0; i < es.asmListingDec.length; i++) { // copy the array
	es.asmListingCurrent[i] = es.asmListingDec[i];
    }
    initListing (m,es);
    es.nInstructionsExecuted = 0;
    document.getElementById("nInstrExecuted").innerHTML =
	es.nInstructionsExecuted;
    ioLogBuffer = "";
    refreshIOlogBuffer();
    getListingDims(es);
    
    for (let i = 0; i < mo.objLine.length; i++) {
        xs = mo.objLine[i];
        console.log (`boot: object line ${i} = ${xs}`);
        fields = link.parseObjLine (xs);
        console.log (`op=${fields.operation} args=${fields.operands}`);
        if (fields.operation == "module") {
            let modname = fields.operands[0];
            let safemodname = modname ? modname : "(unknown)";
            console.log (`boot: module ${safemodname}`);
        } else if (fields.operation == "data") {
            console.log ("boot: data");
            for (let j = 0; j < fields.operands.length; j++) {
                let val = arith.hex4ToWord(fields.operands[j]);
                if (!val) {console.log(`boot: bad data (${val})`)};
                let safeval = val ? val : 0;
                memStore (location,safeval);
                console.log (`boot data mem[${location}]:=${val}`);
                location++;
            }
        } else if (fields.operation == "import") {
            console.log (`boot: import (${fields.operands})`)
            ok = false;
        } else if (fields.operation == "export") {
        } else if (fields.operation == "relocate") {
        } else {
            console.log (`boot: bad operation (${fields.operation})`)
            ok = false;
        }
    }
    if (ok) {
        memShowAccesses();
        memDisplay();
        es.curAsmap = ma.asmap;
        asm.showAsmap (es.curAsmap);
        setProcStatus (es,Ready);
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot was successful"
            + "</span><br>"
            + "</pre>";
        document.getElementById('LinkerText').innerHTML = xs;
        console.log ("boot was successful")
    } else {
        setProcStatus (es,Reset);
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot failed: module is not executable"
            + "</span><br>"
            + "</pre>";
        document.getElementById('LinkerText').innerHTML = xs;
        console.log ("boot failed");
        modalWarning ("boot failed");
    }
    console.log ("boot returning");
}



export let highlightedRegisters = [];

// Update the display of all registers and memory (all of memory)
export function displayFullState () {
    console.log ('displayFullState');
    clearRegisterHighlighting ();
    refreshRegisters ();
    memClearAccesses ();
    memDisplayFull ();
}

// ?????


//------------------------------------------------------------------------------
// Processor execution status
//------------------------------------------------------------------------------

export let Reset  = Symbol ("Reset");   // registers and memory cleared
export let Ready  = Symbol ("Ready");   // can execute instruction
export let Paused = Symbol ("Paused");  // temporary halt for inspecting state
export let Halted = Symbol ("Halted");  // trap 0 has executed

//----------------------------------------------------------------------
//  Registers
//----------------------------------------------------------------------

// deprecated
// let enable, mask, req, isys, ipc, handle;           // interrupt control
// let sEnable, sProg, sProgEnd, sDat, sDatEnd;   // segment control registers

// Move to appropriate place... ??????????????
export let modeHighlightAccess = true;  // for tracing: highlight reg/mem that is accessed

// The registers are defined as global variables.  These variables are
// declared here but their values are set only when window has been
// loaded (the onload event in gui.js), because the gui elements will
// exist at that time.

// Declare the registers
export let regFile = [];                            // register file R0,..., R15
export let pc, ir, adr, dat;                        // instruction control registers
export let statusreg, mask, req, istat, ipc, vect;  // interrupt control registers
export let bpseg, epseg, bdseg, edseg;              // segment control registers

// Generating and accessing registers
export let nRegisters = 0;     // total number of registers; increment as created
export let controlRegisters;   // array of the control registers
export let register = [];      // array of all the registers: regfile and control

// The system control registers are specified in the instruction by an
// index starting from i=0..., but the actual emulator data structures
// for these registers are held in the register[] array at index
// i+ctlRegIndexOffset...  It's useful to have a single array
// containing all the registers, so the emulator can refresh them all
// together.

export const ctlRegIndexOffset = 20; // add to ctl reg number to get index in register[]
export let sysCtlRegIdx = 0;       // index of first system control reg

export let registerIndex = 0;          // unique index for each reg
export let regStored = [];
export let regFetched = [];


// Instructions refer to the system control registers by a 4-bit
// index, but the system control register that has instruction index 0
// will actually have a higher index (16) in the emulator's array of
// registers.  To refer to sysctl reg i, it can be accessed as
// register [sysCtlRegIdx + i].


// Each register is represented by an object that contains its current
// value, as well as methods to get and set the value, and to refresh
// the display.  The mkReg function makes a register corresponding to
// a gui element.  To remove highlight, use refresh.

// Arguments
//   regName is the name of the register, e.g. "pc"
//   eltName is the html id of the gui element
//   show(x) converts a value x to a string that can be displayed
// Set by creation loop
//   regIdx is the unique index in the array of all registers
// Local state
//   val is the current contents of the register
//   elt is gui element used to display the register
//   put(x) discards current val and replaces it with x (highlight if mode set)
//   get() returns current val (highlight if mode set)
//   refresh() puts the current val into display, without any highlighting

// Textual representation of system status, for the emulator display

function showSysStat (s) {
    return s===0 ? 'Usr' : 'Sys'
}

function testReg1 () {
    console.log("testReg1");
    regClearAccesses();
    pc.put(3);
    console.log ("ir = " + ir.get());
    console.log ("pc = " + pc.get());
    regShowAccesses();
}

function testReg2 () {
    console.log("testReg1");
    regClearAccesses();
    console.log ("ir = " + ir.get());
    console.log ("pc = " + pc.get());
    adr.put(20);
    regShowAccesses();
}


// The registers are actually created in gui.js in the window.onload
// function, because they need the gui display elements to be created
// first

export function mkReg (rn,eltName,showfcn) {
    let r = Object.create({
	regIdx : 0, // will be overwritten with actual index
	regName : rn,
	show : showfcn,
	val : 0,
	elt : document.getElementById(eltName),
	put : function (x) {
	    this.val = x;
	    console.log (`reg put rn=${rn} idx=${this.regIdx} x=${x}`);
	    if (this.regIdx<16) {
		// record regfile put
		instrEffect.push (["R", this.regIdx, x, this.regName]);
	    console.log (`mkReg put recording effect 0 ${instrEffect[0]}`);
	    console.log (`mkReg put recording effect 1 ${instrEffect[1]}`);
	    }
	    if (modeHighlight) { regStored.push(this) } },
        get : function () {
	        let x = this.val;
	        if (modeHighlight) { regFetched.push(this) };
	        return x },
	refresh : function() {
	    console.log (`refresh register ${rn}`);
	    this.elt.innerHTML = this.show(this.val);
	},
	showNameVal: function() {return this.regName + '=' + this.show(this.val);}
    });
//    register[nRegisters] = this;
    nRegisters++;
    return r;
}

// R0 is special: it always contains 0 and cannot be changed
export function mkReg0 (rn,eltName,showfcn) {
    let r = Object.create({
	regName : rn,
	show : showfcn,
	val : 0,
	elt : document.getElementById(eltName),
	put : function (x) { },
        get : function () { return 0 },
	refresh : function() {this.elt.innerHTML = this.show(this.val);},
	showNameVal: function() {return this.regName + '=' + this.show(this.val);}
    });
    nRegisters++;
    return r;
}

function regShowAccesses () {
    let i, r;
    for (i = 0; i < regFetched.length; i++) {
	r = regFetched[i];
	r.elt.innerHTML = highlightText(r.show(r.val),'GET')
    }
    for (i = 0; i < regStored.length; i++) {
	r = regStored[i];
	r.elt.innerHTML = highlightText(r.show(r.val),'PUT')
    }
}

function regClearAccesses () {
    let r;
    for (let i = 0; i < regFetched.length; i++) {
	regFetched[i].refresh();
    }
    for (let i = 0; i <regStored.length; i++) {
	regStored[i].refresh();
    }
    regFetched = [];
    regStored = [];

}

// Resetting the registers sets them all to 0, 
function resetRegisters () {
    console.log('Resetting registers');
    for (let i = 0; i < nRegisters; i++) {
	register[i].val = 0;
	register[i].refresh();
    }
}

// Refresh all the registers.  This ensures the display corresponds to the
// current values, and it also removes any highlighting of the registers.

function refreshRegisters() {
    console.log('Refreshing registers');
    for (let i = 0; i < nRegisters; i++) {
	register[i].refresh();
    }
}

//------------------------------------------------------------------------------
// Emulator state
//------------------------------------------------------------------------------

// This is the global emulator state.  The functions in the emulator
// don't use it directly, in order to avoid excessive use of globals.
// The current emulator state is passed as needed to functions (the
// convention is that the parameter name is 'es').

export let emulatorState =
    {
// Processor
        //	procStatus : "Reset",  ??????
        procStatus : Reset,
	nInstructionsExecuted : 0,
	instrLooperDelay : 1000,
	instrLooperShow : false,
	breakEnabled : false,
	breakPCvalue : 0,
	// Instruction being executed
	doInterrupt   : 0,
	ir_op         : 0,
	ir_d          : 0,
	ir_a          : 0,
	ir_b          : 0,
        ea            : 0,
	instrDisp     : 0,
        field_e       : 0,
        field_f       : 0,
        field_g       : 0,
        field_h       : 0,
        field_gh       : 0,
	instrOpCode   : null,
	instrCodeStr  : "",
	instrFmtStr   : "",
	instrOp       : "",
	instrArgs     : "",
	instrEA       : null,
	instrEAStr    : "",
	instrEffect   : [],
	// Tracking source lines corresponding to current instruction
	curAsmap : [],
	asmListingPlain    : [], // plain listing shows address, code, source
	asmListingDec      : [], // decorated listing uses highlighting for fields
	asmListingCurrent  : [], // version of listing displayed in emulator pane
        asmListingHeight   : 0,   // height in pixels of the listing
	curInstrAddr       : 0,
	nextInstrAddr      : 0,
	curInstrLineNo     : -1,  // -1 indicates no line has been highlighted
	nextInstrLineNo    : -1,
	saveCurSrcLine     : "",
	saveNextSrcLine    : "",
//	srcLinePlain       : [],
//	srcLineHighlightedFields : []
    }



//------------------------------------------------------------------------------
// Initialize machine state
//------------------------------------------------------------------------------

//---------------------------------------------------------------------------
// Processor elements: html elements for displaying instruction decode

let instrCodeElt;
let instrFmtElt;
let instrOpElt;
let instrArgsElt;
let instrEAElt;
let instrCCElt;
let instrEffect1Elt;
let instrEffect2Elt;


export function initializeProcessorElements () {
    console.log ('initializeProcessorElements');
    instrCodeElt = document.getElementById("InstrCode");
    instrFmtElt  = document.getElementById("InstrFmt");
    instrOpElt   = document.getElementById("InstrOp");
    instrArgsElt = document.getElementById("InstrArgs");
    instrEAElt   = document.getElementById("InstrEA");
    instrCCElt   = document.getElementById("InstrCC");
    instrEffect1Elt = document.getElementById("InstrEffect1");
    instrEffect2Elt = document.getElementById("InstrEffect2");
}


export function initializeMachineState () {
    console.log ("emulator: initializeMachineState");
    initializeProcessorElements ();  // so far, it's just instr decode
    clearInstrDecode (emulatorState);

    // Build the register file; R0 is built specially as it is
    // constant; all others are built with mkReg
    for (let i = 0; i<16; i++) {
	let regname = 'R' + i; // also the id for element name
        let thisReg;
	thisReg = (i==0) ?
	    mkReg0 (regname, regname, arith.wordToHex4)
	    : mkReg (regname, regname, arith.wordToHex4);
	thisReg.regIdx = i;
	regFile[i] = thisReg;
	register[i] = thisReg;
    }

    // Instruction control registers
    pc    = mkReg ('pc',       'pcElt',       arith.wordToHex4);
    ir    = mkReg ('ir',       'irElt',       arith.wordToHex4);
    adr   = mkReg ('adr',      'adrElt',      arith.wordToHex4);
    dat   = mkReg ('dat',      'datElt',      arith.wordToHex4);

    // Interrupt control registers
    statusreg   = mkReg ('statusreg',  'statusElt',  arith.wordToHex4);
    // bit 0 (lsb) :  0 = User state, 1 = System state
    // bit 1       :  0 = interrupts disabled, 1 = interrupts enabled
    // bit 2       :  0 = segmentation disabled, 1 = segmentation enabled
    mask  = mkReg ('mask',     'maskElt',    arith.wordToHex4);
    req   = mkReg ('req',      'reqElt',     arith.wordToHex4);
    // mask and request use the same bit positions for flags
    // bit 0 (lsb)  overflow
    // bit 1        divide by 0
    // bit 2        trap 3
    // bit 3        
    istat    = mkReg ('istat',    'istatElt',      arith.wordToHex4);
    ipc      = mkReg ('ipc',      'ipcElt',      arith.wordToHex4);
    vect     = mkReg ('vect',   'vectElt', arith.wordToHex4);

// Segment control registers
    bpseg = mkReg ('bpseg',    'bpsegElt',    arith.wordToHex4);
    epseg = mkReg ('epseg',    'epsegElt',    arith.wordToHex4);
    bdseg = mkReg ('bdseg',    'bdsegElt',    arith.wordToHex4);
    edseg = mkReg ('edseg',    'edsegElt',    arith.wordToHex4);

// Record the control registers    
    controlRegisters =
	[pc, ir, adr, dat,   // not accessible to getctl/putctl instructions
	 // the following can be used for getctl/getctl, indexing from 0
	 statusreg, mask, req, istat, ipc, vect,
         bpseg, epseg, bdseg, edseg
	];
    nRegisters = 16;  // Start after the first 16 (the regfile)
    controlRegisters.forEach (function (r) {
	console.log('making reg ' + nRegisters + ' = ' + r.regName);
	register[nRegisters] = r;
	r.regIdx = nRegisters;
	nRegisters++;
        });

    memInitialize();
    resetRegisters();

    
}

//------------------------------------------------------------------------------
// Instruction decode
//------------------------------------------------------------------------------

function showInstrDecode (es) {
    es.instrCodeStr = (instrCode ? arith.wordToHex4 (instrCode) : "")
	+ " " + (es.instrDisp ? arith.wordToHex4 (es.instrDisp) : "");
    es.instrEAStr = es.instrEA ? arith.wordToHex4 (es.instrEA) : "";
    console.log (`showInstrDecode fmt = ${es.instrFmtStr}`);
    refreshInstrDecode (es);
}

export function clearInstrDecode (es) {
    es.instrOpCode = null;
    es.instrDisp = null;
    es.instrCodeStr  = "";
    es.instrFmtStr   = "";
    es.instrOp    = "";
    es.instrArgs  = "";
    es.instrEA = null;
    es.instrEAStr    = "";
    es.instrEffect = [];
}

function refreshInstrDecode (es) {
    console.log ("refreshInstrDecode");
    instrCodeElt.innerHTML = es.instrCodeStr;
    instrFmtElt.innerHTML  = es.instrFmtStr;
    instrOpElt.innerHTML   = es.instrOpStr;
    instrArgsElt.innerHTML = showArgs(es); // instrArgsStr;
    instrEAElt.innerHTML   = es.instrEAStr;
    let ccval = regFile[15].val;
    instrCCElt.innerHTML      = arith.showCC(ccval);
    instrEffect1Elt.innerHTML = showEffect(es,0);
    instrEffect2Elt.innerHTML = showEffect(es,1);
}

//----------------------------------------------------------------------
//  Memory
//----------------------------------------------------------------------

// Usage
//   General operations
//     memInitialize              get html elements, clear refresh, display
//     memClear ()                set each location to 0
//     memRefresh ()              recalculate the memory strings

//   During instruction execution
//     memClearAccesses ()        remove get/put highligting
//     ir = memFetchInstr (a)     instruction fetch
//     adr = memFetchData (a)     data fetch
//     memStore (a,x)             store
//     memShowAccesses ()         update array of hex strings
//     memDisplayFast ()          render html elements showing only accessed area
//     memDisplayFull ()          render html elements showing full memory
//     memDisplay ()              use display mode to select fast or full


// The memory is represented as array of words (represented as an
// integer between 0 and 2^16-q) and a corresponding array of strings
// showing each word as a hex number.  There are html elements for
// displaying the memory contents, and two arrays to track memory
// accesses (fetches and stores) in order to generate the displays.

let memSize = 65536; // number of memory locations = 2^16
let memory = [];  // the memory contents, indexed by address
let memString = []; // a string for each location, to be displayed
let memElt1, memElt2;  // html elements for two views into the memory
let memFetchInstrLog = [];
let memFetchDataLog = [];
let memStoreLog = [];
export let memDisplayModeFull = false;  // show entire memory? or just a little of it?
let memDisplayFastWindow = 16;   // how many locations to show in fast mode
let memDispOffset = 3;    // how many locations above highligted one

// Must wait until onload event

function memInitialize () {
    memElt1 = document.getElementById('MemDisplay1');
    memElt2 = document.getElementById('MemDisplay2');
    memClear();    // set each location to 0
    memRefresh();  // generate a string showing each location
    memDisplay();  // put the strings into the gui display elements
}

// There are two primary memory accesses: fetch and store.  These
// functions record the operation to enable the user interface to show
// the access by using colors to highlight the accessed location.

// There is just one memory, but the gui contains two windows into the
// memory: by convention, display 1 shows instruction fetches and
// display 2 shows data fetches and stores.  In the hardware (the
// digital circuit that implements the processor) there may be no
// distinction between memory and data accesses (although there could
// be if the machine has separate instruction and data caches).

// All memory stores are considered to be data stores.  Howver, there
// are two variants of fetch: instruction fetch and data fetch.  Both
// of these record the operation in the array memFetchInstrLog, but
// they record the address in separate scalar vairables to enable the
// gui to scroll the two displays to show the instruction access in
// disply 1 and the data access in display 2.

// Set all memory locations to 0

function memClear () {
    for (let i = 0; i < memSize; i++) {
	memory[i] = 0;
    }
    memFetchInstrLog = [];
    memFetchDataLog = [];
    memStoreLog = [];
    memRefresh ();
}

// Refresh all the memory strings; the memString array should be accurate
// but this function will recalculate all elements of that array

// Note on data structure.  I tried having a preliminary element [0]
// containing just "<pre class='HighlightedTextAsHtml'>", so address a
// is shown in memString[a+1].  The indexing was fine but the
// scrolling didn't work, possibly because of this dummy element with
// its newline inserted when the array is joined up.

function memRefresh () {
    memString = [];  // clear out and collect any existing elements
//    memString[0] =  "hello";  // "<pre class='HighlightedTextAsHtml'>"
    for (let i = 0; i < memSize; i++) {
	setMemString(i);
    }
//    memString.push("bye");    // "</pre>");

}

// Create a string to represent a memory location; the actual value is
// in the memory array, and the string is placed in the memString
// array.  memString[0] = <pre class="HighlightedTextAsHtml"> and
// mem[a] corresponds to memString[a+1].

function setMemString(a) {
    memString[a] = arith.wordToHex4(a) + ' ' + arith.wordToHex4(memory[a]);
}

// Fetch and return a word from memory at address a, and record the
// address so the display can show this access.

function memFetchInstr (a) {
    memFetchInstrLog.push(a);
    return memory[a];
}

function memFetchData (a) {
    memFetchDataLog.push(a);
    return memory[a];
}

// Store a word x into memory at address a, and record the address so
// the display can show this access.

function memStore (a,x) {
    memStoreLog.push(a);
    instrEffect.push(["M", a, x]);
    memory[a] = x;
}

// Update the memory string for each location that has been accessed,
// so that it contains an html div element which can be used to
// highlight the memory location.  Do the fetches first, then the
// stores: this ensures that if a location has both been fetched and
// stored, the highlighting for the store will take precedence.

function memShowAccesses () {
    let i, a;
    for (i = 0; i < memFetchInstrLog.length; i++) {
	a = memFetchInstrLog[i];
	highlightMemString(a,"GET");
    }
    for (i = 0; i < memFetchDataLog.length; i++) {
	a = memFetchDataLog[i];
	highlightMemString(a,"GET");
    }
    for (i = 0; i < memStoreLog.length; i++) {
	a = memStoreLog[i];
	highlightMemString(a,"PUT");
    }
}

// Remove the highlighting for the memory locations that have been accessed

function memClearAccesses () {
    let a;
    for (let i = 0; i < memFetchInstrLog.length; i++) {
	a = memFetchInstrLog[i];
	setMemString(a);
    }
    for (let i = 0; i < memFetchDataLog.length; i++) {
	a = memFetchDataLog[i];
	setMemString(a);
    }
    for (let i = 0; i < memStoreLog.length; i++) {
	a = memStoreLog[i];
	setMemString(a);
    }
    memFetchInstrLog = [];
    memFetchDataLog = [];
    memStoreLog = [];
}

// Create a string with a span class to represent a memory location
// with highlighting; the actual value is in the memory array, and the
// string is placed in the memString array.

function highlightMemString(a,highlight) {
    memString[a] =
	"<span class='" + highlight + "'>"
	+ arith.wordToHex4(a) + " " + arith.wordToHex4(memory[a])
        + "</span>";
}

// Set the memory displays, using the memString array.  Check mode to
// determine whether the display should be partial and fast or
// complete but slow.

function memDisplay () {
    if (memDisplayModeFull) { memDisplayFull () }
    else { memDisplayFast () }
}

// Set the memory displays, showing only part of the memory to save time

function memDisplayFast () {
//    console.log ('memDisplayFast');
    let xa, xb, xs1, xs, yafet, yasto, ya, yb, ys1, ys;

    xa = (memFetchInstrLog.length===0) ? 0 : (memFetchInstrLog[0] - memDispOffset);
    xa = xa < 0 ? 0 : xa;
    xb = xa + memDisplayFastWindow;
    xs = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
    	+ memString.slice(xa,xb).join('\n')
	+ "</code></pre>";

//    xs = "<pre><code class='HighlightedTextAsHtml'>TEST</code></pre>";
//    xs = "<pre><code>TEST</code></pre>";
//    xs = "<code>TEST</code>";
//    xs = "<pre>TEST</pre>";
    console.log ('  xa=' + xa + '  xb=' + xb);
    memElt1.innerHTML = xs;

    yafet = (memFetchDataLog.length===0) ? 0 : (memFetchDataLog[0] - memDispOffset);
    yasto = (memStoreLog.length===0) ? 0 :(memStoreLog[0] - memDispOffset);
    ya = yafet > 0 && yafet < yasto ? yafet : yasto;
    ya = ya < 0 ? 0 : ya;
    yb = ya + memDisplayFastWindow;
    ys = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	+ memString.slice(ya,yb).join('\n')
	+ "</code></pre>";
    console.log ('  ya=' + ya + '  yb=' + yb);
    memElt2.innerHTML = ys;
}

// Set the memory displays, showing the full memory

// Need <pre> to get the formatting correct with line breaks.  but
// <pre> prevents scrolling from working.  Could try not using pre,
// but putting <br> after each line, but that still wouldn't work
// because multiple spaces in code wouldn't work..  Try <code>; With
// <code class=... scrolling works, but the line breaks aren't
// there.. Is there a problem with HighlightedTextAsHtml?

// THE RIGHT WAY TO DO IT: code inside pre; class defined in code:

//    xs = "<pre><code class='HighlightedTextAsHtml'>"
//	+ memString.join('\n')
//	+ "</code></pre>";

function memDisplayFull () {
    let xs;                 // display text
    let xt, xo;             // display 1 targets and offsets
    let yafet, yasto, ya, yo, yt;
    console.log ('memDisplayFull');
    xs = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	+ memString.join('\n')
	+ "</code></pre>";
    memElt1.innerHTML = xs;
    xt = (memFetchInstrLog.length===0)? 0 : memFetchInstrLog[0] - memDispOffset;
    xo = xt * pxPerChar;
    console.log('  target1 xt = ' + xt + '   offset1 = ' + xo);
    memElt1.scroll(0,xo);
    
    memElt2.innerHTML = xs;
    yafet = (memFetchDataLog.length===0) ? 0 : (memFetchDataLog[0] - memDispOffset);
    yasto = (memStoreLog.length===0) ? 0 :(memStoreLog[0] - memDispOffset);
    yt = (yasto > 0 ? yasto : yafet) - memDispOffset;
    yt = yt < 0 ? 0 : yt;
    yo = yt * pxPerChar;
    console.log('  yafet=' + yafet + ' yasto=' + yasto
		+ '  target1 yt = ' + yt + '   offset1 = ' + yo);
    memElt2.scroll(0,yo);
}

function memTest1a () {
    console.log('testMem1a');
    memClear ();
    memStore(3,7);
    memStore(6,2);
    memShowAccesses ();
    memDisplay ();
}

function memTest1b () {
    console.log('testMem1b');
    memClearAccesses ();
    let y = memFetchInstr(6);
    memStore(300,20);
    memShowAccesses();
    memDisplay ();
//    console.log('testMem1 x = ' + x);  // should be 0, highlight Fetch
//    console.log('testMem1 y = ' + y);  // should be 7, highlight Store
}

function memTest2 () {
    console.log('testMem2');
    memClear ();
    let y = memFetchInstr (32768);
    let q = memFetchData (50);
    memStore (65520, 7);
    memShowAccesses ();
    memDisplay ();
}


// ?????

//------------------------------------------------------------------------------
// Processor execution status
//------------------------------------------------------------------------------

function setProcStatus (es,s) {
    es.procStatus = s;
    document.getElementById("procStatus").innerHTML = showProcStatus(s);
}

function showProcStatus (s) {
    return s == Reset ? "Reset"
        : s == Ready  ? "Ready"
        : s == Paused ? "Paused"
        : s == Halted ? "Halted"
        : "Unknown"
}



// Global variables for instruction decode; used in emulator

let displayInstrDecode = true;
let instrCode, instrDisp, instrCodeStr; // record the values
let instrFmtStr = "";
let instrOpStr = "";
let instrArgsStr = "";
let instrEA, instrEAStr;
let instrEffect = [];



// Fields of the current instruction

let instr = 0;
let ir_op = 0, ir_d = 0, ir_a = 0, ir_b = 0;  // instruction fields

// The value of the effective addresss pecified by an RX instruction

let ea = 0;  // effective address

// Global variables for handling listing display as program runs.
// Uses global variables set by linker: exMod is executing module and
// curAsmap is map from addresses to source lines

let srcLine;        // copy of source statements


// Keep track of the address of the currently executing instruction,
// the address of the next instruction, and the line numbers where
// these instructions appear in the assembly listing.  -1 indicates no
// line has been highlighted

let curInstrAddr, curInstrLineNo, saveCurSrcLine;
let nextInstrAddr, nextInstrLineNo, saveNextSrcLine;


export function initializeSubsystems () {
    memDisplayModeFull = false;
//    document.getElementById('FullDisplayToggleButton').value = "Fast display";
    document.getElementById('PP_Toggle_Display').value = "Fast display";  
}

export function toggleFullDisplay () {
    console.log ('toggleFullDisplay clicked');
    memDisplayModeFull = ! memDisplayModeFull;
    document.getElementById('FullDisplayToggleButton').value =
	memDisplayModeFull ? "Full display" : "Fast display";
    if (memDisplayModeFull) { memDisplayFull () }
    else { memDisplayFast ()
	 }  // loses info but makes tab switching faster
}

// ------------------------------------------------------------------------
// Highlighting registers to indicate accesses

// When a register is accessed, its display in the gui is highlighted
// by setting the text color.  If the register has not been used it
// has the default color black, if it has been read but not written
// its color is READ, and if it has been written its color is WRITE.
// The meanings of the tags for syntax highlighting are defined in
// Sigma16gui.css.  Normally we would use blue for READ and red for
// WRITE.

let modeHighlight = true;  // indicate get/put by setting text color

function setModeHighlight (x) {
    if (x) {
	console.log('Setting modeHighlight to True');
	modeHighlight = true;
    }
    else {
	console.log('Setting modeHighlight to False');
	modeHighlight = false;
	refreshRegisters();
    }
}

function highlightText (txt,tag) {
    return "<span class='" + tag + "'>" + txt + "</span>";
}

function clearRegisterHighlighting () {
    let n =  highlightedRegisters.length;
    let r;
    for (let i = 0; i < n; i++) {
	r = highlightedRegisters[i];
	console.log('clear highlight ' + i + ' reg = ' + r.regName);
	r.refresh();
    };
    highlightedRegisters = [];
}


//-----------------------------------------------------------------------------
// Interrupts
//-----------------------------------------------------------------------------

export function timerInterrupt() {
    console.log ("Timer Interrupt clicked");
    arith.setBitInRegBE (req, arch.timerBit);
    req.refresh();
}

//------------------------------------------------------------------------------
// Interface to the gui
//------------------------------------------------------------------------------

// The functions receive a parameter 'es' which carries the current
// emulator state.  The gui, when it calls one of the main interface
// functions, passes the global emulatorState.

// The machine state (registers and memory) are global and initialized
// by gui.  The emulator state is a global variable named
// emulatorState, defined in state.js.

// The interface to the emulator consists of the following functions,
// which are called directly by the gui. When they call other
// functions, es is passed as a parameter.  Thus only the following
// interface functions access a global variable.

// Called by ?
//   parseCopyObjectModuleToMemory (es)
    
// Called by gui.js
//   clearInstrDecode ()

// Called by events in Sigma16.html
//   procReset(es)       -- put processor into initial state
//   boot(es)            -- copy the executable into memory
//   procStep(es)        -- execute one instruction
//   procRun(es)         -- execute instructions repeatedly until halted
//   procPause(es)       -- halt execution (can resume execution later)
//   procInterrupt()   -- not implemented yet
//   procBreakpoint()  -- not implemented yet


function showArgs (es) {
    if (es.instrFmtStr==="RRR") {
	return `R${es.ir_d},R${es.ir_a},R${es.ir_b}`;
    } else if (es.instrFmtStr==="RX") {
	return `R${es.ir_d},${arith.wordToHex4(es.instrDisp)}[R${es.ir_a}]`;
    } else {
	return "?";
    }
}

function showEffect (es,i) {
    console.log(`showEffect ${i}`);
    if (es.instrEffect[i]) {
	let [dest,idx,val,name] = es.instrEffect[i];
	if (dest==="R") {
	    console.log (`showEffect ${i} ${es.instrEffect[i]}`);
//	    console.log (`showEffect ${i}  ${dest} ${idx} := ${val});
	    return `${name} := ${arith.wordToHex4(val)}`;
	} else if (dest==="M") {
	    console.log ("showEffect M");
	    return `M[${arith.wordToHex4(idx)}]:=${arith.wordToHex4(val)}`;
	}
    } else { return ""; }
}

//------------------------------------------------------------------------------
// Highlighting current and next instruction in processor assembly listing
//------------------------------------------------------------------------------

// The assembler provides an array of source lines, which it passes on
// to the linker and thence to the emulator.  There are two strings
// for each source line: one contains <span> elements to enable the
// fields to be highlighted, just as in the assembly listing.  The
// other omits these elements, so the entire line can be highlighted
// to indicate (with just one color for the line) the instruction that
// has just executed and the instruction that will be executed next.

// The assembler produces listing lines with <span> elements to allow
// the fields of the line to be highlighted. These are stored in
// listingHighlightedFields.  However, the emulator highlights an
// entire listing line to indicate the instruction that is currently
// executing, or that will execute next.  In order to prevent the
// highlighting of fields from overriding the highlighting of the
// current/next instruction, that is done using listingPlain.

// Given address a, the corresponding source statement is
//   i = es.curAsmap[a]
//   if i=null then no source is avaiable
//     else currentModule.asmStmt[i].srcLine


function showListingParameters (es) {
    console.log ('showListingParameters'
		 + ' es.curInstrAddr=' + es.curInstrAddr
		 + ' es.curInstrLineNo=' + es.curInstrLineNo
		 + ' es.nextInstrAddr=' + es.nextInstrAddr
		 + ' es.nextInstrLineNo=' + es.nextInstrLineNo);
}

// temp for testing...
function fooby (i) {
    console.log (`${emulatorState.asmListingCurrent[i]}`);
}

// Prepare the running listing before starting instructionby removing
// any existing highlighting of current and next instruction

function prepareListingBeforeInstr (es) {
    console.log ('prepareListingBeforeInstr');
    if (es.curInstrLineNo >= 0) {
	es.asmListingCurrent[es.curInstrLineNo] =
	    es.asmListingDec[es.curInstrLineNo];
    }
    if (es.nextInstrLineNo >= 0) {
	es.asmListingCurrent[es.nextInstrLineNo] =
	    es.asmListingDec[es.nextInstrLineNo];
    }
    es.curInstrLineNo = -1;
    es.nextInstrLineNo = -1;
    showListingParameters(es);
}

// As it executes an instruction, the emulator sets curInstrAddr and
// nextInstrAddr.  After the instruction has finished, these
// instructions are highlighted in the listing

function highlightListingAfterInstr (es) {
    console.log ('highlightListingAfterInstr');
    console.log ('  curInstrAddr = ' + es.curInstrAddr);
    console.log ('  nextInstrAddr = ' + es.nextInstrAddr);

    es.curInstrLineNo = es.curAsmap[es.curInstrAddr];
    console.log ('  curInstrLineNo = ' + es.curInstrLineNo);
    if (es.curInstrLineNo >= 0) {
	com.highlightListingLine (es, es.curInstrLineNo, "CUR");
    }

    es.nextInstrLineNo = es.curAsmap[es.nextInstrAddr];
    console.log ('  nextInstrLineNo = ' + es.nextInstrLineNo);
    if (es.nextInstrLineNo >= 0) {
	com.highlightListingLine (es, es.nextInstrLineNo, "NEXT");
    }

    if (memDisplayModeFull) {
	highlightListingFull (es)
    } else {
	highlightListingFull (es)    // temp ?????
    }
}

function highlightListingFull (es,m) {
    console.log ('highlightListingFull');
    setProcAsmListing (es);
    let xa = es.curInstrLineNo - asmScrollOffsetAbove;
    xa = xa < 0 ? 0 : xa;
    let scrollOffset = xa * pxPerChar;
    console.log ('curInstrLineNo = ' + es.curInstrLineNo
		 + '  scrollOffset = ' + scrollOffset);
    procAsmListingElt.scroll (0, scrollOffset);
//    let curline = procAsmListingElt.getElementById('CUR');
//    curline.scrollIntoView();
}


function setProcAsmListing (es) {
    console.log ('setProcAsmListing');
    let xs = "<pre><code class='HighlightedTextAsHtml'>"
    	+ es.asmListingCurrent.join('\n')
	+ "</code></pre>";
    document.getElementById('ProcAsmListing').innerHTML = xs;
}

//------------------------------------------------------------------------------

// function clearCtlRegs () {
// }

export function procStep(es) {
    console.log ('procStep');
    if (es.procStatus===Paused) { setProcStatus (es,Ready); }
    if (es.procStatus===Ready) {
	execInstrPrepareFull (es);
	executeInstruction (es);
        console.log ("procStep: executeInstruction finished");
        execInstrPostDisplay (es);
    }
}

export function procRun(es) {
    console.log ("procRun");
    if (es.procStatus===Paused) { setProcStatus (es,Ready); }
    execRunPrepare (es);
    instructionLooper (es);
    runInstrPostDisplay (es);
}

export function procReset(es) {
    console.log ("reset the processor");
    setProcStatus (es,Reset);
    resetRegisters ();
    refreshRegisters ();
    memClear ();
    memClearAccesses ();
    memDisplay ();
    document.getElementById('ProcAsmListing').innerHTML = "";
    clearInstrDecode (es);
    refreshInstrDecode (es);
    es.nInstructionsExecuted = 0;
    document.getElementById("nInstrExecuted").innerHTML = es.nInstructionsExecuted;
}

//------------------------------------------------------------------------------
// Controlling instruction execution
//------------------------------------------------------------------------------

// Run instructions repeatedly until a stopping condition arises.
// Yield control each iteration to avoid blocking the user interface,
// particularly the manual timer interrupt button.

function instructionLooper (es) {
    if (es.procStatus===Ready) {
	console.log ('instructionLooper');
        execInstrPrepareFast (es);
	executeInstruction (es);
        if (es.procStatus===Halted) {
	    console.log ("looper: halted");
            execInstrPostDisplay (es);
        } else if (es.procStatus===Paused) {
        } else if (es.breakEnabled && pc.get() === es.breakPCvalue) {
	    console.log ("looper: breakpoint");
            setProcStatus (es,Paused);
            displayFullState();
	} else {
	    setTimeout (function () {instructionLooper(es)});
        }
    }
    console.log ('instructionLooper terminated');
}

// The Pause button stops the instruction looper and displays the state.

export function procPause(es) {
    console.log ("procPause");
    setProcStatus (es,Paused);
    refreshRegisters();
    regShowAccesses();
    memRefresh();
    memShowAccesses ();
    memDisplayFull();
    showInstrDecode (es);
    highlightListingAfterInstr (es);
}

//---------------------------------------------------------------------------
// Breakpoint
//---------------------------------------------------------------------------

// Temporary: enter a hex constant e.g. $02c9 into the text area and
// click Refresh.  The emulator will break when the pc reaches this
// value.  Spaces before the constant are not allowed, and the $ is
// required.  Later this will be replaced by a richer language for
// specifying the break condition.


export let breakDialogueVisible = false;

export function procBreakpoint () {
    console.log ("procBreakpoint");
    document.getElementById("BreakDialogue").style.display
	= breakDialogueVisible ? "none" : "block";
    breakDialogueVisible = !breakDialogueVisible;
}

export function hideBreakDialogue () {
    document.getElementById("BreakDialogue").style.display = "none";
    breakDialogueVisible = false;
}


function breakRefresh (es) {
    console.log ("breakRefresh");
    let x = document.getElementById('BreakTextArea').value;
    if (x.search(hexParser) == 0) {
	let w = arith.hex4ToWord (x.slice(1));
	es.breakPCvalue = w;
	console.log (`breakPCvalue = + ${w}`);
    } else {
	console.log (`breakRefresh cannot parse + x`);
    }
}

function breakEnable (es) {
    console.log ("breakEnable");
    es.breakEnabled = true;
    console.log (`breakEnable ${es.breakPCvalue}`);
}

function breakDisable (es) {
    console.log ("breakDisable");
    es.breakEnabled = false;
}

function breakClose () {
    console.log ("breakClose");
    hideBreakDialogue ();
}

//------------------------------------------------------------------------------
// Wrapper around instruction execution
//------------------------------------------------------------------------------

// For single stepping, we want to keep display of registers and
// memory up to date and show access by highlighting the fetched and
// updated locations.  For Run mode, we want to avoid updating the
// memory continuosly, although it may be useful to keep the register
// displays updated.

// The strategy is: (1) for stepping, there is a function to prepare
// before executing an instruction, and another to update the displays
// after execution, with the expectation that the user will spend some
// time looking at the displays before steppign again.  (2) For Run,

// (for running) Prepare the displays before running sequence of
// instructions (the Run button).

function execRunPrepare (es) {
    console.log ("execRunPrepare");
    regClearAccesses ();             // remove register highlighting, clear logs
    regShowAccesses()
    memClearAccesses ();             // remove mem highlighting, clear logs
    memRefresh();
    memDisplay ();                   // refresh memory display
    clearInstrDecode (es);           // remove decoding of last instruction
    prepareListingBeforeInstr (es);  //remove any instruction highlighting
    setProcAsmListing (es);
}

// (for runing) Prepare to execute an instruction while in Run mode: clear
// the logs but don't keep memory updated.  This assumes that the
// displays have already been put into a suitable state fur the
// duration of the run.

function execInstrPrepareFast (es) {
    console.log ("execInstrPrepareFast");
// don't refresh the registers (no regClearAccesses), just clear logs
    regFetched = [];  // clear reg fetch log
    regStored = [];   // clear reg update log
// don't refresh memory (no memClearAccesses), just clear logs
    memFetchInstrLog = [];
    memFetchDataLog = [];
    memStoreLog = [];
// don't need to clear instrDecode as we aren't updating it in fast mode
// don't need to prepareListing as we aren't updating it in fast mode
}

// (for stepping) Prepare to execute an instruction with full logging:
// clear the logs and update all the displays.

function execInstrPrepareFull (es) {
    console.log ("execInstrPrepareFast");
    regClearAccesses ();
    memClearAccesses ();
    clearInstrDecode (es);
    prepareListingBeforeInstr (es);
}

// (for stepping) Display the effects of the instruction

function execInstrPostDisplay (es) {
    if (es.procStatus===Halted) { // do a full display
        refreshRegisters ();
	regShowAccesses()
	memShowAccesses();
        memRefresh();
        memShowAccesses();
        memDisplayFull ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    } else if (es.procStatus===Paused) {
    } else { // do normal display
	regShowAccesses()
	memShowAccesses();
	memDisplay ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    }
}

function runInstrPostDisplay (es) {
    console.log("runInstrPostDisplay");
    memClearAccesses ();
    memRefresh();
    memDisplayFull ();
    clearRegisterHighlighting ();
    refreshRegisters ();
}

// Prepare to execute an instruction by clearing the buffers holiding
// log information.

function prepareExecuteInstruction (es) {
    console.log ("prepareExecuteInstruction");

// Preparations before the instruction
    regClearAccesses ();
    memClearAccesses ();
    memDisplay ();
    clearInstrDecode (es);
    prepareListingBeforeInstr (es);
}

// Final actions after the instruction
function finalizeExecuteInstruction (es) {
    if (es.procStatus===Halted) {
        console.log ("procStep: execute instruction: halted")
	regShowAccesses()
        memRefresh();
	memShowAccesses();
	memDisplayFull ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    } else if (es.breakEnabled && pc.get() === es.breakPCvalue) {
	console.log ("Breakpoint");
        setProcStatus (es,Paused);
        memRefresh();
        displayFullState();
    } else {
	regShowAccesses()
	memShowAccesses();
	memDisplay ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    }

    console.log ("runOneInstruction: end");
}

//------------------------------------------------------------------------------
// Machine language semantics
//------------------------------------------------------------------------------

// Execute one instruction and return

// kludge check... remove
//    if ((getBitInReg (statusreg,intEnableBit) ? (mr ?req.get() & mask.get()) : 0) != 0) {
//	while (i<16 && getBitInReg(req,i)==0 && getBitInReg(mask,i)==0) {

// console.log (`interrupt priority search mask=${wordToHex4(mask.get())} req=${wordToHex4(req.get())}`);
//	    console.log(`find interrupt trying i=${i} r=${getBitInReg(req,i)} m=${getBitInReg(mask,i)}`);

function executeInstruction (es) {
    console.log ('executeInstruction');
    es.nInstructionsExecuted++;
    document.getElementById("nInstrExecuted").innerHTML = es.nInstructionsExecuted;

// Check for interrupt
    let mr = mask.get() & req.get();
//    console.log (`interrupt mr = ${arith.wordToHex4(mr)}`);
    if (arith.getBitInRegBE (statusreg,arch.intEnableBit) && mr) {
	let i = 0; // interrupt that is taken
	while (i<16 && arith.getBitInWordBE(mr,i)==0) { i++ };
	console.log (`\n*** Interrupt ${i} ***`);
	ipc.put(pc.get());           // save the pc
	istat.put(statusreg.get());   // save the status register
//	console.log (`ipc=${ipc.get()}`);
//	console.log (`req=${arith.wordToHex4(req.get())}`);
	arith.clearBitInRegBE (req,i);        // clear the interrupt that was taken
//	console.log (`req=${arith.wordToHex4(req.get())}`);
//	console.log (`pc=${arith.wordToHex4(pc.get())}`);
//	console.log (`vect=${arith.wordToHex4(vect.get())} i=${i}`);
	pc.put (vect.get() + 2*i);  // jump to handler
//	console.log (`pc=${arith.wordToHex4(pc.get())}`);
        // Disable interrupts and enter system state
//	console.log (`status=${arith.wordToHex4(statusreg.get())}`);
	statusreg.put (statusreg.get()
		       & arith.maskToClearBitBE(arch.intEnableBit)
		       & arith.maskToClearBitBE(arch.userStateBit));
//	console.log (`statusreg=${arith.wordToHex4(statusreg.get())}`);
//	regShowAccesses();
	return;
    };

// No interrupt, go ahead with instruction
    es.curInstrAddr = pc.get();
    instrCode = memFetchInstr (es.curInstrAddr);
    ir.put (instrCode);
    es.nextInstrAddr = arith.binAdd (es.curInstrAddr, 1);
    pc.put (es.nextInstrAddr);
//    console.log('pc = ' + arith.wordToHex4(pc.get()) + ' ir = ' + arith.wordToHex4(instr));
    let tempinstr = ir.get();
    es.ir_b = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_a = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_d = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_op = tempinstr & 0x000f;
    console.log('instr fields = ' + es.ir_op + ' ' + es.ir_d + ' ' + es.ir_a + ' ' + es.ir_b);
    es.instrFmtStr = "RRR";  // Replace if opcode expands to RX or EXP
    es.instrOpStr = arch.mnemonicRRR[es.ir_op]  // Replace if opcode expands
    dispatch_RRR [es.ir_op] (es);
}

// RRR instructions have three specified registers, and may also use
// the condition code cc, which is R15.

// a = reg[ir_sa]
// b = reg[ir_sb]
// d = reg[ir_d]
// c = reg[15] (condition code)

//  There are some variations in how different instructions use the
//  register operands, and there is a generic function for each
//  variation (e.g rrd) which takes a function argument (e.g add) that
//  performs the specific operation for that instruction.  The generic
//  functions access the registers, and the operation functions
//  perform the calculations.  This approach ensures that the correct
//  registers are highlighted: for example, an instruction that
//  doesn't need rb will not highlight rb.

// Convention: rr refers to the normal operands a and b; c refers to
// the condition code which is in R15; d refers to the destination
// register.

// rrd   -- use ra and rb as operands, place result in rd
// rrdc  -- use ra and rb as operands, place results in rd and cc
// crrdc -- use cc, ra and rb as operands, place results in rd and cc
// rd    -- use ra as operand, place result in rd, ignore rb and cc
// rrc   -- use ra and rb as operands, place result in cc, ignore rd
// rrrc  -- use ra, rb, and cc as operands, place results in rd and cc
// trap  -- perform trap; instruction ignores all registers but OS may use them


/* Example of a lambda expression and a curried lambda
  const foobar = (x) => (y) => x+y;
  let baz = foobar (5);
  let bar = foobar (41);
  let abc = foobar (7) (50);
*/

// Apply f to a and b, load primary result into d, and load secondary
// result into c (e.g. add)

const rrdc = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let [primary, secondary] = f (a,b);
    regFile[es.ir_d].put(primary);
    if (es.ir_d<15) { regFile[15].put(secondary) }
}

// Apply f to a and load primary result into d (e.g. inv)

const rd = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let primary = f (a);
    regFile[es.ir_d].put(primary);
}

// Apply f to a and b, and load primary result into d (e.g. cmplt)

const rrd = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let primary = f (a,b);
    regFile[es.ir_d].put(primary);
}

// Apply f to a and b, and load primary result into c (e.g. cmp)

const rrc = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let cc = f (a,b);
    console.log (`rrc cc=${cc}`);
    regFile[15].put(cc);
}

// Apply f to c, a and b, load primary result into d, and load
// secondary result into c (e.g. addc)

const crrdc = (f) => (es) => {
    let c = regFile[15].get();
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let [primary, secondary] = f (c,a,b);
    regFile[es.ir_d].put(primary);
    if (es.ir_d<15) { regFile[15].put(secondary) }
}

const op_trap = (es) => {
    let code = regFile[es.ir_d].get();
    console.log (`trap code=${code}`);
    if (code===0) { // Halt
	console.log ("Trap: halt");
        setProcStatus (es,Halted);
        refreshRegisters();
        memRefresh();
    } else if (code==1) { // Read
        trapRead(es);
    } else if (code==2) { // Write
        trapWrite(es);
    } else { // Undefined trap is nop
        console.log (`trap with unbound code = ${code}`)
    }
}

// trapRead performs in input from the contents of the input buffer: a
// = address of the buffer, and b = size of buffer.  If the number of
// characters entered by the user into the input buffer exceeds b,
// then only b characters are stored into the buffer, and thse b
// characters are removed from the input buffer.  The user types input
// into the IOinputBuffer, and the input operation obtains up to b
// characters, stores them into memory starting from location a, and
// removes those characters from IOinputBuffer.  Two registers are
// updated: Ra := address just after last word stored, Rb := numbr of
// characters (words) read.

function trapRead (es) {
    let inputElt = document.getElementById("IOinputBuffer")
    let xs = inputElt.value;
    let a = regFile[es.ir_a].get(); // buffer address
    let b = regFile[es.ir_b].get(); // buffer size
    let n = xs.length; // number of chars available in buffer
    let m = n <= b ? n : b; // number of chars actually input
    let xs2 = xs.substring (m,n); // excess chars from input window are not used
    let ys = xs.substring (0,m);  // input string to store into memory
    let charcode = 0;
    document.getElementById("IOinputBuffer").value = xs2;
    console.log (`Read: a=${a} b=${b} m=${m} >>> /${ys}/`);
    console.log (`Read: n=${n} m=${m}`);
    console.log (`Read: xs2=/${xs2}/ ys=/${ys}/`);
    for (let i = 0; i<m; i++) {
	charcode = ys.charCodeAt(i);
	memStore (a, charcode);
	console.log (`Read: mem[${a}] := ${charcode}`);
	a++;
    }
    regFile[es.ir_a].put(a); // just after last address stored
    regFile[es.ir_b].put(m); // number of chars actually input
    ioLogBuffer += highlightField(ys,"READ");   // display chars that were read
    refreshIOlogBuffer ();
    inputElt.value = xs2; // leave unread characters in input buffer
}

// Write b characters starting from address a
function trapWrite (es) {
    let a = regFile[es.ir_a].get(); // buffer address
    let b = regFile[es.ir_b].get(); // buffer size
    let xs = "";
    for (let i = 0; i<b; i++) {
	xs += String.fromCharCode(memFetchData(a));
	a++
    }
    console.log (`Write a=${a} b=${b} >>> /${xs}/`);
    ioLogBuffer += xs;
    console.log (ioLogBuffer);
    refreshIOlogBuffer();
}

const handle_rx = (es) => {
    console.log ('handle rx' + es.ir_b);
    es.instrFmtStr = "RX";
    dispatch_RX [es.ir_b] (es);
}

const handle_EXP = (es) => {
    console.log (`handle_EXP ${es.ir_d} ${es.ir_a} ${es.ir_b}`);
    es.instrFmtStr = "EXP";
    let code = 16*es.ir_a + es.ir_b;
    if (code < limitEXPcode) {
	console.log (`handle_EXP dispatch code=${code} ${es.ir_d} ${es.ir_a} ${es.ir_b}`);
	dispatch_EXP [code] (es);
    } else {
	console.log (`EXP bad code ${arith.wordToHex4(code)}`);
    }
}

const dispatch_RRR =
      [ rrdc (arith.op_add),     // 0
        rrdc (arith.op_sub),     // 1
        rrdc (arith.op_mul),     // 2
        rrdc (arith.op_div),     // 3
        rrc (arith.op_cmp),      // 4
        rrd (arith.op_cmplt),    // 5
        rrd (arith.op_cmpeq),    // 6
        rrd (arith.op_cmpgt),    // 7
        rd  (arith.op_inv),      // 8
        rrd (arith.op_and),      // 9
        rrd (arith.op_or),       // a
        rrd (arith.op_xor),      // b
        crrdc (arith.op_addc),   // c
        op_trap,           // d
        handle_EXP,         // e
        handle_rx ]        // f
	
// Some instructions load the primary result into rd and the secondary
// into cc (which is R15).  If the d field of the instruction is 15,
// the primary result is loaded into rd and the secondary result is
// discarded.

// It is legal for the destiation register rd to be R0.  However, R0
// always contains 0; i.e. any time it is fetched the result is 0.  In
// effect, any value loaded into R0 is discarded.  Any implementation
// that satisfies these rules is conformant.  Any of the following
// approaches is acceptable: (1) don't load into R0; (2) go ahead and
// load into R0 but produce 0 on readout; (3) don't even implement R0
// with state bits, but ensure that its readout produces 0.

const rx = (f) => (es) => {
    console.log('rx');
    es.instrOpStr = arch.mnemonicRX[es.ir_b];
    es.instrDisp = memFetchInstr (pc.get());
    adr.put (es.instrDisp);
    es.nextInstrAddr = arith.binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    //    es.ea = arith.binAdd (regFile[es.ir_a].get(), adr.get());
    es.ea = arith.binAdd (regFile[es.ir_a].get(), es.instrDisp);
    es.instrEA = es.ea;
    console.log (`rx ea, disp=${arith.wordToHex4(es.instrDisp)}`);
    console.log (`rx ea, idx=${arith.wordToHex4(regFile[es.ir_a].get())}`);
    console.log('rx ea = ' + arith.wordToHex4(es.ea));
    f (es);
}

const dispatch_RX =
    [ rx (rx_lea),       // 0
      rx (rx_load),      // 1
      rx (rx_store),     // 2
      rx (rx_jump),      // 3
      rx (rx_jumpc0),    // 4
      rx (rx_jumpc1),    // 5
      rx (rx_jumpf),     // 6
      rx (rx_jumpt),     // 7
      rx (rx_jal),       // 8
      rx (rx_nop),       // 9
      rx (rx_nop),       // a
      rx (rx_nop),       // b
      rx (rx_nop),       // c
      rx (rx_nop),       // d
      rx (rx_nop),       // e
      rx (rx_nop) ];     // f

function rx_lea (es) {
    console.log('rx_lea');
    regFile[es.ir_d].put(es.ea);
}

function rx_load (es) {
    console.log('rx_load');
    regFile[es.ir_d].put(memFetchData(es.ea));
}

function rx_store (es) {
    console.log('rx_store');
    memStore (es.ea, regFile[es.ir_d].get());
}

function rx_jump (es) {
    console.log('rx_jump');
    es.nextInstrAddr = es.ea;
    pc.put(es.nextInstrAddr);
}

function rx_jumpc0 (es) {
    console.log('rx_jumpc0');
    let cc = regFile[15].get();
    if (extractBitBE (cc,es.ir_d)===0) {
	es.nextInstrAddr = es.ea;
	pc.put(es.nextInstrAddr);
    }
}

function rx_jumpc1 (es) {
    console.log('rx_jumpc1');
    let cc = regFile[15].get();
    if (extractBitBE (cc,es.ir_d)===1) {
	es.nextInstrAddr = es.ea;
	pc.put(es.nextInstrAddr);
    }
}

function rx_jumpf (es) {
    console.log('rx_jumpf');
    if (! arith.wordToBool (regFile[es.ir_d].get())) {
	es.nextInstrAddr = es.ea;
	pc.put (es.nextInstrAddr);
    }
}

function rx_jumpt (es) {
    console.log('rx_jumpt');
    if (arith.wordToBool (regFile[es.ir_d].get())) {
	es.nextInstrAddr = es.ea;
	pc.put (es.nextInstrAddr);
    }
}

function rx_jal (es) {
    console.log('rx_jal');
    regFile[es.ir_d].put (pc.get());
    es.nextInstrAddr = es.ea;
    pc.put (es.nextInstrAddr);
}

function rx_nop (es) {
    console.log ('rx_nop');
}

// EXP format


function exp1_nop (es) {
    console.log ('exp1_nop');
}

function exp1_rfi (es) {
    console.log ('exp1_rfi');
    statusreg.put (istat.get());
    pc.put (ipc.get());
}

function exp2_save (es) {
    console.log ('exp2_save');
    sr_looper ((a,r) => memStore (a, regFile[r].get()),
               regFile[es.ir_d].get()+es.field_gh, es.field_e, es.field_f)
}

function exp2_restore (es) {
    console.log ('exp2_restore');
    sr_looper ((a,r) => regFile[r].put(memFetchData(a)),
               regFile[es.ir_d].get()+es.field_gh, es.field_e, es.field_f)
}

function sr_looper (f,addr,first,last) {
    let done = false;
    let r = first;
    while (!done) {
        console.log (`save looper addr=${addr} r=${r}`);
        f(addr,r);
        done = r==last;
        addr += 1;
        r = bininc4(r);
    }
}

function bininc4 (x) { return x >= 15 ? 0 : x+1 }


// temp like put
function exp2_getctl (es) {
    console.log ('exp2_getctl');
    let cregn = es.field_f;
    let cregidx = cregn + ctlRegIndexOffset; // init in gui.js
    console.log (`exp_getctl cregn=${cregn} cregidx=${cregidx}`);
    regFile[es.field_e].put(register[cregidx].get());
}
function exp2_putctl (es) {
    console.log ('putctl');
    let cregn = es.field_f;
    let cregidx = cregn + ctlRegIndexOffset; // init in gui.js
    console.log (`putctl src e==${es.field_e} val=${regFile[es.field_e].get()}`);
    console.log (`putctl dest f=${es.field_f} cregn=${cregn} cregidx=${cregidx}`);
    register[cregidx].put(regFile[es.field_e].get());
    register[cregidx].refresh();
}

function exp2_execute (es) {
    console.log ("exp2_execute");
}

function exp2_push (es) {
    console.log ('exp2_push');
 //   console.log (`e=${es.field_e} f=${es.field_f} g=${es.field_g} h=${es.field_h} `);
    let top = regFile[es.field_f].get();
    let last = regFile[es.field_g].get();
//    console.log (`push: top=${top} last=${last}`);
    if (top < last) {
        top += 1;
        memStore (top, regFile[es.field_e].get());
        regFile[es.field_f].put(top);
    } else {
        console.log ("push: stack overflow")
    }
}

function exp2_pop (es) {
    console.log ('exp2_pop');
    let top = regFile[es.field_f].get();
    let first = regFile[es.field_g].get();
    if (top >= first) {
        regFile[es.field_e].put(memFetchData(top));
        top -= 1;
        regFile[es.field_f].put(top);
    } else {
        console.log ("pop: stack underflow")
    }
}

function exp2_top (es) {
    console.log ('exp2_top');
    let top = regFile[es.field_f].get();
    let first = regFile[es.field_g].get();
    if (top >= first) {
        regFile[es.field_e].put(memFetchData(top));
        regFile[es.field_f].put(top);
    } else {
        console.log ("top: stack underflow")
    }
}

function exp2_shiftl (es) {
    console.log ("exp2_shiftl");
    let x = regFile[es.field_e].get();
    let k = es.field_g;
    let result = x << k;  // logical shift
    console.log (`shiftl ${arith.wordToHex4(x)} left by ${k} bits => ${arith.wordToHex4(result)}`);
    regFile[es.ir_d].put(result);
}

function exp2_shiftr (es) {
    console.log ("exp2_shiftr");
    let x = regFile[es.field_e].get();
    let k = es.field_g;
    let result = x >>> k;  // logical shift
    console.log (`shiftr ${arith.wordToHex4(x)} right by ${k} bits => ${arith.wordToHex4(result)}`);
    regFile[es.ir_d].put(result);
}

// Instruction fields for extract and extractii:
//   d = destination
//   e = operand
//   (g,h) = field

function exp2_extract (es) {
    console.log ('exp2_extract');
    let x = regFile[es.field_e].get();
    let i = es.field_g;
    let j = es.field_h;
    let a = (x << i) & 0xffff;
    let b = a >> (15 - (j-i));
    regFile[es.ir_d].put(b);
}

function exp2_extracti (es) {
    console.log ('exp2_extracti');
    let x = wordInvert (regFile[es.field_e].get());
    let i = es.field_g;
    let j = es.field_h;
    let a = (x << i) & 0xffff;
    let b = a >> (15 - (j-i));
    regFile[es.ir_d].put(b);
}

function foobar (g,h) {
    let x = 0x0000;  // operand
    let i = g;  // field start, there will be this many 0s to left of field
    let j = h;  // field end
    let jjj = 15 - j; // there will be this many 0s to right of field
    let a = 0xffff;
    let b = (a << jjj) & 0xffff; // get jjj 0s on right side
    let c = b >> i; // get i 0s on left side
    console.log (`x=${x} i=${i} j=${j} a=${arith.wordToHex4(a)}`);
    console.log (`a=${arith.wordToHex4(a)} b=${arith.wordToHex4(b)} c=${arith.wordToHex4(c)}`);
    return 0
}

// Instruction fields for inject and injecti:
//   d = destination
//   e = operand
//   (g,h) = field


function exp2_inject (es) {
    console.log ('exp2_inject');
    let e = regFile[es.field_e].get(); // inject into this word
    let f = regFile[es.field_f].get(); // word contains the field to be injected
    let g = es.field_g;  // start bit index of field
    let h = es.field_h;  // end bit index of field
    let fieldsize = h-g+1;
    let shrdist = 15-h+g; // shift ffff right to get right-adjusted field
    let shldist = 15-h;   // shift left to put field into position
    let radjustedField = 0xffff >>> shrdist;
    let x = f & radjustedField; // value to be injected
    let field = radjustedField << shldist; // 1s in the field
    let fieldInv = (~field) & 0xffff; // mask to clear field in e
    let result = (e & fieldInv) | (x << shldist) ;
    regFile[es.ir_d].put(result);
}

//    console.log (`inject e=${wordToHex4(e)}`);
//    console.log (`inject f=${wordToHex4(f)}`);
//    console.log (`inject g=${g} h=${h}`);
//    console.log (`inject fieldsize=${fieldsize}`);
//    console.log (`inject shrdist=${shrdist} shldist=${shldist}`);
//    console.log (`inject field=${wordToHex4(field)}`);
//    console.log (`inject fieldInv=${wordToHex4(fieldInv)}`);
//    console.log (`inject result=${wordToHex4(result)}`);

function exp2_injecti (es) {
    console.log ('exp2_injecti');
    let e = regFile[es.field_e].get(); // inject into this word
    let f = regFile[es.field_f].get(); // word contains the field to be injected
    let g = es.field_g;  // start bit index of field
    let h = es.field_h;  // end bit index of field
    let fieldsize = h-g+1;
    let shrdist = 15-h+g; // shift ffff right to get right-adjusted field
    let shldist = 15-h;   // shift left to put field into position
    let radjustedField = 0xffff >>> shrdist;
    let x = (~f) & radjustedField; // value to be injected
    let field = radjustedField << shldist; // 1s in the field
    let fieldInv = (~field) & 0xffff; // mask to clear field in e
    let result = (e & fieldInv) | (x << shldist) ;
    regFile[es.ir_d].put(result);
}

function exp2_logicw (es) {
    console.log ('exp2_logicw');
    let x = regFile[es.field_e].get(); // operand 1
    let y = regFile[es.field_f].get(); // operand 2
    let fcn = es.field_g; // logic function
    let result = applyLogicFcnWord (fcn,x,y); // fcn x y
    regFile[es.ir_d].put(result);
}


function exp2_logicb (es) {
    console.log (`exp2_logicb`);
    let xw = regFile[es.field_e].get(); // operand 1
    let yw = regFile[es.field_f].get(); // operand 2
    let i = es.field_h; // bit index
    let x = arith.getBitInWordBE(xw,i);
    let y = arith.getBitInWordBE(yw,i);
    let fcn = es.field_g; // logic function
    let bresult = applyLogicFcnBit (fcn,x,y); // bit result
    let w = putBitInWord (regFile[es.ir_d].get(), i, bresult); // word result
    regFile[es.ir_d].put(w);
}


// EXP format instructions that use only one word
const exp1 = (f) => (es) => {
    let expCode = 16*es.ir_a + es.ir_b;
//    es.instrOpStr = mnemonicEXP[expCode];  ???????????????????????????
    es.instrDisp = memFetchInstr (pc.get());
    es.nextInstrAddr = arith.binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    console.log(`EXPF code=${expCode} d=${es.ir_d} es.instrDisp=${arith.wordToHex4(es.instrDisp)}`);
    f(es);
}

// EXP format instructions that require a second word
const exp2 = (f) => (es) => {
    console.log (`exp2 format instruction`);
    let expCode = 16*es.ir_a + es.ir_b;
    //    es.instrOpStr = mnemonicEXP[expCode];
    es.instrOpStr = "EXPtodo";  // ?????????????????????????????????
    es.instrDisp = memFetchInstr (pc.get());
    adr.put (es.instrDisp);
    es.nextInstrAddr = arith.binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    let tempinstr = es.instrDisp;
    es.field_gh = tempinstr & 0x00ff;
    es.field_h = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_g = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_f = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_e = tempinstr & 0x000f;
    console.log(`EXPF code=${expCode} d=${es.ir_d} es.instrDisp=${arith.wordToHex4(es.instrDisp)}`);
    f(es);
}


const dispatch_EXP =
      [ exp1 (exp1_rfi),       // 0
        exp1 (exp1_nop),       // 1
        exp1 (exp1_nop),       // 2
        exp1 (exp1_nop),       // 3
        exp1 (exp1_nop),       // 4
        exp1 (exp1_nop),       // 5
        exp1 (exp1_nop),       // 6
        exp1 (exp1_nop),       // 7
        
        exp2 (exp2_save),      // 8
        exp2 (exp2_restore),   // 9
        exp2 (exp2_getctl),    // 10
        exp2 (exp2_putctl),    // 11
        exp2 (exp2_execute),   // 12
        exp2 (exp2_push),      // 13
        exp2 (exp2_pop),       // 14
        exp2 (exp2_top),       // 15

        exp2 (exp2_shiftl),    // 16
        exp2 (exp2_shiftr),    // 17
        exp2 (exp2_extract),   // 18
        exp2 (exp2_extracti),  // 19
        exp2 (exp2_inject),    // 20
        exp2 (exp2_injecti),   // 21
        exp2 (exp2_logicw),    // 22
        exp2 (exp2_logicb)     // 23
    ]
const limitEXPcode = dispatch_EXP.length;  // any code above this is nop


//------------------------------------------------------------------------------
// Test pane
//------------------------------------------------------------------------------

// From emulator.js

// In the mem display, the formatting is ok when the container
// specifies the style class.  However, when <pre> ... </pre> are
// added around the text, the font and size are wrong and the
// specified style is ignored.  Perhaps <pre> has an inappropriate
// default style that overrides the existing font.  Solution is to use
// <pre class="HighlightedTextAsHtml"> but don't put it inside a div
// with HighlightedTExtAsHtml


function testpane1() {
    console.log ('testpane 1 clicked')
    let xs = ["<pre class='HighlightedTextAsHtml'>", 'line 1 text',
	      "<span class='CUR'>this is line 2 text</span>",
	      'and finally line 3', '</pre>'];
    console.log ('xs = ' + xs);
    let ys = xs.join('\n');
    console.log ('ys = ' + ys);

    let qs = ys;
    console.log ('qs = ' + qs);
    document.getElementById('TestPaneBody').innerHTML = qs;
}

function testpane2 () {
    console.log ('testpane 2 clicked');
}

function testpane3 () {
    console.log ('testpane 3 clicked');
}
