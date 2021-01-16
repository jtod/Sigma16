// Sigma16: emulator.mjs
// Copyright (C) 2021 John T. O'Donnell
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
import * as asm from './assembler.mjs';
import * as link from './linker.mjs';

// The mode determines the quantity and destination of output

export const Mode_GuiDisplay = 100
export const Mode_Console    = 200
export const Mode_Quiet      = 300

// Number of header lines in the listing before the source lines begin
const listingLineInitialOffset = 1;


// Time

export function clearTime (es) {
    const now = new Date ()
    es.startTime = now.getTime ()
    document.getElementById("PP_time").innerHTML = `0ms`
}

//------------------------------------------------------------------------------
// Booter 
//------------------------------------------------------------------------------

// Prepare assembly listing when executable is booted

export function initListing (m,es) {
    es.curInstrAddr = 0;
    es.curInstrLineNo = -1;  // -1 indicates no line has been highlighted
    es.nextInstrAddr = 0;
    es.nextInstrLineNo = es.metadata.getSrcIdx (es.nextInstrAddr)
        + listingLineInitialOffset;
    
    com.highlightListingLine (es, es.nextInstrLineNo, "NEXT");
    setProcAsmListing (es,m);
}

// Should check the operation, implement org, and provide suitable
// error messages, but that's for later.  For now, just assume it is
// hexdata with valid argument

function linkerBootLine (es,m,i,x) {
    com.mode.devlog (`linkerBootLine i=${i}`)
    let y = parseAsmLine (m,i,x);
//    printAsmLine (y);
    let w = y.fieldOperands;
    let n =  arith.hex4ToWord(w);
//    com.mode.devlog('linkerBootLine ' + w + ' = ' + n);
    com.mode.devlog('linkerBootLine ' + i + ' ---' + x + '--- = ' + n);
    updateMem2(bootCurrentLocation,n);
    bootCurrentLocation++;
}

//-------------------------------------------------------------------------------
// Booter
//-------------------------------------------------------------------------------

// Find the executable; it may come from assembler (object code) or
// linker (executable code).

export function obtainExecutable () {
    let m = st.env.getSelectedModule();
    let exe = m.executable ? m.executable : m.objMd;
    if (exe) {
        com.mode.devlog (`Found executable for selected module`);
        return exe;
    } else {
        com.mode.devlog (`Cannot find executable`);
        return null;
    }
}

export function boot (es) {
    com.mode.trace = true;
    com.mode.devlog ("boot");
    com.mode.devlog (`current emulator mode = ${es.mode}`)
    st.resetSCB (es)
    
    let m = st.env.getSelectedModule ();
    let exe = obtainExecutable ();
    const objectCodeText = exe.objText;
    const metadataText   = exe.mdText;
    initializeProcessorElements (es);  // so far, it's just instr decode
//    com.mode.devlog ("------------- boot reading code --------------- ")
//    com.mode.devlog (`*** Boot object code = ${objectCodeText}`)
//    com.mode.devlog (`*** Boot metadata = ${metadataText}`)
//    com.mode.devlog ("------------- boot starting --------------- ")
    es.metadata = new st.Metadata ();
    es.metadata.fromText (metadataText);

    let objectCode = objectCodeText.split("\n");
    
//    memClearAccesses ();
    let xs = "";
    let fields = null;
    let isExecutable = true; // will set to false if module isn't bootable
    let location = 0; // address where next word will be stored
    document.getElementById('ProcAsmListing').innerHTML = "";
//    es.nInstructionsExecuted = 0;
    st.writeSCB (es, st.SCB_nInstrExecuted, 0)
    guiDisplayNinstr (es)
//    document.getElementById("nInstrExecuted").innerHTML =
//	es.nInstructionsExecuted;
    ioLogBuffer = "";
    refreshIOlogBuffer();
    com.mode.trace = true;
    for (let i = 0; i < objectCode.length; i++) {
        xs = objectCode[i];
        com.mode.devlog (`boot: objectCode line ${i} = <${xs}>`);
        fields = link.parseObjLine (xs);
        com.mode.devlog (`boot op=<${fields.operation}> args=<${fields.operands}>`);
        if (fields.operation == "module") {
            let modname = fields.operands[0];
            let safemodname = modname ? modname : "(anonymous)";
            com.mode.devlog (`boot: module ${safemodname}`);
        } else if (fields.operation == "org") {
            com.mode.devlog ("--- skipping org");
        } else if (fields.operation == "data") {
            com.mode.devlog ("boot: data");
            for (let j = 0; j < fields.operands.length; j++) {
                let val = arith.hex4ToWord(fields.operands[j]);
                if (!val) {com.mode.devlog(`boot: bad data (${val})`)};
                let safeval = val ? val : 0;
                memStore (es, location, safeval);
                com.mode.devlog (`boot data mem[${location}]:=${val}`);
                location++;
            }
        } else if (fields.operation == "import") {
            com.mode.devlog (`boot: import (${fields.operands})`)
            isExecutable = false;
        } else if (fields.operation == "export") {
        } else if (fields.operation == "relocate") {
        } else if (fields.operation == "") {
            com.mode.devlog ("boot: skipping blank object code line");
        } else {
            com.mode.devlog (`boot: bad operation (${fields.operation})`)
            isExecutable = false;
        }
    }
    if (isExecutable) {
        com.mode.devlog ("boot ok so far, preparing...");
        es.metadata.listingDec.forEach ((x,i) => es.asmListingCurrent[i] = x);
        initListing (m,es);
        es.curInstrAddr = 0;
        es.curInstrLineNo = -1;  // -1 indicates no line has been highlighted
        es.nextInstrAddr = 0;
        es.nextInstrLineNo = es.metadata.getSrcIdx (es.nextInstrAddr)
            + listingLineInitialOffset;
            com.highlightListingLine (es, es.nextInstrLineNo, "NEXT");
        setProcAsmListing (es,m);

        st.writeSCB (es, st.SCB_status, st.SCB_ready)
        getListingDims(es);
        resetRegisters (es);
        es.pc.put (0) // shouldn't be needed?
        refreshRegisters (es)
        updateMemory (es)
        memDisplayFull(es);
        clearTime (es)
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot was successful"
            + "</span><br>"
            + "</pre>";
        com.mode.devlog ("boot was successful")
    } else {
        st.writeSCB (es, st.SCB_status, st.SCB_reset)
        let xs =  "<pre class='HighlightedTextAsHtml'>"
            + "<span class='ExecutableStatus'>"
            + "Boot failed: module is not executable"
            + "</span><br>"
            + "</pre>";
        document.getElementById('LinkerText').innerHTML = xs;
        com.mode.devlog ("boot failed");
        alert ("boot failed");
    }
    if (es.thread_host === ES_gui_thread) {
        document.getElementById("procStatus").innerHTML = st.showSCBstatus (es)
    }
    com.mode.devlog ("boot returning");
}

export function refreshProcStatusDisplay (es) {
    let xs = st.showSCBstatus (es)
    document.getElementById("procStatus").innerHTML = xs
}

export let highlightedRegisters = [];

// Update the display of all registers and memory (all of memory)

export function displayFullState (es) {
    com.mode.devlog ('displayFullState');
    updateRegisters (es)
    updateMemory (es)
    memDisplayFull (es);
}

//----------------------------------------------------------------------
//  Registers
//----------------------------------------------------------------------

// Move to appropriate place... ??????????????
export let modeHighlightAccess = true;
  // for tracing: highlight reg/mem that is accessed

// The registers are defined as global variables.  These variables are
// declared here but their values are set only when window has been
// loaded (the onload event in gui.js), because the gui elements will
// exist at that time.

// Declare the registers - window.onload sets their values
// export let regFile = [];                            // register file R0,..., R15
//export let pc, ir, adr, dat;                        // instruction control registers
// export let statusreg, mask, req, rstat, rpc, vect;  // interrupt control registers
// export let bpseg, epseg, bdseg, edseg;              // segment control registers

// Generating and accessing registers
// export let controlRegisters;   // array of the control registers
// export let register = [];      // array of all the registers: regfile and control

// The system control registers are specified in the instruction by an
// index starting from i=0..., but the actual emulator data structures
// for these registers are held in the register[] array at index
// i+ctlRegIndexOffset...  It's useful to have a single array
// containing all the registers, so the emulator can refresh them all
// together.

export const ctlRegIndexOffset = 20;  // add to ctl reg no. to get register[] index
export let sysCtlRegIdx = 0;          // index of first system control reg
export let registerIndex = 0;         // unique index for each reg
export let regFetched = [];
export let regFetchedOld = [];
export let regStored = [];
export let regStoredOld = [];

// Instructions refer to the system control registers by a 4-bit
// index, but the system control register that has instruction index 0
// will actually have a higher index (16) in the emulator's array of
// registers.  To refer to sysctl reg i, it can be accessed as
// register [sysCtlRegIdx + i].

// Each register is represented by an object that contains its current
// value, as well as methods to get and set the value, and to refresh
// the display.

// Textual representation of system status, for the emulator display

function showSysStat (s) {
    return s===0 ? 'Usr' : 'Sys'
}

function testReg1 () {
    com.mode.devlog("testReg1");
//    regClearAccesses();
    es.pc.put(3);
    com.mode.devlog ("ir = " + es.ir.get());
    com.mode.devlog ("pc = " + es.pc.get());
//    regShowAccesses();
}

function testReg2 () {
    com.mode.devlog("testReg1");
//    regClearAccesses();
    com.mode.devlog ("ir = " + es.ir.get());
    com.mode.devlog ("pc = " + es.pc.get());
    es.adr.put(20);
//    regShowAccesses();
}

//-----------------------------------------------------------------------------
// Registers
//-----------------------------------------------------------------------------

// genregister creates and returns an object representing a register.
// The registers are created in gui.js in the window.onload function,
// because they need the gui display elements to be created first.
// Arguments to the constructor are:

//   - regNumber is index in a reg file (0 if irrelevant); e.g. for R5 it's 5
//   - regName is string representation; e.g. for R5 it's "R5"
//   - eltName is the html id of the gui element that displays the register
//   - showFcn converts the value to a string

// The actual value of a register is kept in the system state vector,
// where its index is regStIndex.  As registers are created, the
// number of existing registers (nRegisters) is used as the value of
// regStIndex.  So it's important to create the registers in a
// sensible order, with the register file first.

// let nRegisters = 0  // number of registers that have been defined

export class genregister {
    constructor (es, regNumber, regName, eltName, showFcn) {
        this.es = es
        this.regStIndex = es.nRegisters
        es.nRegisters++
        this.regNumber = regNumber
        this.regName = regName
        this.eltName = eltName
        this.show = showFcn
        this.elt = this.es.thread_host === ES_gui_thread
            ? document.getElementById (this.eltName)
            : null
        es.register.push (this)
    }
    get () {
        this.es.regFetched.push (this)
        let i = st.EmRegBlockOffset + this.regStIndex
        let x = this.regStIndex === 0 ? 0 : this.es.shm[i]
        return x
    }
    put (x) {
        this.es.regStored.push (this)
        let i = st.EmRegBlockOffset + this.regStIndex
        this.es.shm[i] = x
        if (this.regIdx < 16) { // register file
            instrEffect.push (["R", this.regNumber, x, this.regName]);
        }
//        com.mode.devlog (`--- reg put ${this.regName} :=`
//                     + ` ${arith.wordToHex4(x)} = ${x} (idx=${i})`)
    }
    highlight (key) {
//        console.log (`reg-highlight ${this.regName} ${key}`)
        if (this.es.thread_host === ES_gui_thread) {
            let i = st.EmRegBlockOffset + this.regStIndex
            let x = this.regStIndex === 0 ? 0 : this.es.shm[i]
            let xs = highlightText (this.show(x), key)
            this.elt.innerHTML = xs
        } else {
        }
    }
    refresh () {
        if (this.es.thread_host === ES_gui_thread) {
            let i = st.EmRegBlockOffset + this.regStIndex
            let x = this.regStIndex === 0 ? 0 : this.es.shm[i]
            let xs = this.show (x)
            this.elt.innerHTML = xs
        }
    }
}


// Reset every register to 0

function resetRegisters (es) {
    com.mode.devlog('Resetting registers');
    for (let i = 0; i < es.nRegisters; i++) {
        es.register[i].put (0)
        es.register[i].refresh ()
    }
}

export function refresh (es) {
    console.log ("Refresh")
    refreshRegisters (es)
    memRefresh (es)
    memDisplayFull (es)
    refreshProcStatusDisplay (es)
    guiDisplayNinstr (es)
}
//    let n = st.readSCB (es, st.SCB_nInstrExecuted)
//    com.mode.devlog (`refresh getting n`)
//    com.mode.devlog (`refresh n=${n}`)
//    updateInstructionCount (n)


//------------------------------------------------------------------------------
// Emulator state
//------------------------------------------------------------------------------

// This is the global emulator state.  The functions in the emulator
// don't use it directly, in order to avoid excessive use of globals.
// The current emulator state is passed as needed to functions (the
// convention is that the parameter name is 'es').

// The mode determines how much output the emulator provides and where
// it goes.  The mode values are: 100: gui display, 200: console
// display, 300: fast and quiet.  initialMode is provided when the
// state is created, and mode is the current value, which might change
// during execution.

// ES_thread_host indicates which thread this emulator instance is
// running in.  This is represented with an unsigned int, not a
// symbol, so it can be stored in the system state vector.

export const ES_gui_thread   = 0
export const ES_worker_thread     = 1

export class EmulatorState {
    constructor (thread_host) {
        this.thread_host      = thread_host // which thread runs this instance
        this.shm              = null // set by allocateStateVec
        this.emRunCapability  = ES_gui_thread // default: run in main thread
        this.emRunThread      = ES_gui_thread // default: run in main thread
        this.startTime        = null
        this.eventTimer       = null  // returned by setInterval
	this.instrLooperDelay = 1000
	this.instrLooperShow  = false
	this.breakEnabled     = false
     	this.breakPCvalue     = 0
	this.doInterrupt      = 0
        this.pc               = null
        this.ir               = null
        this.adr              = null
        this.dat              = null
        this.statusreg        = null
        this.mask             = null
        this.req  = null
        this.rstat  = null
        this.rpc = null
        this.vect = null
        this.bpseg = null
        this.epseg = null
        this.bdseg = null
        this.edseg = null
        this.regfile = []
        this.controlRegisters = []
        this.nRegisters = 0
        this.register = []
	this.ir_op              = 0
	this.ir_d               = 0
	this.ir_a               = 0
	this.ir_b               = 0
        this.ea                 = 0
	this.instrDisp          = 0
        this.field_e            = 0
        this.field_f            = 0
        this.field_g            = 0
        this.field_h            = 0
        this.field_gh           = 0
	this.instrOpCode        = null
	this.instrCodeStr       = ""
	this.instrFmtStr        = ""
	this.instrOp            = ""
	this.instrArgs          = ""
	this.instrEA            = null
	this.instrEAStr         = ""
	this.instrEffect        = []
        this.metadata           = null
	this.asmListingCurrent  = [] // listing displayed in emulator pane
        this.asmListingHeight   = 0   // height in pixels of the listing
	this.curInstrAddr       = 0
	this.nextInstrAddr      = 0
	this.curInstrLineNo     = -1  // -1 indicates no line has been highlighted
	this.nextInstrLineNo    = -1
	this.saveCurSrcLine     = ""
	this.saveNextSrcLine    - ""
        this.memElt1 = null
        this.memElt2 = null
        this.instrCodeElt = null
        this.instrFmtElt  = null
        this.instrOpElt   = null
        this.instrArgsElt = null
        this.instrEAElt   = null
        this.instrCCElt   = null
        this.instrEffect1Elt = null
        this.instrEffect2Elt = null
        this.regFetchedOld = []
        this.regStoredOld = []
        this.regFetched = []
        this.regStored = []
    }
}

//------------------------------------------------------------------------------
// Initialize machine state
//------------------------------------------------------------------------------


export function initializeMachineState (es) {
    com.mode.devlog ("emulator: initializeMachineState");
    com.mode.devlog (`em.initializeMachineState: current emulator mode = ${es.mode}`)
    initializeProcessorElements (es);  // so far, it's just instr decode
//    clearInstrDecode (emulatorState);
    clearInstrDecode (es);

    // Build the register file; sysStateVec index = reg number
    for (let i = 0; i < 16; i++) {
	let regname = 'R' + i; // also the id for element name
        es.regfile[i] = new genregister (es, i, regname, regname, arith.wordToHex4)
	es.register[i] = es.regfile[i]
    }

    // Instruction control registers
    es.pc   = new genregister (es, 0,  'pc',   'pcElt',    arith.wordToHex4);
    es.ir   = new genregister (es, 0,  'ir',   'irElt',    arith.wordToHex4);
    es.adr  = new genregister (es, 0,  'adr',  'adrElt',   arith.wordToHex4);
    es.dat  = new genregister (es, 0,  'dat',  'datElt',   arith.wordToHex4);

    // Interrupt control registers
    es.statusreg  = new genregister (es, 0, 'statusreg', 'statusElt',  arith.wordToHex4);
    // bit 0 (lsb) :  0 = User state, 1 = System state
    // bit 1       :  0 = interrupts disabled, 1 = interrupts enabled
    // bit 2       :  0 = segmentation disabled, 1 = segmentation enabled

    es.mask  = new genregister (es, 0, 'mask', 'maskElt',  arith.wordToHex4);
    es.req   = new genregister (es, 0, 'req',  'reqElt',   arith.wordToHex4);
    // mask and request use the same bit positions for flags
    // bit 0 (lsb)  overflow
    // bit 1        divide by 0
    // bit 2        trap 3
    // bit 3        

    es.rstat    = new genregister (es, 0, 'rstat',  'rstatElt',  arith.wordToHex4);
    es.rpc      = new genregister (es, 0, 'rpc',    'rpcElt',    arith.wordToHex4);
    es.vect     = new genregister (es, 0, 'vect',   'vectElt',   arith.wordToHex4);

// Segment control registers
    es.bpseg = new genregister (es, 0, 'bpseg',  'bpsegElt',  arith.wordToHex4);
    es.epseg = new genregister (es, 0, 'epseg',  'epsegElt',  arith.wordToHex4);
    es.bdseg = new genregister (es, 0, 'bdseg',  'bdsegElt',  arith.wordToHex4);
    es.edseg = new genregister (es, 0, 'edseg',  'edsegElt',  arith.wordToHex4);

// Record the control registers    
    es.controlRegisters =
	[es.pc, es.ir, es.adr, es.dat,   // not accessible to getctl/putctl instructions
	 // the following can be used for getctl/getctl, indexing from 0
	 es.statusreg, es.mask, es.req, es.rstat, es.rpc, es.vect,
         es.bpseg, es.epseg, es.bdseg, es.edseg
	]

    memInitialize (es);
    resetRegisters (es);
}

//------------------------------------------------------------------------------
// Instruction decode
//------------------------------------------------------------------------------

function showInstrDecode (es) {
    es.instrCodeStr = (instrCode ? arith.wordToHex4 (instrCode) : "")
	+ " " + (es.instrDisp ? arith.wordToHex4 (es.instrDisp) : "");
    es.instrEAStr = es.instrEA ? arith.wordToHex4 (es.instrEA) : "";
    com.mode.devlog (`showInstrDecode fmt = ${es.instrFmtStr}`);
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
let memFetchInstrLogOld = [];
let memFetchDataLogOld = [];
let memStoreLogOld = [];

export let memDisplayModeFull = false;  // show entire/partial memory
let memDisplayFastWindow = 16;          // how many locations to show in fast mode
let memDispOffset = 3;                  // how many locations above highligted one

// Must wait until onload event

function memInitialize (es) {
    if (es.thread_host === ES_gui_thread) {
        es.memElt1 = document.getElementById('MemDisplay1');
        es.memElt2 = document.getElementById('MemDisplay2');
        memClear(es);    // set each location to 0
        memRefresh(es);  // generate a string showing each location
        memDisplay(es);  // put the strings into the gui display elements
    }
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

function memClear (es) {
    for (let a = 0; a < memSize; a++) {
        memStore (es, a, 0)
    }
    memFetchInstrLog = [];
    memFetchDataLog = [];
    memStoreLog = [];
    memRefresh (es);
}


// Fetch and return a word from memory at address a, and record the
// address so the display can show this access.

function memFetchInstr (es, a) {
    memFetchInstrLog.push(a);
    let i = st.EmMemOffset + a
    let x =  es.shm[i]
//    let x = st.sysStateVec [st.EmMemOffset + a]
    com.mode.devlog (`memFetchInstr a=${arith.wordToHex4(a)}`
                 + ` offset=${st.EmMemOffset} i=${i} x=${arith.wordToHex4(x)}`)
    return x
}

function memFetchData (es, a) {
    memFetchDataLog.push(a);
    let x = es.shm[st.EmMemOffset + a]
    return x
}

// Store a word x into memory at address a, and record the address so
// the display can show this access.

function memStore (es, a,x) {
    memStoreLog.push(a);
    instrEffect.push(["M", a, x]);
    es.shm[st.EmMemOffset + a] = x
}

//------------------------------------------------------------------------------
// Processor execution status
//------------------------------------------------------------------------------

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


//-----------------------------------------------------------------------------
// Interrupts
//-----------------------------------------------------------------------------

export function timerInterrupt (es) {
    com.mode.devlog ("Timer Interrupt clicked");
    arith.setBitInRegBE (es.req, arch.timerBit);
    es.req.refresh();
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
//   procRunMainThread(es)         -- execute instructions repeatedly until halted
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
    com.mode.devlog(`showEffect ${i}`);
    if (es.instrEffect[i]) {
	let [dest,idx,val,name] = es.instrEffect[i];
	if (dest==="R") {
	    com.mode.devlog (`showEffect ${i} ${es.instrEffect[i]}`);
//	    com.mode.devlog (`showEffect ${i}  ${dest} ${idx} := ${val});
	    return `${name} := ${arith.wordToHex4(val)}`;
	} else if (dest==="M") {
	    com.mode.devlog ("showEffect M");
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

// Given address a, the corresponding source statement is found using metadata

function showListingParameters (es) {
    com.mode.devlog ('showListingParameters'
		 + ' es.curInstrAddr=' + es.curInstrAddr
		 + ' es.curInstrLineNo=' + es.curInstrLineNo
		 + ' es.nextInstrAddr=' + es.nextInstrAddr
		 + ' es.nextInstrLineNo=' + es.nextInstrLineNo);
}

// Prepare the running listing before starting instructionby removing
// any existing highlighting of current and next instruction


function showLst (es, xs, i) {
//    return // disable
    com.mode.devlog (`--- Listing line ${i} ${xs}`)
    com.mode.devlog (`----- Cur = ${es.asmListingCurrent[i].slice(0,30)}`)
    com.mode.devlog (`----- Pla = ${es.metadata.listingPlain[i].slice(0,30)}`)
    com.mode.devlog (`----- Dec = ${es.metadata.listingDec[i].slice(0,30)}`)
}

//------------------------------------------------------------------------------
// Step and run instructions
//------------------------------------------------------------------------------

// Runs in main gui thread

export function procStep (es) {
    if (es.thread_host != ES_gui_thread) {
        console.log (`procStep: host=${es.thread_host}, skipping`)
        return
    }
    let q = st.readSCB (es, st.SCB_status)
    switch (q) {
    case st.SCB_ready:
    case st.SCB_paused:
    case st.SCB_break:
    case st.SCB_relinquish:
        console.log ("procStep: main thread executing instruction...")
        st.writeSCB (es, st.SCB_status, st.SCB_running_gui)
        executeInstruction (es)
        if (st.readSCB (es, st.SCB_status) != st.SCB_halted) {
            st.writeSCB (es, st.SCB_status, st.SCB_ready)
        }
        execInstrPostDisplay (es)
        guiDisplayNinstr (es)
        break
    case st.SCB_reset:
    case st.SCB_running_gui:
    case st.SCB_running_emwt:
    case st.SCB_halted:
    case st.SCB_blocked:
        console.log ("procStep skipping instruction...")
        break
    default: console.log (`error: procStep unknown SCB_tatus= ${q}`)
    }
}

// Separate clearing state from refreshing display
export function procReset (es) {
    console.log ("em reset");
    com.mode.devlog ("reset the processor");
    st.resetSCB (es)
    resetRegisters (es);
    memClear (es);
    clearTime (es)
    refreshDisplay (es)
}

export function refreshDisplay (es) {
    refreshRegisters (es);
    memDisplay (es);
    document.getElementById('ProcAsmListing').innerHTML = "";
    clearInstrDecode (es);
    refreshInstrDecode (es);
    guiDisplayNinstr (es)
    ioLogBuffer = ""
    refreshIOlogBuffer ()
    st.showSCBstatus (es)
//    memClearAccesses ();
}

//------------------------------------------------------------------------------
// Controlling instruction execution
//------------------------------------------------------------------------------

// Run instructions repeatedly until a stopping condition arises.
// Yield control each iteration to avoid blocking the user interface,
// particularly the manual timer interrupt button.

export function procRunMainThread (es) {
    let q = st.readSCB (es, st.SCB_status)
    if (q === st.SCB_ready || q === st.SCB_paused) {
        com.mode.devlog ("procRunMainThread: start looper in gui thread");
        instructionLooper (es);
        execInstrPostDisplay (es)
    } else {
        console.log (`procRunMainThread skipping: SCB_status=${q}`)
    }
}

// Run instructions in the main gui thread.  Caller should check the
// system status before calling the looper; it's assumed that it is ok
// to execute an instruction in the gui thread.

function instructionLooper (es) {
    st.writeSCB (es, st.SCB_status, st.SCB_running_gui)
    executeInstruction (es);
    let q = st.readSCB (es, st.SCB_status)
    switch (q) {
    case st.SCB_running_gui:
	setTimeout (function () {instructionLooper(es)});
        break
    default:
        execInstrPostDisplay (es)
    }
    st.writeSCB (es, st.SCB_status, st.SCB_ready)
    com.mode.devlog ('instructionLooper terminated');
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
    com.mode.devlog ("procBreakpoint");
    document.getElementById("BreakDialogue").style.display
	= breakDialogueVisible ? "none" : "block";
    breakDialogueVisible = !breakDialogueVisible;
}

export function hideBreakDialogue () {
    document.getElementById("BreakDialogue").style.display = "none";
    breakDialogueVisible = false;
}

function breakRefresh (es) {
    com.mode.devlog ("breakRefresh");
    let x = document.getElementById('BreakTextArea').value;
    if (x.search(hexParser) == 0) {
	let w = arith.hex4ToWord (x.slice(1));
	es.breakPCvalue = w;
	com.mode.devlog (`breakPCvalue = + ${w}`);
    } else {
	com.mode.devlog (`breakRefresh cannot parse + x`);
    }
}

function breakEnable (es) {
    com.mode.devlog ("breakEnable");
    es.breakEnabled = true;
    com.mode.devlog (`breakEnable ${es.breakPCvalue}`);
}

function breakDisable (es) {
    com.mode.devlog ("breakDisable");
    es.breakEnabled = false;
}

function breakClose () {
    com.mode.devlog ("breakClose");
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


// (for stepping) Display the effects of the instruction

export function execInstrPostDisplay (es) {
    console.log ("execInstrPostDisplay")
    switch (es.thread_host) {
    case ES_worker_thread: // should be impossible
        break;
    case ES_gui_thread:
        //    clearRegisterHighlighting (es); // deprecated
        console.log ("main: execInstrPostDisplay, proceeding")
        updateMemory (es)
        memDisplay (es)
        showInstrDecode (es)
        guiDisplayNinstr (es)
        updateRegisters (es)
        document.getElementById("procStatus").innerHTML = st.showSCBstatus (es)
        highlightListingAfterInstr (es)
        break
    default: // should be impossible
        console.log (`error: execInstrPostDisplay host=${es.thread_host}`)
    }
}

// When running, the logging data isn't needed.  The worker thread
// needs to clear it to prevent a space leak.

export function clearLoggingData (es) {
    es.regFetched = []
    es.regStored = []
    memFetchInstrLog = []
    memFetchDataLog = []
    memStoreLog = []
}

// Prepare to execute an instruction by clearing the buffers holiding
// log information.

function prepareExecuteInstruction (es) {
    com.mode.devlog ("prepareExecuteInstruction");

// Preparations before the instruction
    memDisplay (es);
    clearInstrDecode (es);
    prepareListingBeforeInstr (es);
}

//------------------------------------------------------------------------------
// Machine language semantics
//------------------------------------------------------------------------------

// Execute one instruction; runs in either in any thread

export function executeInstruction (es) {
    com.mode.devlog (`em.executeInstruction starting`)

    // Check for interrupt
    let mr = es.mask.get() & es.req.get() // ???
    com.mode.devlog (`interrupt mr = ${arith.wordToHex4(mr)}`)
    st.writeSCB (es, st.SCB_cur_instr_addr, es.pc.get())
    if (arith.getBitInRegBE (es.statusreg, arch.intEnableBit) && mr) {
        com.mode.devlog (`execute instruction: interrupt`)
        console.log (`execute instruction: interrupt`)
	let i = 0; // interrupt that is taken
	while (i<16 && arith.getBitInWordBE(mr,i)==0) { i++ }
	com.mode.devlog (`\n*** Interrupt ${i} ***`)
	es.rpc.put(es.pc.get())           // save the pc
	es.rstat.put(es.statusreg.get())   // save the status register
	arith.clearBitInRegBE (es.req,i)  // clear the interrupt that was taken
	es.pc.put (es.vect.get() + 2*i)  // jump to handler
        // Disable interrupts and enter system state
	es.statusreg.put (es.statusreg.get()
		       & arith.maskToClearBitBE(arch.intEnableBit)
		       & arith.maskToClearBitBE(arch.userStateBit))
	return
    }

    // No interrupt, so proceed with next instruction
    com.mode.devlog (`no interrupt, proceeding...`)
    es.curInstrAddr = es.pc.get();
    com.mode.devlog (`ExInstr pc=${arith.wordToHex4(es.curInstrAddr)}`)
    instrCode = memFetchInstr (es, es.curInstrAddr);
    com.mode.devlog (`ExInstr ir=${arith.wordToHex4(instrCode)}`)
    es.ir.put (instrCode);
    es.nextInstrAddr = arith.binAdd (es.curInstrAddr, 1);
    es.pc.put (es.nextInstrAddr);
    st.writeSCB (es, st.SCB_next_instr_addr, es.nextInstrAddr)

    com.mode.devlog (`ExInstr pcnew=${arith.wordToHex4(es.nextInstrAddr)}`)
    
// com.mode.devlog('pc = ' + arith.wordToHex4(pc.get()) + ' ir = ' + arith.wordToHex4(instr));
    let tempinstr = es.ir.get();
    com.mode.devlog (`ExInstr instr=${arith.wordToHex4(tempinstr)}`)
    es.ir_b = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_a = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_d = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_op = tempinstr & 0x000f;
//    com.mode.devlog(`ir fields ${es.ir_op} ${es.ir_d} ${es.ir_a} ${es.ir_b}`);
    com.mode.devlog(`ExInstr ir fields ${es.ir_op} ${es.ir_d} ${es.ir_a} ${es.ir_b}`);
    es.instrFmtStr = "RRR";  // Replace if opcode expands to RX or EXP
    es.instrOpStr = arch.mnemonicRRR[es.ir_op]  // Replace if opcode expands
    com.mode.devlog (`ExInstr dispatch primary opcode ${es.ir_op}`);
    dispatch_primary_opcode [es.ir_op] (es);
    st.incrSCB (es, st.SCB_nInstrExecuted)
}

// RRR instruction pattern functions

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
// ab_dc  -- use ra and rb as operands, place results in rd and cc
// cab_dc -- use cc, ra and rb as operands, place results in rd and cc
// rd    -- use ra as operand, place result in rd, ignore rb and cc
// ab_c   -- use ra and rb as operands, place result in cc, ignore rd
// rrrc  -- use ra, rb, and cc as operands, place results in rd and cc
// trap  -- perform trap; instruction ignores all registers but OS may use them


/* JavaScript: example of a lambda expression and a curried lambda
  const foobar = (x) => (y) => x+y;
  let baz = foobar (5);
  let bar = foobar (41);
  let abc = foobar (7) (50);
*/

// ab_dc instructions take two register arguments in a and b, and
// produce two results: primary and secondary.  The primary is loaded
// into d.  The secondary is normally loaded into condition code, but
// if d=15 the secondary is discarded.  The instruction semantics is
// defined by f.

const ab_dc = (f) => (es) => {
    let a = es.regfile[es.ir_a].get();
    let b = es.regfile[es.ir_b].get();
    let [primary, secondary] = f (a,b);
    es.regfile[es.ir_d].put(primary);
    if (es.ir_d<15) { es.regfile[15].put(secondary) }
}

// ab_dac instructions (e.g. pop):
const ab_dac = (f) => (es) => {
    let a = es.regfile[es.ir_a].get();
    let b = es.regfile[es.ir_b].get();
    let [primary,secondary] = f (a,b);
    es.regfile[es.ir_a].put(primary);
    if (es.ir_a<15) { es.regfile[15].put(secondary) }
}


// Apply f to d, a, b
const rrrc = (f) => (es) => {
    let a = es.regfile[es.ir_a].get();
    let b = es.regfile[es.ir_b].get();
    let [primary, secondary] = f (a,b);
    es.regfile[es.ir_d].put(primary);
    if (es.ir_d<15) { es.regfile[15].put(secondary) }
}

// Apply f to a and load primary result into d (e.g. inv)

const rd = (f) => (es) => {
    let a = es.regfile[es.ir_a].get();
    let primary = f (a);
    es.regfile[es.ir_d].put(primary);
}

// Apply f to a and b, and load primary result into d (e.g. cmplt)

const rrd = (f) => (es) => {
    let a = es.regfile[es.ir_a].get();
    let b = es.regfile[es.ir_b].get();
    let primary = f (a,b);
    es.regfile[es.ir_d].put(primary);
}

// ab_c -- Arguments are in a and b.  The result is loaded into the
// condition code.  The instruction semantics is defined by f.

const ab_c = (f) => (es) => {
    let a = es.regfile[es.ir_a].get();
    let b = es.regfile[es.ir_b].get();
    let cc = f (a,b);
    com.mode.devlog (`ab_c cc=${cc}`);
    com.mode.devlog (`CMP/AB_C ab_c cc=${cc}`);
    es.regfile[15].put(cc);
}

// cab_dc -- arguments are in c, a, and b.  The primary result is
// loaded into d and the secondary is loaded into c.  The instruction
// semantics is defined by f.

const cab_dc = (f) => (es) => {
    let c = es.regfile[15].get();
    let a = es.regfile[es.ir_a].get();
    let b = es.regfile[es.ir_b].get();
    let [primary, secondary] = f (c,a,b);
    es.regfile[es.ir_d].put(primary);
    if (es.ir_d<15) { es.regfile[15].put(secondary) }
}

const op_trap = (es) => {
    switch (es.thread_host) {
    case ES_gui_thread:
        console.log (`handle trap in main thread`)
        let code = es.regfile[es.ir_d].get();
        com.mode.devlog (`trap code=${code}`);
        if (code===0) { // Halt
	    console.log ("Trap: halt");
	    com.mode.devlog ("Trap: halt");
            st.writeSCB (es, st.SCB_status, st.SCB_halted)
            updateRegisters (es)
            updateMemory (es)
            memRefresh(es);
        } else if (code==1) { // Read
            trapRead(es);
        } else if (code==2) { // Write
            trapWrite(es);
        } else { // Undefined trap is nop
            com.mode.devlog (`trap with unbound code = ${code}`)
        }
        break
    case ES_worker_thread:
        console.log (`emworker: relinquish control on a trap`)
        st.writeSCB (es, st.SCB_status, st.SCB_relinquish)
        console.log (`trap relinquish before fixup, pc = ${es.pc.get()}`)
        es.pc.put (st.readSCB (es, st.SCB_cur_instr_addr))
        console.log (`trap relinquish after fixup, pc = ${es.pc.get()}`)
        break
    default:
        console.log (`system error: trap has bad shm_token ${q}`)
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
    let a = es.regfile[es.ir_a].get(); // buffer address
    let b = es.regfile[es.ir_b].get(); // buffer size
    let n = xs.length; // number of chars available in buffer
    let m = n <= b ? n : b; // number of chars actually input
    let xs2 = xs.substring (m,n); // excess chars from input window are not used
    let ys = xs.substring (0,m);  // input string to store into memory
    let charcode = 0;
    document.getElementById("IOinputBuffer").value = xs2;
    com.mode.devlog (`Read: a=${a} b=${b} m=${m} >>> /${ys}/`);
    com.mode.devlog (`Read: n=${n} m=${m}`);
    com.mode.devlog (`Read: xs2=/${xs2}/ ys=/${ys}/`);
    for (let i = 0; i<m; i++) {
	charcode = ys.charCodeAt(i);
	memStore (es, a, charcode);
	com.mode.devlog (`Read: mem[${a}] := ${charcode}`);
	a++;
    }
    es.regfile[es.ir_a].put(a); // just after last address stored
    es.regfile[es.ir_b].put(m); // number of chars actually input
    ioLogBuffer += highlightField(ys,"READ");   // display chars that were read
    refreshIOlogBuffer ();
    inputElt.value = xs2; // leave unread characters in input buffer
}

// Write b characters starting from address a
function trapWrite (es) {
    let a = es.regfile[es.ir_a].get(); // buffer address
    let b = es.regfile[es.ir_b].get(); // buffer size
    let xs = "";
    for (let i = 0; i<b; i++) {
	xs += String.fromCharCode(memFetchData(es,a));
	a++
    }
    com.mode.devlog (`Write a=${a} b=${b} >>> /${xs}/`);
    ioLogBuffer += xs;
    com.mode.devlog (ioLogBuffer);
    refreshIOlogBuffer();
}

const handle_rx = (es) => {
    com.mode.devlog ('handle rx' + es.ir_b);
    com.mode.devlog (`handle rx secondary=${es.ir_b}`);
    es.instrFmtStr = "RX";
    dispatch_RX [es.ir_b] (es);
}

const handle_EXP = (es) => {
    es.instrFmtStr = "EXP";
    let code = 16*es.ir_a + es.ir_b;
    if (code < limitEXPcode) {
	com.mode.devlog (`>>> dispatching EXP code=${code} d=${es.ir_d}`);
	console.log (`>>> dispatching EXP code=${code} d=${es.ir_d}`);
	dispatch_EXP [code] (es);
    } else {
	com.mode.devlog (`EXP bad code ${arith.wordToHex4(code)}`);
    }
}

const op_push  = (es) => {
    const d = es.regfile[es.ir_d].get();
    const ra = es.ir_a;
    const a = es.regfile[ra].get();
    const b = es.regfile[es.ir_b].get();
    com.mode.devlog (`push d=${d} ra=${ra} a=${a} b=${b}`)
    if (a < b) {
        es.regfile[ra].put(a+1);
        memStore (es, a+1, d);
    } else {
        com.mode.devlog (`push: stack overflow`);
        arith.setBitInRegBE (req, arch.stackOverflowBit);
    }
}

// pop: Rd = data, Ra = top, Rb = base

const op_pop  = (es) => {
    const a = es.regfile[es.ir_a].get();
    const b = es.regfile[es.ir_b].get();
    if (a >= b) {
        es.regfile[es.ir_d].put(memFetchData(es,a));
        es.regfile[es.ir_a].put(a-1);
    } else {
        com.mode.devlog (`pop: stack underflow`);
        arith.setBitInRegBE (req, arch.stackUnderflowBit);
    }
}

const op_top  = (es) => {
    const a = es.regfile[es.ir_a].get();
    const b = es.regfile[es.ir_b].get();
    if (a >= b) {
        es.regfile[es.ir_d].put(memFetchData(es,a));
    } else {
        com.mode.devlog (`pop: stack underflow`);
        arith.setBitInRegBE (req, arch.stackUnderflowBit);
    }
}

// Use the opcode to dispatch to the corresponding instruction.  If
// the opcode is 14/15 the instruction representation escapes to
// EXP/RX format.  Otherwise the instruction is RRR.  There are
// several patterns of register usage for RRR instructions; each
// pattern has a pattern function (e.g. ab_dc) that is applied to a
// specific instruction semantics function (e.g. arith.op_add).  There
// is a naming convention for the pattern functions: the name consists
// of the source registers, underscore, and the result registers.  The
// normal pattern for RRR instructions is ab_dc, where the arguments
// are in Ra and Rb, and the primary result goes into Rd with the
// condition code set.  Several RRR instructions use the registers
// differently, so they have different pattern functions.

// There are two reasons for introducing the instruction pattern
// functions. They abstract the common behaviors and allow the
// specific instruction calculations to be separated out, and they
// avoid fetching registers that will not be used in an instruction (a
// circuit implementing the architecture would likely fetch all the
// register operands, but for a student learning architecture, it
// would be confusing for the register highlighting to indicate that a
// register has been fetched if it isn't needed for the instruction).

const dispatch_primary_opcode =
      [ ab_dc (arith.op_add),     // 0
        ab_dc (arith.op_sub),     // 1
        ab_dc (arith.op_mul),     // 2
        ab_dc (arith.op_div),     // 3
        cab_dc (arith.op_addc),   // 4
        cab_dc (arith.op_muln),   // 5
        cab_dc (arith.op_divn),   // 6
        ab_c  (arith.op_cmp),     // 7
        op_push,                  // 8
        op_pop,                   // 9
        op_top,                   // a
        ab_dc (arith.op_nop),     // b  reserved, currently nop
        op_trap,                  // c  trap=13
        ab_dc (arith.op_nop),     // d  reserved for expanding opcode
        handle_EXP,               // e  escape to EXP
        handle_rx ]               // f  escape to RX

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
        
    com.mode.devlog('rx');
    es.instrOpStr = arch.mnemonicRX[es.ir_b];
    es.instrDisp = memFetchInstr (es, es.pc.get());
    es.adr.put (es.instrDisp);
    es.nextInstrAddr = arith.binAdd (es.nextInstrAddr, 1);
    es.pc.put (es.nextInstrAddr);
    st.writeSCB (es, st.SCB_next_instr_addr, es.nextInstrAddr)
    //    es.ea = arith.binAdd (regFile[es.ir_a].get(), adr.get());
    es.ea = arith.binAdd (es.regfile[es.ir_a].get(), es.instrDisp);
    es.instrEA = es.ea;
    com.mode.devlog (`rx ea, disp=${arith.wordToHex4(es.instrDisp)}`);
    com.mode.devlog (`rx ea, idx=${arith.wordToHex4(es.regfile[es.ir_a].get())}`);
    com.mode.devlog('rx ea = ' + arith.wordToHex4(es.ea));
    f (es);
}

const dispatch_RX =
    [ rx (rx_lea),       // 0
      rx (rx_load),      // 1
      rx (rx_store),     // 2
      rx (rx_jump),      // 3
      rx (rx_jal),       // 4
      rx (rx_jumpc0),    // 5
      rx (rx_jumpc1),    // 6
      rx (rx_jumpz),     // 7 should be jumpn ?????????
      rx (rx_jumpz),     // 8
      rx (rx_jumpnz),    // 9
      rx (rx_jumpz),     // a should be jumpp ?????????
      rx (rx_testset),   // b
      rx (rx_nop),       // c
      rx (rx_nop),       // d
      rx (rx_nop),       // e
      rx (rx_nop) ];     // f

function rx_lea (es) {
    com.mode.devlog('rx_lea');
    es.regfile[es.ir_d].put(es.ea);
}

function rx_load (es) {
    com.mode.devlog('rx_load');
    es.regfile[es.ir_d].put(memFetchData(es,es.ea));
}

function rx_store (es) {
    com.mode.devlog('rx_store');
    memStore (es, es.ea, es.regfile[es.ir_d].get());
}

function rx_jump (es) {
    com.mode.devlog('rx_jump');
    es.nextInstrAddr = es.ea;
    es.pc.put(es.nextInstrAddr);
}

function rx_jumpc0 (es) {
    com.mode.devlog('rx_jumpc0');
    let cc = es.regfile[15].get();
    if (arith.extractBitBE (cc,es.ir_d)===0) {
	es.nextInstrAddr = es.ea;
	es.pc.put(es.nextInstrAddr);
    }
}

function rx_jumpc1 (es) {
    com.mode.devlog('rx_jumpc1');
    let cc = es.regfile[15].get();
    if (arith.extractBitBE (cc,es.ir_d)===1) {
	es.nextInstrAddr = es.ea;
	es.pc.put(es.nextInstrAddr);
    }
}

function rx_jumpz (es) {
    com.mode.devlog('rx_jumpz');
    if (! arith.wordToBool (es.regfile[es.ir_d].get())) {
	es.nextInstrAddr = es.ea;
	es.pc.put (es.nextInstrAddr);
    }
}

function rx_jumpnz (es) {
    com.mode.devlog('rx_jumpnz');
    if (arith.wordToBool (es.regfile[es.ir_d].get())) {
	es.nextInstrAddr = es.ea;
	es.pc.put (es.nextInstrAddr);
    }
}

function rx_testset (es) {
    com.mode.devlog (`testset`);
    es.regfile[es.ir_d].put(memFetchData(es.ea));
    memStore (es, es.ea, 1);
}

function rx_jal (es) {
    com.mode.devlog('rx_jal');
    es.regfile[es.ir_d].put (es.pc.get());
    es.nextInstrAddr = es.ea;
    es.pc.put (es.nextInstrAddr);
}

function rx_nop (es) {
    com.mode.devlog ('rx_nop');
}

// EXP format

function exp1_nop (es) {
    com.mode.devlog ('exp1_nop');
}

function exp1_resume (es) {
    com.mode.devlog ('exp1_resume');
    es.statusreg.put (es.rstat.get());
    es.pc.put (es.rpc.get());
}

function exp2_save (es) {
    const rStart = es.ir_d;
    const rEnd = es.field_e;
    const index = es.regfile[es.field_f].get();
    const offset = es.field_gh;
    const ea = index + offset;
    com.mode.devlog (`save regs = ${rStart}..${rEnd} index=${index}`
                 + ` offset=${offset} ea=${arith.wordToHex4(ea)}`);
    sr_looper ((a,r) => memStore (es, a, es.regfile[r].get()), ea, rStart, rEnd);
}

function exp2_restore (es) {
    com.mode.devlog ('exp2_restore');
    const rStart = es.ir_d;
    const rEnd = es.field_e;
    const index = es.regfile[es.field_f].get();
    const offset = es.field_gh;
    const ea = index + offset;
    com.mode.devlog (`restore regs = ${rStart}..${rEnd} index=${index}`
                 + ` offset=${offset} ea=${arith.wordToHex4(ea)}`);
    sr_looper ((a,r) => es.regfile[r].put(memFetchData(es,a)), ea, rStart, rEnd);
}

function sr_looper (f,addr,first,last) {
    let done = false;
    let r = first;
    while (!done) {
        com.mode.devlog (`save looper addr=${addr} r=${r}`);
        f(addr,r);
        done = r==last;
        addr += 1;
        r = bininc4(r);
    }
}

function bininc4 (x) { return x >= 15 ? 0 : x+1 }

// temp like put
function exp2_getctl (es) {
    com.mode.devlog ('exp2_getctl');
    let cregn = es.field_f;
    let cregidx = cregn + ctlRegIndexOffset; // init in gui.js
    com.mode.devlog (`exp_getctl cregn=${cregn} cregidx=${cregidx}`);
    es.regfile[es.field_e].put(es.register[cregidx].get());
}
function exp2_putctl (es) {
    com.mode.devlog ('putctl');
    let cregn = es.field_f;
    let cregidx = cregn + ctlRegIndexOffset; // init in gui.js
    com.mode.devlog (`putctl src e==${es.field_e} val=${es.regfile[es.field_e].get()}`);
    com.mode.devlog (`putctl dest f=${es.field_f} cregn=${cregn} cregidx=${cregidx}`);
    es.register[cregidx].put(es.regfile[es.field_e].get());
    es.register[cregidx].refresh(); // ???
}

function exp2_execute (es) {
    com.mode.devlog ("exp2_execute");
}

function exp2_push (es) {
    com.mode.devlog ('exp2_push');
 //   com.mode.devlog (`e=${es.field_e} f=${es.field_f} g=${es.field_g} h=${es.field_h} `);
    let top = es.regfile[es.field_f].get();
    let last = es.regfile[es.field_g].get();
//    com.mode.devlog (`push: top=${top} last=${last}`);
    if (top < last) {
        top += 1;
        memStore (es, top, es.regfile[es.field_e].get());
        es.regfile[es.field_f].put(top);
    } else {
        com.mode.devlog ("push: stack overflow")
    }
}

function exp2_pop (es) {
    com.mode.devlog ('exp2_pop');
    let top = es.regfile[es.field_f].get();
    let first = es.regfile[es.field_g].get();
    if (top >= first) {
        es.regfile[es.field_e].put(memFetchData(es,top));
        top -= 1;
        es.regfile[es.field_f].put(top);
    } else {
        com.mode.devlog ("pop: stack underflow")
    }
}

function exp2_top (es) {
    com.mode.devlog ('exp2_top');
    let top = es.regfile[es.field_f].get();
    let first = es.regfile[es.field_g].get();
    if (top >= first) {
        es.regfile[es.field_e].put(memFetchData(es,top));
        es.regfile[es.field_f].put(top);
    } else {
        com.mode.devlog ("top: stack underflow")
    }
}

function exp2_shiftl (es) {
    com.mode.devlog ("exp2_shiftl");
    com.mode.devlog (`shift d=${arith.wordToHex4(es.field_d)} e=${arith.wordToHex4(es.field_e)}  f=${arith.wordToHex4(es.field_f)} `);
    const x = es.regfile[es.field_e].get();
    const k = es.field_f;
    const result = arith.shiftL (x,k);
//    const result1 = x << k;  // logical shift
//    const result2 = arith.truncateWord (result1);
    com.mode.devlog (`shiftl x=${arith.wordToHex4(x)} k=${arith.wordToHex4(k)} r1=${arith.wordToHex4(result)}`);
    com.mode.devlog (`shiftl ${arith.wordToHex4(x)} left by ${k} bits => ${arith.wordToHex4(result)}`);
    es.regfile[es.ir_d].put(result);
}

function exp2_shiftr (es) {
    com.mode.devlog ("exp2_shiftr");
    com.mode.devlog (`shiftr d=${arith.wordToHex4(es.field_d)} e=${arith.wordToHex4(es.field_e)}  f=${arith.wordToHex4(es.field_f)} `);
    let x = es.regfile[es.field_e].get();
    let k = es.field_f;
    const result = arith.shiftR (x,k);
//    let result1 = x >>> k;  // logical shift
//    const result2 = arith.truncateWord (result1);
    com.mode.devlog (`shiftr x=${arith.wordToHex4(x)} k=${arith.wordToHex4(k)} r1=${arith.wordToHex4(result)}`);
    com.mode.devlog (`shiftr ${arith.wordToHex4(x)} right by ${k} bits => ${arith.wordToHex4(result)}`);
    es.regfile[es.ir_d].put(result);
}

// extract

function exp2_extract (es) {
    com.mode.devlog ('exp2_extract');
    com.mode.devlog ('exp2_extract');
    const src = es.regfile[es.field_g].get();
    const srci = es.field_h;
    const yold = es.regfile[es.ir_d].get();
    const yi = es.field_e;
    const size = es.field_f;
    const ynew = arith.calculateExtract (16, size, src, srci, yold, yi);
    com.mode.devlog (`extract src=${arith.wordToHex4(src)} srci=${srci} `
                 + `yold=${arith.wordToHex4(yold)} yi=${yi} size=${size} `
                 + ` ynew=${arith.wordToHex4(ynew)}`);
    es.regfile[es.ir_d].put(ynew);
}

function exp2_extracti (es) {
    com.mode.devlog ('exp2_extracti');
    const src = es.regfile[es.field_g].get();
    const srci = es.field_h;
    const yold = es.regfile[es.ir_d].get();
    const yi = es.field_e;
    const size = es.field_f;
    const ynew = arith.calculateExtracti (16,size,src,srci,yi);
    com.mode.devlog (`extract src=${arith.wordToHex4(src)} srci=${srci} `
                 + `yold=${arith.wordToHex4(yold)} yi=${yi} size=${size} `
                 + ` ynew=${arith.wordToHex4(ynew)}`);
    es.regfile[es.ir_d].put(ynew);
}

function exp2_logicw (es) {
    com.mode.devlog ('EXP2 logicw');
    com.mode.devlog ('exp2_logicw');
    let x = es.regfile[es.field_e].get(); // operand 1
    let y = es.regfile[es.field_f].get(); // operand 2
    let fcn = es.field_h; // logic function
    let result = arith.applyLogicFcnWord (fcn,x,y); // fcn x y
    com.mode.devlog (`logicw x=${arith.wordToHex4(x)} y=${arith.wordToHex4(y)} result=${arith.wordToHex4(result)}`);
    es.regfile[es.ir_d].put(result);
}

function exp2_logicb (es) {
    com.mode.devlog (`>>>>>>>>>>>>>>>>>>>>>>>> exp2_logicb`);
    com.mode.devlog (`exp2_logicb`);
    const x = es.regfile[es.ir_d].get();
    const f = arith.getBitInWordBE (x, es.field_f);
    const g = arith.getBitInWordBE (x, es.field_g);
    const fcn = es.field_h;  // logic function
    const bresult = arith.applyLogicFcnBit (fcn, f, g); // bit result
    const y = arith.putBitInWord (x, es.field_e, bresult); // word result
    com.mode.devlog (`logicb x=${arith.wordToHex4(x)} f=${f} g=${g} `
                 + `fcn=${fcn} b=${bresult} result=${arith.wordToHex4(y)}`);;
    es.regfile[es.ir_d].put(y);
}

// EXP format instructions that use only one word
const exp1 = (f) => (es) => {
    com.mode.devlog (`>>> EXP1 instruction`);
    let expCode = 16*es.ir_a + es.ir_b;
    es.instrOpStr = `EXP1 menonic code=${expCode}`;  // ?????????????
    com.mode.devlog(`EXP1 code=${expCode} d=${es.ir_d}`);
    f(es);
}

// EXP2 format instructions require a second word

const exp2 = (f) => (es) => {
    com.mode.devlog (`>>> EXP2 instruction`);
    console.log (`>>> EXP2 instruction`);
    let expCode = 16 * es.ir_a + es.ir_b;
    es.instrOpStr = `EXP2 mnemonic code=${expCode}`;  // ????????????
    es.instrDisp = memFetchInstr (es, es.pc.get());
    es.adr.put (es.instrDisp);
    es.nextInstrAddr = arith.binAdd (es.nextInstrAddr, 1);
    es.pc.put (es.nextInstrAddr);
    let tempinstr = es.instrDisp;
    es.field_gh = tempinstr & 0x00ff;
    es.field_h = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_g = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_f = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_e = tempinstr & 0x000f;
    com.mode.devlog(`>>> EXP2 code=${expCode} d=${es.ir_d} e=${es.field_e} `
                + `f=${es.field_f} g=${es.field_g} h=${es.field_h}`);
    f (es);
}

const dispatch_EXP =
      [ exp1 (exp1_resume),    // 0
        exp2 (exp2_save),      // 1
        exp2 (exp2_restore),   // 2
        exp2 (exp2_shiftl),    // 3
        exp2 (exp2_shiftr),    // 4
        exp2 (exp2_logicw),    // 5
        exp2 (exp2_logicb),    // 6
        exp2 (exp2_extract),   // 7
        exp2 (exp2_extracti),  // 8
        exp2 (exp2_getctl),    // 9
        exp2 (exp2_putctl)     // a
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
    com.mode.devlog ('testpane 1 clicked')
    let xs = ["<pre class='HighlightedTextAsHtml'>", 'line 1 text',
	      "<span class='CUR'>this is line 2 text</span>",
	      'and finally line 3', '</pre>'];
    com.mode.devlog ('xs = ' + xs);
    let ys = xs.join('\n');
    com.mode.devlog ('ys = ' + ys);

    let qs = ys;
    com.mode.devlog ('qs = ' + qs);
    document.getElementById('TestPaneBody').innerHTML = qs;
}

function testpane2 () {
    com.mode.devlog ('testpane 2 clicked');
}

function testpane3 () {
    com.mode.devlog ('testpane 3 clicked');
}

//-----------------------------------------------------------------------------
// Emulator gui
//-----------------------------------------------------------------------------

/*
function updateInstructionCount (es) {
    if (es.thread_host === ES_gui_thread) {
        guiDisplayNinstr (es)
    }
// document.getElementById("nInstrExecuted").innerHTML = n;
}
*/


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
    com.mode.devlog (`h=${h} w=${w}`);
    let n = es.metadata.listingPlain.length;
    com.mode.devlog(`getListingDims: n=${n}`);
    pxPerChar = n ? h/n : 10; // update this global variable, used for scrolling
    com.mode.devlog (`getListingDims: pxPerChar = ${pxPerChar}`);
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


// Should move to gui.mjs
function refreshIOlogBuffer() {
    com.mode.devlog (`refreshIOlogBugfer ${ioLogBuffer}`);
    console.log (`refreshIOlogBugfer ${ioLogBuffer}`);
    let elt = document.getElementById("IOlog");
    elt.innerHTML = "<pre>" + ioLogBuffer + "</pre>";
    elt.scrollTop = elt.scrollHeight;
}


export let ioLogBuffer = "";

// export const procAsmListingElt = document.getElementById('ProcAsmListing');

// export let procAsmListingElt; // global variables for emulator

// Update the memory string for each location that has been accessed,
// so that it contains an html div element which can be used to
// highlight the memory location.  Do the fetches first, then the
// stores: this ensures that if a location has both been fetched and
// stored, the highlighting for the store will take precedence.

function updateMemory (es) {
    // Clear previous highlighting
    for (let x of memFetchInstrLogOld) { memRefresh (es) }
    for (let x of memFetchDataLogOld)  { memRefresh (es) }
    for (let x of memStoreLogOld)      { memRefresh (es) }
    // Update new memory accesses
    for (let x of memFetchInstrLog)    { memHighlight (es, x, "GET") }
    for (let x of memFetchDataLog)     { memHighlight (es, x, "GET") }
    for (let x of memStoreLog)         { memHighlight (es, x, "PUT") }
    memFetchInstrLogOld = memFetchInstrLog
    memFetchDataLogOld = memFetchDataLog
    memStoreLogOld = memStoreLog
    memFetchInstrLog = []
    memFetchDataLog = []
    memStoreLog = []
}

// Create a string with a span class to represent a memory location
// with highlighting; the actual value is in the memory array, and the
// string is placed in the memString array.

// Set the memory displays, using the memString array.  Check mode to
// determine whether the display should be partial and fast or
// complete but slow.

function memDisplay (es) {
    if (memDisplayModeFull) { memDisplayFull (es) }
    else { memDisplayFast (es) }
}

// Set the memory displays, showing only part of the memory to save time

function memDisplayFast (es) {
    let xa, xb, xs1, xs, yafet, yasto, ya, yb, ys1, ys;
    xa = (memFetchInstrLog.length===0) ? 0 : (memFetchInstrLog[0] - memDispOffset);
    xa = xa < 0 ? 0 : xa;
    xb = xa + memDisplayFastWindow;
    xs = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
    	+ memString.slice(xa,xb).join('\n')
	+ "</code></pre>";
//    com.mode.devlog ('  xa=' + xa + '  xb=' + xb);
    guiDisplayMem (es, es.memElt1, xs)
//    memElt1.innerHTML = xs;
    yafet = (memFetchDataLog.length===0) ? 0 : (memFetchDataLog[0] - memDispOffset);
    yasto = (memStoreLog.length===0) ? 0 :(memStoreLog[0] - memDispOffset);
    ya = yafet > 0 && yafet < yasto ? yafet : yasto;
    ya = ya < 0 ? 0 : ya;
    yb = ya + memDisplayFastWindow;
    ys = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	+ memString.slice(ya,yb).join('\n')
	+ "</code></pre>";
//    com.mode.devlog ('  ya=' + ya + '  yb=' + yb);
    //    memElt2.innerHTML = ys;
    guiDisplayMem (es, es.memElt2, xs)
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

function memDisplayFull (es) {
    let memElt1 = document.getElementById('MemDisplay1');
    let memElt2 = document.getElementById('MemDisplay2');

    let xs;                 // display text
    let xt, xo;             // display 1 targets and offsets
    let yafet, yasto, ya, yo, yt;
    com.mode.devlog ('memDisplayFull');
    xs = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	+ memString.join('\n')
	+ "</code></pre>";
    memElt1.innerHTML = xs;
    xt = (memFetchInstrLog.length===0)? 0 : memFetchInstrLog[0] - memDispOffset;
    xo = xt * pxPerChar;
    com.mode.devlog('  target1 xt = ' + xt + '   offset1 = ' + xo);
    memElt1.scroll(0,xo);
    
    memElt2.innerHTML = xs;
    yafet = (memFetchDataLog.length===0) ? 0 : (memFetchDataLog[0] - memDispOffset);
    yasto = (memStoreLog.length===0) ? 0 :(memStoreLog[0] - memDispOffset);
    yt = (yasto > 0 ? yasto : yafet) - memDispOffset;
    yt = yt < 0 ? 0 : yt;
    yo = yt * pxPerChar;
    com.mode.devlog('  yafet=' + yafet + ' yasto=' + yasto
		+ '  target1 yt = ' + yt + '   offset1 = ' + yo);
    memElt2.scroll(0,yo);
}


// Global variables for handling listing display as program runs.

let srcLine;        // copy of source statements

// Keep track of the address of the currently executing instruction,
// the address of the next instruction, and the line numbers where
// these instructions appear in the assembly listing.  -1 indicates no
// line has been highlighted

// let curInstrAddr, curInstrLineNo, saveCurSrcLine;
// let nextInstrAddr, nextInstrLineNo, saveNextSrcLine;

export function initializeSubsystems () {
    memDisplayModeFull = false;
//     document.getElementById('PP_Toggle_Display').value = "Fast display";  
}

export function toggleFullDisplay () {
    com.mode.devlog ('toggleFullDisplay clicked');
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
	com.mode.devlog('Setting modeHighlight to True');
	modeHighlight = true;
    }
    else {
	com.mode.devlog('Setting modeHighlight to False');
	modeHighlight = false;
	refreshRegisters();
    }
}

function highlightText (txt,tag) {
    return "<span class='" + tag + "'>" + txt + "</span>";
}

function prepareListingBeforeInstr (es) {
    com.mode.trace = true;
    com.mode.devlog ('prepareListingBeforeInstr');
    showListingParameters (es)

    if (es.curInstrLineNo >= 0) {
        com.mode.devlog (`prepare resetting cur: line ${es.curInstrLineNo}`)
        showLst (es, "prepare before revert current", es.curInstrLineNo)
        com.revertListingLine (es, es.curInstrLineNo)
        showLst (es, "prepare after revert current", es.curInstrLineNo)
    }
    if (es.nextInstrLineNo >= 0) {
        com.mode.devlog (`prepare resetting next: line ${es.nextInstrLineNo}`)
        showLst (es, "prepare before revert next", es.nextInstrLineNo)
        com.revertListingLine (es, es.nextInstrLineNo)
        showLst (es, "prepare after revert next", es.nextInstrLineNo)
    }
    es.curInstrLineNo = -1;
    es.nextInstrLineNo = -1;
    showListingParameters(es);
    com.mode.devlog ("returning from prepareListingbeforeInstr")
    showListingParameters (es)
    com.mode.trace = false;
}

// As it executes an instruction, the emulator sets curInstrAddr and
// nextInstrAddr.  After the instruction has finished, these
// instructions are highlighted in the listing

function highlightListingAfterInstr (es) {
    com.mode.trace = true;
    com.mode.devlog ('highlightListingAfterInstr');
    showListingParameters (es)
    com.mode.devlog ('  curInstrAddr = ' + es.curInstrAddr);
    com.mode.devlog ('  nextInstrAddr = ' + es.nextInstrAddr);

// Clear any statement highlighting, if any

        if (es.curInstrLineNo >= 0) {
        com.mode.devlog (`prepare resetting cur: line ${es.curInstrLineNo}`)
        showLst (es, "prepare before revert current", es.curInstrLineNo)
        com.revertListingLine (es, es.curInstrLineNo)
        showLst (es, "prepare after revert current", es.curInstrLineNo)
    }
    if (es.nextInstrLineNo >= 0) {
        com.mode.devlog (`prepare resetting next: line ${es.nextInstrLineNo}`)
        showLst (es, "prepare before revert next", es.nextInstrLineNo)
        com.revertListingLine (es, es.nextInstrLineNo)
        showLst (es, "prepare after revert next", es.nextInstrLineNo)
    }
    es.curInstrLineNo = -1;
    es.nextInstrLineNo = -1;

    // Highlight the instruction that just executed
    es.curInstrLineNo = es.metadata.getSrcIdx (es.curInstrAddr)
            + listingLineInitialOffset;
    showLst (es, "highlight, before highlight cur", es.curInstrLineNo)
    com.highlightListingLine (es, es.curInstrLineNo, "CUR");
    showLst (es, "highlight, after highlight cur", es.curInstrLineNo)
    com.mode.devlog (`Highlight current instruction: a=${es.curInstrAddr}`
                 + ` s=${es.curInstrLineNo}`)

    // Highlight the instruction that will be executed next
    es.nextInstrLineNo = es.metadata.getSrcIdx (es.nextInstrAddr)
        + listingLineInitialOffset;
    showLst (es, "highlight, before highlight next", es.nextInstrLineNo)
    com.highlightListingLine (es, es.nextInstrLineNo, "NEXT");
    showLst (es, "highlight, after highlight next", es.nextInstrLineNo)
    com.mode.devlog (`Highlight next instruction: a=${es.nextInstrAddr}`
                 + ` s=${es.nextInstrLineNo}`)

    // Display the memory
    if (memDisplayModeFull) {
	highlightListingFull (es)
    } else {
	highlightListingFull (es)    // temporary ?????
    }
    showListingParameters (es)
    com.mode.devlog ("returning from highlightlistingAfterInstr")
    com.mode.trace = false;
}

function highlightListingFull (es,m) {
    com.mode.devlog ('highlightListingFull');
    setProcAsmListing (es);
    let xa = es.curInstrLineNo - asmScrollOffsetAbove;
    xa = xa < 0 ? 0 : xa;
    let scrollOffset = xa * pxPerChar;
    com.mode.devlog ('curInstrLineNo = ' + es.curInstrLineNo
		     + '  scrollOffset = ' + scrollOffset);
    let procAsmListingElt = document.getElementById('ProcAsmListing');
    procAsmListingElt.scroll (0, scrollOffset);
//    let curline = procAsmListingElt.getElementById('CUR');
//    curline.scrollIntoView();
}

function setProcAsmListing (es) {
    com.mode.devlog ('setProcAsmListing');
    let xs = "<pre><code class='HighlightedTextAsHtml'>"
    	+ es.asmListingCurrent.join('\n')
	+ "</code></pre>";
    document.getElementById('ProcAsmListing').innerHTML = xs;
}

// // Initialize machine state

// Processor elements: html elements for displaying instruction decode

export function initializeProcessorElements (es) {
    //    if (es.mode === Mode_GuiDisplay) {
    if (es.thread_host === ES_gui_thread) {
        com.mode.devlog ('initializeProcessorElements');
        es.instrCodeElt = document.getElementById("InstrCode");
        es.instrFmtElt  = document.getElementById("InstrFmt");
        es.instrOpElt   = document.getElementById("InstrOp");
        es.instrArgsElt = document.getElementById("InstrArgs");
        es.instrEAElt   = document.getElementById("InstrEA");
        es.instrCCElt   = document.getElementById("InstrCC");
        es.instrEffect1Elt = document.getElementById("InstrEffect1");
        es.instrEffect2Elt = document.getElementById("InstrEffect2");
    }
}

function refreshInstrDecode (es) {
    com.mode.devlog ("refreshInstrDecode");
//    if (false) { // ????????????????????? temporary...
    if (es.thread_host === ES_gui_thread) {
        es.instrCodeElt.innerHTML = es.instrCodeStr;
        es.instrFmtElt.innerHTML  = es.instrFmtStr;
        es.instrOpElt.innerHTML   = es.instrOpStr;
        es.instrArgsElt.innerHTML = showArgs(es); // instrArgsStr;
        es.instrEAElt.innerHTML   = es.instrEAStr;
        //        let ccval = es.regfile[15].val;
        let ccval = es.regfile[15].get ()
        es.instrCCElt.innerHTML      = arith.showCC(ccval);
        es.instrEffect1Elt.innerHTML = showEffect(es,0);
        es.instrEffect2Elt.innerHTML = showEffect(es,1);
    }
}

// Memory display

// Refresh all the memory strings; the memString array should be accurate
// but this function will recalculate all elements of that array

// Note on data structure.  I tried having a preliminary element [0]
// containing just "<pre class='HighlightedTextAsHtml'>", so address a
// is shown in memString[a+1].  The indexing was fine but the
// scrolling didn't work, possibly because of this dummy element with
// its newline inserted when the array is joined up.

function memRefresh (es) {
    memString = [];  // clear out and collect any existing elements
    for (let i = 0; i < memSize; i++) {
	setMemString(es,i);
    }
}

// Create a string to represent a memory location; the actual value is
// in the memory array, and the string is placed in the memString
// array.  memString[0] = <pre class="HighlightedTextAsHtml"> and
// mem[a] corresponds to memString[a+1].


function memHighlight (es, a, highlight) {
    let x = es.shm[st.EmMemOffset + a]
    memString[a] =
	"<span class='" + highlight + "'>"
	+ arith.wordToHex4(a) + " " + arith.wordToHex4(x)
        + "</span>";
}

function setMemString(es,a) {
    let x = es.shm[st.EmMemOffset + a]
    memString[a] = arith.wordToHex4(a) + ' ' + arith.wordToHex4(x)
}

//-------------------------------------------------
// Register display
//-------------------------------------------------


// Display any changes to registers with highlighting

function updateRegisters (es) {
    if (es.thread_host != ES_gui_thread) {
        console.log (`updateRegisters host=${es.thread_host}: skipping`)
        return
    }
    // Clear previous highlighting by refreshing the registers
    console.log (`${es.regFetchedOld.length}`)
    console.log (`${es.regStoredOld.length}`)
    console.log (`${es.regFetched.length}`)
    console.log (`${es.regStored.length}`)
    for (let x of es.regFetchedOld) { x.refresh () }
    for (let x of es.regStoredOld)  { x.refresh () }
    // Update the new register accesses
    for (let x of es.regFetched) x.highlight ("GET")
    for (let x of es.regStored)  x.highlight ("PUT")
    es.regFetchedOld = es.regFetched
    es.regStoredOld = es.regStored
    es.regFetched = []
    es.regStored = []
}

function initRegHightlghting (es) {
    es.regFetchedOld = []
    es.regStoredOld = []
    es.regFetched = []
    es.regStored = []
}

// Display all the registers and remove highlighting.

function refreshRegisters (es) {
    com.mode.devlog('Refreshing registers');
    if (es.thread_host != ES_gui_thread) {
        console.log (`refreshRegisters host=${es.thread_host}, skipping`)
    }
    for (let i = 0; i < es.nRegisters; i++) {
	es.register[i].refresh();
    }
    regFetched = []
    regFetchedOld = []
    regStored = []
    regStoredOld = []
}

//-------------------------------------------------
// Emulator control from the gui
//-------------------------------------------------

// The Pause button stops the instruction looper and displays the state.

export function procPause(es) {
    com.mode.devlog ("procPause");
    st.showSCBstatus (es)
    st.writeSCB (es, st.SCB_pause_request, 1)
    st.showSCBstatus (es)
    console.log ("em wrote procPause request")
}

//-----------------------------------------------------------------------------
// Gui display
//-----------------------------------------------------------------------------

// These functions display information on the gui; they abstract the
// document DOM out of the emulator

export function guiDisplayNinstr (es) {
    if (es.thread_host === ES_gui_thread) {
        let n = st.readSCB (es, st.SCB_nInstrExecuted)
        document.getElementById("nInstrExecuted").innerHTML = n
    }
}

export function guiDisplayMem (es, elt, xs) {
    if (es.thread_host === ES_gui_thread) elt.innerHTML = xs
}
