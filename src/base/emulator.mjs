// Sigma16: emulator.mjs
// Copyright (C) 2019-2021 John T. O'Donnell
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
// emulator.mjs defines the machine language semantics
//-----------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as st from './state.mjs';
import * as asm from './assembler.mjs';
import * as link from './linker.mjs';

//-----------------------------------------------------------------------------
// Interface to emulator
//-----------------------------------------------------------------------------

// initializeMachineState (es)     - create registers and memory
// procReset (es)                  - set registers and memory to 0
// boot (es)                       - load executable into memory
// mainThreadLooper (es)           - run main until stopping condition
// executeInstructino (es)         - execute one instruction in either thread

// look up these...

// procPause  procInterrupt procBreakpoing procReset
//   timerInterrupt
// TODO Refactoring... Should be in ui

// prepareExecuteInstruction (es)
// execInstrPostDisplay (es)
// setting breakpoints


// Show key information stored in emulator state
export function showEsInfo (es) {
//    const rfet = es.regFetched.map(r=>r.regName) 
//    const rsto = es.regStored.map(r=>r.regName) 
    return `showEsInfo thread=${es.thread_host}\n`
        + `  regFetched = ${es.copyable.regFetched}\n`
        + `  regStored = ${es.copyable.regStored}\n`
        + `  memFetchInstrLog = ${es.copyable.memFetchInstrLog}\n`
        + `  memFetchDataLog = ${es.copyable.memFetchDataLog}\n`
        + `  memStoreLog = ${es.copyable.memStoreLog}\n`
}
//        + `  regFetched = ${es.copyable.regFetched.map(r=>r.regName)}\n`
//        + `  regStored = ${es.copyable.regStored.map(r=>r.regName)}\n`

export let modeHighlightAccess = true;

export function initRegHighlighting (es) {
    console.log ('initRegHighlighting')
    es.regFetchedOld = []
    es.regStoredOld = []
    es.regFetched = []
    es.regStored = []
}

//------------------------------------------------------------------------------
// Emulator state
//------------------------------------------------------------------------------

// The emulator state contains the variables needed to interpret the
// machine language.  It's passed as needed to functions; the
// convention is that the parameter name is 'es'.

// The mode determines how much output the emulator provides and where
// it goes.  The mode values are: 100: gui display, 200: console
// display, 300: fast and quiet.  initialMode is provided when the
// state is created, and mode is the current value, which might change
// during execution.

const initEsCopyable = {
    breakPCvalue : 0,
    breakEnabled : false,
    regFetched : [],
    regStored : [],
    memFetchInstrLog : [],
    memFetchDataLog : [],
    memStoreLog : [],
    memFetchInstrLogOld : [],
    memFetchDataLogOld : [],
    memStoreLogOld : []
}

export function showCopyable (x) {
    console.log (`showCopyable`)
    console.log (`breakEnabled = ${x.breakEenabled}`)
    console.log (`breakPCvalue = ${x.breakPCvalue}`)
    console.log (`regFetched = ${x.regFetched.join(',')}`)
    console.log (`regStored = ${x.regStored.join(',')}`)
    console.log (`memFetchInstrLog = ${x.memFetchInstrLog.join(',')}`)
    console.log (`memFetchDataLog = ${x.memFetchDataLog.join(',')}`)
    console.log (`memStoreLog = ${x.memStoreLog.join(',')}`)
}

export class EmulatorState {
    constructor (thread_host, f, g, h) {
        this.thread_host = thread_host // which thread runs this instance
        console.log (`%cCreating ES thread_host = ${this.thread_host}`, 'color:red')

        // Display functions provided by user interface
        this.clearProcessorDisplay = null
        
        this.initRunDisplay   = f
        this.duringRunDisplay = g
        this.endRunDisplay    = h
        this.shm              = null // set by allocateStateVec
        this.vecbuf           = null
        this.vec32            = null
        this.vec16            = null
        this.emRunCapability  = com.ES_gui_thread // default: run in main thread
        this.emRunThread      = com.ES_gui_thread // default: run in main thread
        this.startTime        = null
        this.eventTimer       = null  // returned by setInterval
        this.emInstrSliceSize = 500 // n instructions before looper yields
	this.instrLooperDelay = 1000
	this.instrLooperShow  = false
	this.breakEnabled     = false
	this.doInterrupt      = 0
        this.ioLogBuffer      = ""
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
	this.instrCode          = 0
	this.instrOp            = ""
	this.instrArgs          = ""
	this.instrEA            = null
	this.instrEAStr         = ""
	this.instrEffect        = []
	this.curInstrAddr       = 0
	this.nextInstrAddr      = 0
        this.instrCodeElt = null
        this.instrFmtElt  = null
        this.instrOpElt   = null
        this.instrArgsElt = null
        this.instrEAElt   = null
        this.instrCCElt   = null
        this.instrEffect1Elt = null
        this.instrEffect2Elt = null
        this.copyable = initEsCopyable
    }
}
//        this.regFetchedOld = []
//        this.regStoredOld = []
//        this.regFetched = []
//        this.regStored = []
//        this.memFetchInstrLog = []
//        this.memFetchInstrLogOld = []
//        this.memFetchDataLog = []
//        this.memFetchDataLogOld = []
//        this.memStoreLog = []
//        this.memStoreLogOld = []


// The system control registers are specified in the instruction by an
// index starting from i=0..., but the actual emulator data structures
// for these registers are held in the register[] array at index
// i+ctlRegIndexOffset...  It's useful to have a single array
// containing all the registers, so the emulator can refresh them all
// together.

export const ctlRegIndexOffset = 20;  // add to ctl reg no. to get register[] index
export let sysCtlRegIdx = 0;          // index of first system control reg
export let registerIndex = 0;         // unique index for each reg
// export let regFetched = [];
// export let regFetchedOld = [];
// export let regStored = [];
// export let regStoredOld = [];

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

//let nRegisters = 0  // number of registers that have been defined

export class genregister {
    constructor (es, regName, eltName, showFcn) {
        this.es = es
        this.regStIndex = es.nRegisters
        this.regNumber = es.nRegisters
        es.nRegisters++
        this.regName = regName
        this.eltName = eltName
        this.show = showFcn
        this.elt = this.es.thread_host === com.ES_gui_thread
            ? document.getElementById (this.eltName)
            : null
        es.register.push (this)
    }
    get () {
        this.es.copyable.regFetched.push (this.regNumber)
        let i = st.EmRegBlockOffset + this.regStIndex
        let x = this.regStIndex === 0 ? 0 : this.es.shm[i]
        return x
    }
    put (x) {
        this.es.copyable.regStored.push (this.regNumber)
        let i = st.EmRegBlockOffset + this.regStIndex
        this.es.shm[i] = x
        if (this.regIdx < 16) { // register file
            instrEffect.push (["R", this.regNumber, x, this.regName]);
        }
//        com.mode.devlog (`--- reg put ${this.regName} :=`
//                     + ` ${arith.wordToHex4(x)} = ${x} (idx=${i})`)
    }
    highlight (key) {
//        com.mode.devlog (`reg-highlight ${this.regName} ${key}`)
        if (this.es.thread_host === com.ES_gui_thread) {
            let i = st.EmRegBlockOffset + this.regStIndex
            let x = this.regStIndex === 0 ? 0 : this.es.shm[i]
            let xs = com.highlightText (this.show(x), key)
            this.elt.innerHTML = xs
        } else {
        }
    }
    refresh () {
        if (this.es.thread_host === com.ES_gui_thread) {
            let i = st.EmRegBlockOffset + this.regStIndex
            let x = this.regStIndex === 0 ? 0 : this.es.shm[i]
            let xs = this.show (x)
            this.elt.innerHTML = xs
        }
    }
}

// Reset every register to 0

export function resetRegisters (es) {
    com.mode.devlog('Resetting registers');
    for (let i = 0; i < es.nRegisters; i++) {
        es.register[i].put (0)
//        es.register[i].refresh ()
    }
}

//----------------------------------------------------------------------
//  Memory representation and access
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

let memory = [];  // the memory contents, indexed by address
// let memString = []; // a string for each location, to be displayed
// let memElt1, memElt2;  // html elements for two views into the memory IN es
// export let memFetchInstrLog = [];
// export let memFetchDataLog = [];
// export let memStoreLog = [];
// export let memFetchInstrLogOld = [];
// export let memFetchDataLogOld = [];
// export let memStoreLogOld = [];


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

export function memClear (es) {
    for (let a = 0; a < arch.memSize; a++) {
        memStore (es, a, 0)
    }
}
//    clearLoggingData (es)
//    es.copyable.memFetchInstrLog = [];
//    es.copyable.memFetchDataLog = [];
//    es.copyable.memStoreLog = [];
//    memRefresh (es);


// Fetch and return a word from memory at address a, and record the
// address so the display can show this access.

export function memFetchInstr (es, a) {
    es.copyable.memFetchInstrLog.push(a);
    let i = st.EmMemOffset + a
    let x =  es.shm[i]
//    let x = st.sysStateVec [st.EmMemOffset + a]
    com.mode.devlog (`memFetchInstr a=${arith.wordToHex4(a)}`
                 + ` offset=${st.EmMemOffset} i=${i} x=${arith.wordToHex4(x)}`)
    return x
}

export function memFetchData (es, a) {
    es.copyable.memFetchDataLog.push(a);
    let x = es.shm[st.EmMemOffset + a]
    return x
}

// Store a word x into memory at address a, and record the address so
// the display can show this access.

export function memStore (es, a,x) {
    es.copyable.memStoreLog.push(a);
    es.instrEffect.push(["M", a, x]);
    es.shm[st.EmMemOffset + a] = x
}

//------------------------------------------------------------------------------
// Initialize machine state
//------------------------------------------------------------------------------

// Separate clearing state from refreshing display
export function procReset (es) {
    console.log ('em.procReset start')
    com.mode.devlog ("em reset");
    com.mode.devlog ("reset the processor");
    st.resetSCB (es)
    resetRegisters (es);
    memClear (es);
    console.log ('em.procReset finished')
//    clearClock (es)
//    refreshDisplay (es)
}

export function initializeMachineState (es) {
    com.mode.devlog ("emulator: initializeMachineState");
    com.mode.devlog (`em.initializeMachineState: current emulator mode = ${es.mode}`)
//    initializeProcessorElements (es);  // so far, it's just instr decode
//    clearInstrDecode (emulatorState);
//    clearInstrDecode (es);

    // Build the register file; sysStateVec index = reg number
    for (let i = 0; i < 16; i++) {
	let regname = 'R' + i; // also the id for element name
        es.regfile[i] = new genregister (es, regname, regname, arith.wordToHex4)
//     es.regfile[i] = new genregister (es, i, regname, regname, arith.wordToHex4)
//	es.register[i] = es.regfile[i]
    }

    // Instruction control registers
    es.pc   = new genregister (es, 'pc',   'pcElt',    arith.wordToHex4);
    es.ir   = new genregister (es, 'ir',   'irElt',    arith.wordToHex4);
    es.adr  = new genregister (es, 'adr',  'adrElt',   arith.wordToHex4);
    es.dat  = new genregister (es, 'dat',  'datElt',   arith.wordToHex4);
//    es.pc   = new genregister (es, 0,  'pc',   'pcElt',    arith.wordToHex4);
//    es.ir   = new genregister (es, 0,  'ir',   'irElt',    arith.wordToHex4);
//    es.adr  = new genregister (es, 0,  'adr',  'adrElt',   arith.wordToHex4);
//    es.dat  = new genregister (es, 0,  'dat',  'datElt',   arith.wordToHex4);

    // Interrupt control registers
    es.statusreg  = new genregister (es, 'statusreg', 'statusElt',  arith.wordToHex4);
//   es.statusreg  = new genregister (es, 0, 'statusreg', 'statusElt',  arith.wordToHex4);
    // bit 0 (lsb) :  0 = User state, 1 = System state
    // bit 1       :  0 = interrupts disabled, 1 = interrupts enabled
    // bit 2       :  0 = segmentation disabled, 1 = segmentation enabled

    es.mask  = new genregister (es, 'mask', 'maskElt',  arith.wordToHex4);
    es.req   = new genregister (es, 'req',  'reqElt',   arith.wordToHex4);
//    es.mask  = new genregister (es, 0, 'mask', 'maskElt',  arith.wordToHex4);
//    es.req   = new genregister (es, 0, 'req',  'reqElt',   arith.wordToHex4);
    // mask and request use the same bit positions for flags
    // bit 0 (lsb)  overflow
    // bit 1        divide by 0
    // bit 2        trap 3
    // bit 3        

    es.rstat    = new genregister (es, 'rstat',  'rstatElt',  arith.wordToHex4);
    es.rpc      = new genregister (es, 'rpc',    'rpcElt',    arith.wordToHex4);
    es.vect     = new genregister (es, 'vect',   'vectElt',   arith.wordToHex4);
//  es.rstat    = new genregister (es, 0, 'rstat',  'rstatElt',  arith.wordToHex4);
//  es.rpc      = new genregister (es, 0, 'rpc',    'rpcElt',    arith.wordToHex4);
//  es.vect     = new genregister (es, 0, 'vect',   'vectElt',   arith.wordToHex4);

// Segment control registers
    es.bpseg = new genregister (es, 'bpseg',  'bpsegElt',  arith.wordToHex4);
    es.epseg = new genregister (es, 'epseg',  'epsegElt',  arith.wordToHex4);
    es.bdseg = new genregister (es, 'bdseg',  'bdsegElt',  arith.wordToHex4);
    es.edseg = new genregister (es, 'edseg',  'edsegElt',  arith.wordToHex4);

//    es.bpseg = new genregister (es, 0, 'bpseg',  'bpsegElt',  arith.wordToHex4);
//  es.epseg = new genregister (es, 0, 'epseg',  'epsegElt',  arith.wordToHex4);
//  es.bdseg = new genregister (es, 0, 'bdseg',  'bdsegElt',  arith.wordToHex4);
//  es.edseg = new genregister (es, 0, 'edseg',  'edsegElt',  arith.wordToHex4);

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


// Must wait until onload event

function memInitialize (es) {
    if (es.thread_host === com.ES_gui_thread) {
        memClear(es);    // set each location to 0
//        memRefresh(es);  // generate a string showing each location
//        es.initializeMemDisplay (es)
//        es.memElt1 = document.getElementById('MemDisplay1');
//        es.memElt2 = document.getElementById('MemDisplay2');
//        memDisplay(es);  // put the strings into the gui display elements
    }
}

//-----------------------------------------------------------------------------
// Interrupts
//-----------------------------------------------------------------------------

/* move to gui
export function timerInterrupt (es) {
    com.mode.devlog ("Timer Interrupt clicked");
    arith.setBitInRegBE (es.req, arch.timerBit);
    es.req.refresh();
}
*/

//------------------------------------------------------------------------------
// Decode instruction
//------------------------------------------------------------------------------

export function showInstrDecode (es) {
    es.instrCodeStr = (es.instrCode ? arith.wordToHex4 (es.instrCode) : "")
	+ " " + (es.instrDisp ? arith.wordToHex4 (es.instrDisp) : "");
    es.instrEAStr = es.instrEA ? arith.wordToHex4 (es.instrEA) : "";
    com.mode.devlog (`showInstrDecode fmt = ${es.instrFmtStr}`);
//    refreshInstrDecode (es);
}

//------------------------------------------------------------------------------
// Controlling instruction execution
//------------------------------------------------------------------------------

// Using the main gui thread, run instructions repeatedly until a
// stopping condition arises.  Yield control each iteration to avoid
// blocking the user interface, particularly the manual timer
// interrupt button.

// Run instructions in the main gui thread.  Caller should check the
// system status before calling the looper; it's assumed that it is ok
// to execute an instruction in the gui thread.

/*
export function startRunMainThread (es) {
    let q = st.readSCB (es, st.SCB_status)
    switch (q) {
    case st.SCB_ready:
    case st.SCB_paused:
    case st.SCB_blocked:
        console.log ("procRunMainThread: start looper");
        es.initRunDisplay (es)
        mainInstructionLooper (es)
        console.log ("procRunMainThread: looper finished");
//        execInstrPostDisplay (es) no, 
        break
    default:
        com.mode.devlog (`procRunMainThread skipping: SCB_status=${q}`)
    }
}
*/

export function mainThreadLooper (es) {
    com.mode.devlog ("mainInstructionLooper starting")
    let i = 0
    let status = 0
    let pauseReq = false
    let continueRunning = true
    let finished = false
    let externalBreak = false
    while (continueRunning) {
        executeInstruction (es)
        i++
        status = st.readSCB (es, st.SCB_status)
        switch (status) {
        case st.SCB_halted:
        case st.SCB_paused:
        case st.SCB_break:
        case st.SCB_relinquish:
            finished = true
            break
        default:
        }
        externalBreak = es.copyable.breakEnabled
            && (es.pc.get() === es.copyable.breakPCvalue)
        if (externalBreak) finished = true

        console.log (`em after exec checkbreak: pc=${es.pc.get()}`
                     + ` bEN=${es.copyable.breakEnabled}`
                     + ` bPC=${es.copyable.breakPCvalue}`
                     + ` eb=${externalBreak}`)

        pauseReq = st.readSCB (es, st.SCB_pause_request) != 0
        continueRunning = !finished  && !pauseReq && i < es.emInstrSliceSize
    }
        if (pauseReq && status != st.SCB_halted) {
            com.mode.devlog ("main looper pausing")
            st.writeSCB (es, st.SCB_status, st.SCB_paused)
            st.writeSCB (es, st.SCB_pause_request, 0)
        } else if (externalBreak) {
            console.log (`external breakpoint`)
            st.writeSCB (es, st.SCB_status, st.SCB_break)
        }
    if (finished) {
        es.endRunDisplay (es)
    } else {
        es.duringRunDisplay (es)
	setTimeout (function () {mainThreadLooper (es)})
    }
}

//------------------------------------------------------------------------------
// Wrapper around instruction execution
//------------------------------------------------------------------------------

// When running, the logging data isn't needed.  The worker thread
// needs to clear it to prevent a space leak.

export function clearMemLogging (es) {
    es.copyable.memFetchInstrLog = []
    es.copyable.memFetchInstrLogOld = []
    es.copyable.memFetchDataLog = []
    es.copyable.memFetchDataLogOld = []
    es.copyable.memStoreLog = []
    es.copyable.memStoreLogOld = []
}

export function clearRegLogging (es) {
    es.copyable.regFetched = []
    es.copyable.regStored = []
}

/*
export function clearInstrDecode (es) {
    return ; // ?????????????????????
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

*/

//------------------------------------------------------------------------------
// Machine language semantics
//------------------------------------------------------------------------------

export function clearInstrDecode (es) {
    es.instrOpCode = null
    es.instrDisp = null
    es.instrCodeStr  = ""
    es.instrFmtStr   = ""
    es.instrOp    = ""
    es.instrArgs  = ""
    es.instrEA = null
    es.instrEAStr    = ""
    es.instrEffect1 = ""
    es.instrEffect2 = ""
    es.instrEffect = []
}

// Execute one instruction; runs in either in any thread.  The choice
// of thread is determined by es.

export function executeInstruction (es) {
    com.mode.devlog (`em.executeInstruction starting`)
    //    clearRegLoggingData (es)
    clearRegLogging (es)
    clearMemLogging (es)
    clearInstrDecode (es)
    // Check for interrupt
    let mr = es.mask.get() & es.req.get() // ???
    com.mode.devlog (`interrupt mr = ${arith.wordToHex4(mr)}`)
    st.writeSCB (es, st.SCB_cur_instr_addr, es.pc.get())
    if (arith.getBitInRegBE (es.statusreg, arch.intEnableBit) && mr) {
        com.mode.devlog (`execute instruction: interrupt`)
        com.mode.devlog (`execute instruction: interrupt`)
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
    es.instrCode = memFetchInstr (es, es.curInstrAddr);
    com.mode.devlog (`ExInstr ir=${arith.wordToHex4(es.instrCode)}`)
    es.ir.put (es.instrCode);
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
    st.incrInstrCount (es)
//    console.log (`Finished executeInstruction: ${showEsInfo(es)}`)
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
// if d=15 the secondary is discarded and the primary is loaded into
// R15.  The instruction semantics is defined by f.

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
    console.log (`*** op_trap es.thread_host=${es.thread_host}`)
//    switch (es.thread_host) {
//    case com.ES_gui_thread:
        com.mode.devlog (`handle trap in main thread`)
        console.log (`handle trap in main thread`)
        let code = es.regfile[es.ir_d].get();
        com.mode.devlog (`trap code=${code}`);
        if (code===0) { // Halt
	    console.log ("Trap: halt");
	    com.mode.devlog ("Trap: halt");
            st.writeSCB (es, st.SCB_status, st.SCB_halted)
        } else if (code==1) { // nonblocking read
            console.log ('trap: nonblocking read')
            trapRead(es);
        } else if (code==2) { // nonblocking write
            console.log ('trap: nonblocking write')
            trapWrite(es);
        } else if (code==3) { //blocking read
            console.log ('trap: blocking read (not implemented)')
        } else if (code==4) { // break
            console.log ('trap: break')
            st.writeSCB (es, st.SCB_status, st.SCB_break)
        } else { // Undefined trap is nop
            com.mode.devlog (`trap with unbound code = ${code}`)
        }
}
//        break
//    case com.ES_worker_thread:
//        com.mode.devlog (`emworker: relinquish control on a trap`)
//        st.writeSCB (es, st.SCB_status, st.SCB_relinquish)
//        com.mode.devlog (`trap relinquish before fixup, pc = ${es.pc.get()}`)
//        es.pc.put (st.readSCB (es, st.SCB_cur_instr_addr))
//        com.mode.devlog (`trap relinquish after fixup, pc = ${es.pc.get()}`)
//        break
//    default:
//        console.log (`system error: trap has bad shm_token ${q}`)
//    }

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
    es.ioLogBuffer += com.highlightField(ys,"READ"); // display input
    refreshIOlogBuffer (es)
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
    console.log (`Write a=${a} b=${b} >>> /${xs}/`);
    es.ioLogBuffer += xs;
    com.mode.devlog (es.ioLogBuffer);
    refreshIOlogBuffer (es)
}

// Should make more abstract; shouldn't refer to DOM
export function refreshIOlogBuffer (es) {
    com.mode.devlog (`refreshIOlogBugfer ${es.ioLogBuffer}`);
    com.mode.devlog (`refreshIOlogBugfer ${es.ioLogBuffer}`);
    let elt = document.getElementById("IOlog");
    elt.innerHTML = "<pre>" + es.ioLogBuffer + "</pre>";
    elt.scrollTop = elt.scrollHeight;
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
	com.mode.devlog (`>>> dispatching EXP code=${code} d=${es.ir_d}`);
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
    com.mode.devlog (`>>> EXP2 instruction`);
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

// **********************************************************************
// Deprecated
// **********************************************************************

// Processor execution status - deprecated, use fields in es
// Global variables for instruction decode; used in emulator
// let displayInstrDecode = true;
// let instrCode, instrDisp, instrCodeStr; // record the values
// let instrFmtStr = "";
// let instrOpStr = "";
// let instrArgsStr = "";
// let instrEA, instrEAStr;
// let instrEffect = [];
// Fields of the current instruction
// let instr = 0;
// let ir_op = 0, ir_d = 0, ir_a = 0, ir_b = 0;  // instruction fields
// The value of the effective addresss pecified by an RX instruction
// let ea = 0;  // effective address


//----------------------------------------------------------------------
//  Registers - old version
//----------------------------------------------------------------------

// for tracing: highlight reg/mem that is accessed

// The registers are defined as global variables.  These variables are
// declared here but their values are set only when window has been
// loaded (the onload event in gui.js), because the gui elements will
// exist at that time.

// Declare the registers - window.onload sets their values
// export let regFile = [];                       // register file R0,..., R15
//export let pc, ir, adr, dat;                    // instruction control registers
// export let statusreg, mask, req, rstat, rpc, vect; // interrupt control registers
// export let bpseg, epseg, bdseg, edseg;             // segment control registers

// Generating and accessing registers
// export let controlRegisters;   // array of the control registers
// export let register = [];      // array of all the registers: regfile and control

// trap main thread, halted...
//            displayFullState (es)
//            updateRegisters (es)  update display in ui
//            updateMemory (es)
//            memRefresh(es);
