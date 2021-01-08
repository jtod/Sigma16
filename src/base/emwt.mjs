// Sigma16: emthread.mjs
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

// Emulator thread

import * as shm from "./shmparams.mjs"
import * as arch from './architecture.mjs'
import * as arith from './arithmetic.mjs';
import * as st  from './state.mjs';
import * as em from "./emulator.mjs"

//-----------------------------------------------------------------------------
// Emulator state
//-----------------------------------------------------------------------------

let emwt = {
    shm: null, // shared system state vector
    es: null // emulator state
    }


//-----------------------------------------------------------------------------
// Communication protocol
//-----------------------------------------------------------------------------

self.addEventListener ("message", e => {
    if (e.data) {
        console.log (`ent code = ${e.data.code}`)
        let result = 0 // default
        let msg = {code:0 , payload: 0} // default
        switch (e.data.code) {
        case 100: // emwt init: received shared system state vector
            console.log ("emwt: received request init")
            emwt.shm = e.data.payload
            emwt.es = new em.EmulatorState (em.ES_worker_thread, emwt.shm)
            em.initializeMachineState (emwt.es)
            emwt.initialized = true
            msg = {code: 200, payload: 0}
            self.postMessage (msg)
            break
        case 101: // emwt step
            console.log (`emwt: received request step`)
            doStep ()
            msg = {code: 201, payload: 0}
            self.postMessage (msg)
            break
        case 102: // emwt run
            console.log (`emwt: received request run`)
            result = doRun (e.data.payload)
            msg = {code: 202, payload: result}
            self.postMessage (msg)
            break
        case 103: // show
            console.log (`emwt: received request show`)
            show ()
            msg = {code: 203, payload: 0}
            self.postMessage (msg)
            break
        case 104: // test 1
            console.log (`emwt: received request test 1`)
            //            let slowResult = longComputation ()
            result = runTest1 ()
            msg = {code: 204, payload: result}
            self.postMessage (msg)
            break
        case 105: // test 2
            console.log (`emwt: received request test 2`)
            rmtestResult = regMemwtest ()
            msg = {code: 205, payload: rmtestResult}
            self.postMessage (msg)
            break
        default:
            console.log (`emwt: received unknown code ${e.data.code}`)
        }
    }
})

//-----------------------------------------------------------------------------
// Registers
//-----------------------------------------------------------------------------

// Declare the registers - window.onload sets their values

let regFile = [];                            // register file R0,..., R15
let pc, ir, adr, dat;                        // instruction control registers
let statusreg, mask, req, istat, ipc, vect;  // interrupt control registers
let bpseg, epseg, bdseg, edseg;              // segment control registers

// Generating and accessing registers

let controlRegisters;   // array of the control registers
let register = [];      // array of all the registers: regfile and control

// The system control registers are specified in the instruction by an
// index starting from i=0..., but the actual emulator data structures
// for these registers are held in the register[] array at index
// i+ctlRegIndexOffset...  It's useful to have a single array
// containing all the registers, so the emulator can refresh them all
// together.

const ctlRegIndexOffset = 20;  // add to ctl reg no. to get register[] index
let sysCtlRegIdx = 0;          // index of first system control reg
let registerIndex = 0;         // unique index for each reg

let nRegisters = 0  // number of registers that have been defined

class genregister {
    constructor (regNumber, regName, eltName, showFcn) {
        this.regStIndex = nRegisters
        nRegisters++
        this.regNumber = regNumber
        this.regName = regName
        this.eltName = eltName // not used in emwt
        this.show = showFcn
//        this.elt = document.getElementById (eltName)
//	this.elt.innerHTML = this.regStIndex
        console.log (`--- Generate reg regNum=${this.regNumber}`
                     + ` name=${this.regName} idx=${this.regStIndex}`)
    }
    get () {
//        regFetched.push (this)
        let i = shm.EmRegBlockOffset + this.regStIndex
        console.log (`----- get  ${this.regName} erbo=${shm.EmRegBlockOffset} rsi=${this.regStIndex} `)
        let x = this.regStIndex === 0 ? 0 : sysStateVec [i]
        console.log (`--- reg get ${this.regName} =`
                     + ` ${arith.wordToHex4(x)} = ${x} (idx=${i})`)
        return x
    }
    put (x) {
//        regStored.push (this)
        let i = shm.EmRegBlockOffset + this.regStIndex
        sysStateVec [i] = x
//        if (this.regIdx < 16) { // register file
//            instrEffect.push (["R", this.regNumber, x, this.regName]);
//        }
        console.log (`--- reg put ${this.regName} :=`
                     + ` ${arith.wordToHex4(x)} = ${x} (idx=${i})`)
    }
    highlight (key) {
//        let i = st.EmRegBlockOffset + this.regStIndex
//        let x = this.regStIndex === 0 ? 0 : st.sysStateVec [i]
//        let xs = highlightText (this.show(x), key)
//        console.log (`--- reg highlight ${this.regName} := ${xs} (idx=${i})`)
//        this.elt.innerHTML = xs
    }
    refresh () {
//        let i = st.EmRegBlockOffset + this.regStIndex
//        let x = this.regStIndex === 0 ? 0 : st.sysStateVec [i]
//        let xs = this.show (x)
//        console.log (`--- reg refresh ${this.regName} :=`
//                     + ` ${arith.wordToHex4(x)} = ${x} /${xs}/ (idx=${i})`)
//        this.elt.innerHTML = xs
    }
}

function initializeMachineState () {
     console.log ("emulator: initializeMachineState");
//     initializeProcessorElements ();  // so far, it's just instr decode
//     clearInstrDecode (emulatorState);

    // Build the register file; sysStateVec index = reg number
    for (let i = 0; i < 16; i++) {
        let regname = 'R' + i; // also the id for element name
        regFile[i] = new genregister (i, regname, regname, arith.wordToHex4)
        register[i] = regFile[i]
    }

    // Instruction control registers
    pc   = new genregister (0,  'pc',   'pcElt',    arith.wordToHex4);
    ir   = new genregister (0,  'ir',   'irElt',    arith.wordToHex4);
    adr  = new genregister (0,  'adr',  'adrElt',   arith.wordToHex4);
    dat  = new genregister (0,  'dat',  'datElt',   arith.wordToHex4);
    
// Interrupt control registers
    statusreg  = new genregister (0, 'statusreg', 'statusElt',  arith.wordToHex4);
    // bit 0 (lsb) :  0 = User state, 1 = System state
    // bit 1       :  0 = interrupts disabled, 1 = interrupts enabled
    // bit 2       :  0 = segmentation disabled, 1 = segmentation enabled

    mask  = new genregister (0, 'mask', 'maskElt',  arith.wordToHex4);
    req   = new genregister (0, 'req',  'reqElt',   arith.wordToHex4);
    // mask and request use the same bit positions for flags
    // bit 0 (lsb)  overflow
    // bit 1        divide by 0
    // bit 2        trap 3
    // bit 3        

    istat    = new genregister (0, 'istat',  'istatElt',  arith.wordToHex4);
    ipc      = new genregister (0, 'ipc',    'ipcElt',    arith.wordToHex4);
    vect     = new genregister (0, 'vect',   'vectElt',   arith.wordToHex4);

// Segment control registers
    bpseg = new genregister (0, 'bpseg',  'bpsegElt',  arith.wordToHex4);
    epseg = new genregister (0, 'epseg',  'epsegElt',  arith.wordToHex4);
    bdseg = new genregister (0, 'bdseg',  'bdsegElt',  arith.wordToHex4);
    edseg = new genregister (0, 'edseg',  'edsegElt',  arith.wordToHex4);

// Record the control registers    
    controlRegisters =
	[pc, ir, adr, dat,   // not accessible to getctl/putctl instructions
	 // the following can be used for getctl/getctl, indexing from 0
	 statusreg, mask, req, istat, ipc, vect,
         bpseg, epseg, bdseg, edseg
	]

//    memInitialize();
//    resetRegisters();
}



 
//-----------------------------------------------------------------------------
// Memory
//-----------------------------------------------------------------------------

function memFetchInstr (a) {
//    memFetchInstrLog.push(a);
    let x = sysStateVec [shm.EmMemOffset + a]
    return x
}

function memFetchData (a) {
//    memFetchDataLog.push(a);
    let x = sysStateVec [shm.EmMemOffset + a]
    return x
}

function memStore (a,x) {
//    memStoreLog.push(a);
//    instrEffect.push(["M", a, x]);
    sysStateVec [shm.EmMemOffset + a] = x
}


//-----------------------------------------------------------------------------
// Test
//-----------------------------------------------------------------------------

function show () {
    showRegs ()
    showMem (20)
}

function showRegs () {
    console.log (`emwt showRegs`)
    for (let i = 0; i < shm.EmRegBlockSize; i++) {
        let j = shm.EmRegBlockOffset + i
        let x = sysStateVec [j]
        //        console.log (`emwt reg[${i}] = ${x} (j=${j})`)
        console.log (`emwt reg[${i}] = ${x}`)
    }
}

function showMem (limit) {
    console.log (`emwt showMem up to ${limit}`)
    for (let a = 0; a <= limit; a++) {
        let j = shm.EmMemOffset + a
        let x = sysStateVec [j]
        console.log (`emwt m[${a}] = ${x}`)
    }
}

function runTest1 () {
    console.log ("emwt long computation, here goes...")
    let sum = 0
    const lim1 = 1000
    const lim2 = 1000
    const lim3 = 1000
    for (let i = 0; i < lim1; i++) {
        for (let j = 0; j < lim2; j++) {
            for (let k = 0; k < lim2; k++) {
                sum += k
            }
        }
    }
    console.log ("emwt long computation finished!")
    return sum
}



function longComputation () {
    console.log ("emwt long computation, here goes...")
    let sum = 0
    const lim1 = 1000
    const lim2 = 1000
    const lim3 = 1000
    for (let i = 0; i < lim1; i++) {
        for (let j = 0; j < lim2; j++) {
            for (let k = 0; k < lim2; k++) {
                sum += k
            }
        }
    }
    console.log ("emwt long computation finished!")
    return sum
}

function regMemTest () {
    let a = pc.get ()
    console.log (`emwt rmtest a = ${a}`)
    let b = ir.get ()
    console.log (`emwt rmtest b = ${b}`)
    pc.put (a + 10)
    let c = pc.get ()
    console.log (`emwt rmtest c = ${c}`)
    return c
    return x
}

//-----------------------------------------------------------------------------
// Interface to emulator
//-----------------------------------------------------------------------------

function doStep () {
    console.log (`emwt doStep`)
    em.executeInstruction (emwt.es)
    console.log (`emwt doStep returning`)
}

function doRun (limit) {
    console.log (`emwt: start doRun limit=${limit}`)
    let count = st.readNinstrExecuted (emwt.es)
    let curStatus = st.readSCB (emwt.es, st.SCB_status)
    let curPauseReq = 0
    console.log (`starting run status${curStatus}`)
    let continueRunning = curStatus === st.SCB_running_emwt
    while (continueRunning) {
        em.executeInstruction (emwt.es)
        em.clearLoggingData (emwt.es)
        count++
        curPauseReq = st.readSCB (emwt.es, st.SCB_pause_request)
        if (curPauseReq === 1) {
            console.log ("emwt received pause request")
            st.writeSCB (emwt.es, st.SCB_status, st.SCB_paused)
            st.writeSCB (emwt.es, st.SCB_pause_request, 0)
            continueRunning = false
        }
        curStatus = st.readSCB (emwt.es, st.SCB_status)
        if (curStatus === st.SCB_relinquish) {
            continueRunning = false
        }
    }
    st.writeNinstrExecuted (emwt.es, count)
    console.log (`emwt run stopped, count=${count},`
                 + `status=${curStatus}, pause=${curPauseReq}`)
    return count
}

//    let startTime = performance.now ()
//    let finishTime = performance.now ()
//    let elapsedTime = (finishTime - startTime) / 1000
//    console.log (`emwt: doRun finished, executed ${count} instructions`
//                 + ` in ${elapsedTime} sec`)
//     b = curStatus === st.SCB_running_emwt && curPauseReq === 0
//        b =  (st.readSCB (emwt.es, st.SCB_status) === st.SCB_running_emwt
//              && st.readSCB (emwt.es, st.SCB_pause_request) === 0)
//    console.log (`doRun count=${count} b=${b}`)
//    b =  (st.readSCB (emwt.es, st.SCB_status) === st.SCB_running_emwt
//          && st.readSCB (emwt.es, st.SCB_pause_request) === 0)
//        console.log (`doRun count=${count} b=${b}`)
//        console.log ("emwt doRun execute instruction")
//    while (st.readSCB (emwt.es, st.SCB_status) === st.SCB_running_emwt
//           && st.readSCB (emwt.es, st.SCB_pause_request) === 0) {

console.log ("emwthread has been read")
