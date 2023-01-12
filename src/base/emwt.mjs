// Sigma16: emwt.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3 or later
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

// Emulator worker thread

console.log ("loading emwt.mjs")

import * as com from './common.mjs';
import * as ab from './arrbuf.mjs';
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
            emwt.es = new em.EmulatorState (com.ES_worker_thread)
            emwt.es.vecbuf = e.data.payload
            emwt.es.vec16 = new Uint16Array (emwt.es.vecbuf)
            emwt.es.vec32 = new Uint32Array (emwt.es.vecbuf)
            emwt.es.shm = emwt.es.vec16

            // Temporary testing...
            emwt.es.vec32[0] = 0 // n instructions executed
            emwt.es.vec32[1] = 0 // 2 * emwt.es.vec32[0]
            // console.log (`***foobar**** ${emwt.es.vec32[0]} ${emwt.es.vec32[1]}`)
            
            em.initializeMachineState (emwt.es)
            emwt.initialized = true
            msg = {code: 200, payload: 0}
            self.postMessage (msg)
            break
        case 101: // emwt step
            console.log (`emwt: received request step`)
            console.log (`emwt before doStep : ${em.showEsInfo (gst.es)}`)
            doStep ()
            console.log (`emwt after doStep : ${em.showEsInfo (gst.es)}`)
            msg = {code: 201, payload: 0}
            self.postMessage (msg)
            break
        case 102: // emwt run
            console.log (`emwt: received request run`)
            emwt.es.copyable = e.data.payload
            em.showCopyable (emwt.es.copyable)
            doRun ()
            msg = {code: 202, payload: emwt.es.copyable}
            console.log (`emwt returning from run...`)
            em.showCopyable (emwt.es.copyable)
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
// Interface to emulator
//-----------------------------------------------------------------------------

function doStep () {
    console.log (`emwt doStep`)
    em.clearRegLogging (emwt.es)
    em.clearMemLogging (emwt.es)
    em.executeInstruction (emwt.es)
    console.log (`emwt doStep returning`)
}

function doRun () {
    console.log ("emwt doRun")
    const es = emwt.es
    em.initRegHighlighting (es)
    em.clearRegLogging (es)
    em.clearMemLogging (es)
    es.sliceUnlimited = true
    emwtLooper (es)
}

function emwtLooper (es) {
    console.log ("emwt instruction looper starting")
    let icount = 0
    let countOK = true
    let status = 0
    let pauseReq = false
    let continueRunning = true
    let finished = false
    let externalBreak = false
    while (continueRunning) {
        em.executeInstruction (es)
        icount++
        status = ab.readSCB (es, ab.SCB_status)
        com.mode.devlog (`looper after instruction, status=${status}`)
        switch (status) {
        case ab.SCB_halted:
        case ab.SCB_paused:
        case ab.SCB_break:
        case ab.SCB_relinquish:
            finished = true
            break
        default:
        }
        externalBreak = es.copyable.breakEnabled
            && (es.pc.get() === es.copyable.breakPCvalue)
        if (externalBreak) finished = true
        pauseReq = ab.readSCB (es, ab.SCB_pause_request) != 0
        countOK = es.sliceUnlimited || icount < es.emInstrSliceSize
        continueRunning = !finished  && !pauseReq && countOK
    }
    com.mode.devlog ('discontinue instruction looper')
    if (pauseReq && status != ab.SCB_halted) {
        com.mode.devlog ("pausing execution")
        ab.writeSCB (es, ab.SCB_status, ab.SCB_paused)
        ab.writeSCB (es, ab.SCB_pause_request, 0)
    } else if (externalBreak) {
        console.log ("emwt stopping at breakpoint")
        ab.writeSCB (es, ab.SCB_status, ab.SCB_break)
    }
}

console.log ("finished loading emwt.mjs")
