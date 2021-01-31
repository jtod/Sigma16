// Sigma16: config.mjs
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

// Configuration: test browser compatibility and set options to
// configure the app


import * as com from '../base/common.mjs';

export function output (xs) {
    console.log (`config: output ${xs}`)
//    let txt = document.getElementById("OptionsBody").innerHTML
//    document.getElementById("OptionsBody").innerHTML = txt + `${xs}<br>`
}

export function configureOptions (gst, es) {
    console.log ("configurerOptions")
    checkLocalStorageSupport (gst)
    checkBrowserWorkerSupport (gst)
    checkSharedMemSupport (gst)
    setMainSliceSize (gst, defaultExecSliceSize)
    initializeListeners (gst)
    let workerShmOK = gst.supportWorker && gst.supportSharedMem
    es.emRunCapability = workerShmOK ? com.ES_worker_thread : com.ES_gui_thread
    es.emRunThread = es.emRunCapability // default: run according to capability
    console.log (`Emulator run capability = ${es.emRunCapability}`)
    let capabilityStr =
        es.emRunCapability === com.ES_worker_thread ? "worker/shm"
        : "main"
    document.getElementById("WorkerThreadOK").innerHTML = workerShmOK
}

function checkLocalStorageSupport (gst) {
    gst.supportLocalStorage = !!window.localStorage
    console.log (`support local storage = ${gst.supportLocalStorage}`)
    document.getElementById("SupportLocalStorage").innerHTML =
        gst.supportLocalStorage
}

function checkBrowserWorkerSupport (gst) {
    console.log ("checkBrowserWorkerSupport")
    gst.supportWorker = !!window.Worker
    console.log (`support worker = ${gst.supportWorker}`)
    document.getElementById("SupportWorker").innerHTML = gst.supportWorker
}

function checkSharedMemSupport (gst) {
    gst.supportSharedMem = !!window.SharedArrayBuffer
    console.log (`support shared memory = ${gst.supportSharedMem}`)
    document.getElementById("SupportSharedMem").innerHTML = gst.supportSharedMem
}

const defaultExecSliceSize = 500

export function initializeListeners (gst) {
    setRTworker (gst) (null) // use worker if available
    document.getElementById("RTmain")
        .addEventListener ("change", setRTmain (gst))
    document.getElementById("RTworkerShm")
        .addEventListener ("change", setRTworker (gst))
    document.getElementById("EnterMainSliceSize")
        .addEventListener ("keydown", (e) => { e.stopPropagation () })
    document.getElementById("UpdateSliceSize")
        .addEventListener ("click", updateMainSliceSize (gst))
}

function setMainSliceSize (gst, x) {
    console.log (`setMainSliceSize ${x}`)
    document.getElementById("MainSliceSize").innerHTML = x
    gst.mainSliceSize = x
}

const updateMainSliceSize = (gst) => (e) => {
    console.log ("updateMTsliceSize")
    e.stopPropagation ()
    let xs = document.getElementById("EnterMainSliceSize").value
    let x = parseInt (xs)
    console.log (`update MTsliceSize <${xs}> = ${x}`)
    if (!isNaN(x)) setMainSliceSize (gst, x)
}

const setRTmain = (gst) => (e) => {
    console.log ("setRTmain")
    gst.emRunThread = com.ES_gui_thread
    document.getElementById("CurrentThreadSelection").innerHTML
        = com.showThread (com.ES_gui_thread)
}

const setRTworker = (gst) => (e) => {
    if (gst.supportWorker && gst.supportSharedMem) {
        console.log ("setRTworker: success")
        gst.emRunThread = com.ES_worker_thread
        document.getElementById("CurrentThreadSelection").innerHTML
            = com.showThread (com.ES_worker_thread)
    } else {
        console.log (`setRTworker: Platform does not support worker, using main`)
        setRTmain (gst) (null)
    }
}
