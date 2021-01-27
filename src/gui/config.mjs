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

export function output (xs) {
    console.log (`config: output ${xs}`)
//    let txt = document.getElementById("OptionsBody").innerHTML
//    document.getElementById("OptionsBody").innerHTML = txt + `${xs}<br>`
}

export function configureOptions (gst) {
    console.log ("configurerOptions")
    checkLocalStorageSupport (gst)
    checkBrowserWorkerSupport (gst)
    checkSharedMemSupport (gst)
    initializeListeners (gst)
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

export function initializeListeners (gst) {
    document.getElementById("RTmain")
        .addEventListener ("change", setRTmain)
    document.getElementById("RTworkerShm")
        .addEventListener ("change", setRTworkerShm)
    
    document.getElementById("MTsliceSize")
        .addEventListener ("keydown", (e) => { e.stopPropagation () })
    document.getElementById("MTsliceSize")
        .addEventListener ("change", setMTsliceSize (gst))
}

const setMTsliceSize = (gst) => (e) => {
    console.log ("setMTsliceSize")
    e.stopPropagation ()
    let x = document.getElementById("MTsliceSize").value
    console.log (`Setting MTsliceSize <${x}>`)
}

function setRTmain (event) {
    console.log ("setRTmain")
}

function setRTworkerShm (event) {
    console.log ("setRTworkerShm")
}
