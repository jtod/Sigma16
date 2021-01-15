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


function output (xs) {
    console.log (`*** output ${xs}`)
    let txt = document.getElementById("OptionsBody").innerHTML
    document.getElementById("OptionsBody").innerHTML = txt + `${xs}<br>`
}

function printarr () {
    let xs = "<br>printarr...<br>"
    for (let i = 0;  i < n; i++) xs += ` ${i}->${arr[i]}`
    output (xs)
}

function shmFeatureTest () {
    let ok = window.SharedArrayBuffer
    let a = true
    return ok
}

function checkBrowserWorkerSupport () {
    output ("checkBrowserWorkerSupport")
    let workersSupported = false
    if (window.Worker) {
        output ("Browser supports concurrent worker threads");
        workersSupported = true
    } else {
        output ("Browser does not support concurrent worker threads");
    }
    return workersSupported
}

function checkSharedMemorySupport () {
    output ("checkSharedMemorySupport")
    let shmSupported = false
    if (window.SharedArrayBuffer) {
        output ("Browser supports shared memory");
        shmSupported = true
    } else {
        output ("Browser does not support shared memory");
    }
    return shmSupported
}

function tryNewShm () {
    output ("tryNewShm starting")
    maybeShm = new SharedArrayBuffer (10)
    output ("tryNewShm finished")
}

function freezeShm (b) {
    if (b) {
        output ("freezeShm: true case")
        output ("<br>Defining shared array buffer<br>")
        sysStateBuf = new SharedArrayBuffer (nb)
        output ("<br>Have definined shared array buffer<br>")
        output ("freezeShm just defined sysStateBuf")
        output ("<br>Defining array view onto buffer<br>")
        arr = new Uint16Array (sysStateBuf)
        output ("<br> Defined array view onto buffer<br>")
    } else {
        output ("freezeShm: false case")
        arr = []
    }
}

output ("Starting configuration")
const workerOK = checkBrowserWorkerSupport ()
output (`worker support = ${workerOK}`)
const shmOK = checkSharedMemorySupport ()
output (`shared memory support = ${shmOK}`)

let maybeShm

output ("Calling tryNewShm")
output (tryNewShm () )
output ("Called tryNewShm")

const n = 5
const nb = 2 * n
let sysStateBuf
let arr

freezeShm (false)

output ("<br>Updating shared array in main thread<br>")
for (let i = 0;  i < n; i++) arr[i] = i
printarr ()
for (let i = 0;  i < n; i++) arr[i] += 100
printarr ()
output ("Updating array...finished")

output ("Finished")

/* deprecated
function outputold (xs) {
    console.log (`*** output ${xs}`)
    curtext += `${xs}<br>`
    document.getElementById("OptionsBody").innerHTML = curtext
}

output ("Trying worker support")
output ("Trying shared array buffer")

*/
