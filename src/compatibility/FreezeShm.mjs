// Compatibility test: freeze creation of shared array

let curtext = "<h1>Freeze shared array test</h1>\n<p>"

function output (xs) {
    console.log (`*** output ${xs}`)
    curtext += `${xs}<br>`
    document.getElementById("OptionsBody").innerHTML = curtext
}

function printarr () {
    let xs = "<br>printarr...<br>"
    for (let i = 0;  i < n; i++) xs += ` ${i}->${arr[i]}`
    output (xs)
}

output ("<br>Starting<br>")

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

output ("Trying worker support")
const workerOK = checkBrowserWorkerSupport ()
output (`worker support = ${workerOK}`)

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

output ("Trying shared array buffer")
const shmOK = checkSharedMemorySupport ()
output (`shared memory support = ${shmOK}`)

let maybeShm
function tryNewShm () {
    output ("tryNewShm starting")
    maybeShm = new SharedArrayBuffer (10)
    output ("tryNewShm finished")
}

output ("Calling tryNewShm")
output (tryNewShm () )
output ("Called tryNewShm")

const n = 5
const nb = 2 * n
let sysStateBuf
let arr

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

freezeShm (false)

output ("<br>Updating shared array in main thread<br>")
for (let i = 0;  i < n; i++) arr[i] = i
printarr ()
for (let i = 0;  i < n; i++) arr[i] += 100
printarr ()
output ("<br>Updating array...finished<br>")

output ("<br>Finished<br>")
