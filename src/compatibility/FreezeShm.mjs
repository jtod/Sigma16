// Compatibility test: freeze creation of shared array

let curtext = "<h1>Freeze shared array test</h1>\n<p>"

function output (xs) {
    console.log (`*** output ${xs}`)
    curtext += xs
    document.getElementById("textarea").innerHTML = curtext
}

function printarr () {
    let xs = "<br>printarr...<br>"
    for (let i = 0;  i < n; i++) xs += ` ${i}->${arr[i]}`
    output (xs)
}

output ("<br>Starting<br>")

function shmFeatureTest () {
    let ok = false
    let a = true
    return ok
}

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
