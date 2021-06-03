// Compatibility test: shared arrays

let curtext = "<h1>Shared array test</h1>\n<p>"

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

output ("<br>Defining shared array buffer<br>")
const n = 5
const nb = 2 * n
export const sysStateBuf = new SharedArrayBuffer (nb)
output ("<br>Have definined shared array buffer<br>")

output ("<br>Defining array view onto buffer<br>")
export const arr = new Uint16Array (sysStateBuf)
output ("<br> Defined array view onto buffer<br>")

output ("<br>Updating shared array in main thread<br>")
for (let i = 0;  i < n; i++) arr[i] = i
printarr ()
for (let i = 0;  i < n; i++) arr[i] += 100
printarr ()
output ("<br>Updating array...finished<br>")

output ("<br>Finished<br>")
