import * as childproc from 'child_process'

const PAPERTRAIL_TOKEN = process.env.PAPERTRAIL_TOKEN

function getpart (y,m,d,h) {
    console.log (`getpart y=${y} m=${m} d=${d} h=${h}`)
    let ymdh = `${y}-${m}-${d}-${h}`
    ymdh = '2021-06-05-07'
    console.log (`ymdh = ${ymdh}`)
    //    childproc.exec ('papertrail', (err, stdout, stderr) => {
    let cmd = `curl --no-include -o ARCHIVE-${ymdh}.tsv.gz`
        + ' -L -H'
        + ` "X-Papertrail-Token: ${PAPERTRAIL_TOKEN}" `
        + ` https://papertrailapp.com/api/v1/archives/${ymdh}/download`
    console.log (cmd)
    childproc.exec (cmd, (err, stdout, stderr) => {
        if (err) {
            console.log(`error: ${err.message}`);
            return;
        }
        if (stderr) {
            console.log(`stderr: ${stderr}`);
            return;
        }
        console.log(`stdout: ${stdout}`);
    })
    console.log ('getpart returning')
}

console.log ('processlog starting')
console.log (`papertrail token = ${PAPERTRAIL_TOKEN}`)
getpart (2021, 3, 20, 15)
console.log ('processlog finished')
