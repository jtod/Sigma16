// Sigma16: gui.mjs
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

// Emulator worker thread

// Local state

let emtCount = 0
let sysstate

// Communication protocol

this.addEventListener ("message", e => {
    if (e.data) {
        console.log (`ent code = ${e.data.code}`)
        switch (e.data.code) {
        case 100:
            console.log ("emt initializing")
            sysstate = e.data.payload
            break
        case 101:
            console.log (`emt rec 101, n=${emtCount} val=<${e.data.payload}>`)
            break
        case 102:
            console.log (`emt rec 102, n=${emtCount} val=<${e.data.payload}>`)
            let msg = {code: 200, payload: emtCount}
            this.postMessage (msg)
        case 104: // Request to print sysStateArr[23] and increment it
            console.log (`emt 104`)
            console.log (`emt 104 before ++ ${sysstate[23]}`)
            sysstate[23]++
            console.log (`emt 104 after ++ ${sysstate[23]}`)
            break
        default:
            console.log (`emt received bad code ${e.data.code}`)
        }
        emtCount++
    }
})

// old deprecated

/*
let shared;
let index;

const updateAndPing = () => {
    ++shared[index];
    index = (index + 1) % shared.length;
    this.postMessage({type: "ping"});
}

this.addEventListener("message", e => {
    if (e.data) {
        switch (e.data.type) {
            case "init":
                shared = e.data.sharedArray;
                index = 0;
                updateAndPing();
                break;
            case "pong":
                updateAndPing();
                break;
        }
    }
});
*/

/*
let workerCounter = 0;

onmessage = function (e) {
    console.log ("worker received a message");
    let result = e.data;
    console.log (`I am the worker, this is what I received: /${result}/`);
    postMessage (workerCounter);
    console.log (`I am the worker, replying with ${workerCounter}`);
    workerCounter++;
    
}
*/

// basic-SharedArrayBuffer-worker.js
