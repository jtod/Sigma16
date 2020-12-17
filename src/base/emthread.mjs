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

// Constant parameters must be identical to definitions in state.mjs

const EMCTLSIZE   = 8
const REGFILESIZE = 16
const CREGSIZE    = 16
const MEMSIZE     = 65536
const StateSize =  2 * (EMCTLSIZE + REGFILESIZE + CREGSIZE + MEMSIZE)

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
            let msg = {code: 202, payload: emtCount}
            this.postMessage (msg)
        case 104: // Increment sysStateArr[23]
//            console.log (`emt 104`)
//            console.log (`emt 104 before ++ ${sysstate[23]}`)
            sysstate[23]++
//            console.log (`emt 104 after ++ ${sysstate[23]}`)
            break
        case 105: //
            for (let i = 0; i < 64; i++) {
                let x = sysstate [i] // 8 is REGOFFSET, should be 8+i
                console.log (`emt R${i} = ${x}`)
            }
            break
        default:
            console.log (`emt received bad code ${e.data.code}`)
        }
        emtCount++
    }
})
