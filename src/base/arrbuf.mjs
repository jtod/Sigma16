// Sigma16: arrbuf.mjs
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

// arrbuf.mjs defines the array buffer which holds the architecture state

import * as com from './common.mjs'
import * as arch from './architecture.mjs'

//-------------------------------------------------------------------------------
// Array buffer
//-------------------------------------------------------------------------------

// Local state for main gui thread
// export let foo = 300 // dummy state belonging to main thread
// export let foobar = 0

// Constant parameters

// Sizes of memory blocks (words)
export const EmSCBsize        =  32
export const EmRegFileSize    =  16
export const EmSysRegSize     =  16
export const EmRegBlockSize   =  EmRegFileSize + EmSysRegSize
export const EmMemSize        =  65536

// Size of the shared memory (words and bytes)
export const EmStateSizeWord  =  EmSCBsize + EmRegBlockSize + EmMemSize
export const EmStateSizeByte  =  2 * EmStateSizeWord

// Offsets of memory blocks (words)
export const EmSCBOffset      =  0
export const EmRegBlockOffset =  EmSCBsize
export const EmMemOffset      =  EmRegBlockOffset + EmRegBlockSize

// Codes for system control block

// The system control block is stored in shared memory and contains
// information that pertains to the entire system, including all
// running emulators.  Any information that is specific to a
// particular emulator (either the main gui thread or a worker thread)
// is kept in the EmulatorState belonging to that thread.

//export const SCB_nInstrExecuted    =   5  // count instructions executed

// SCB flag indices
export const SCB_nInstrExecuted    =   0  // count instructions executed
export const SCB_status            =   4  // condition of the entire system
export const SCB_cur_instr_addr    =   5  // address of instruction executing now
export const SCB_next_instr_addr   =   6  // address of next instruction
export const SCB_emwt_run_mode     =   7
export const SCB_emwt_trap         =   8
export const SCB_pause_request     =   9  // pause request is pending

// SCB_status specifies the condition of the entire system
export const SCB_reset             = 0 // after initialization or Reset command
export const SCB_ready             = 1 // after boot
export const SCB_running_gui       = 2 // running in main gui thread
export const SCB_running_emwt      = 3 // running in emulator worker thread
export const SCB_paused            = 4 // after Pause command
export const SCB_break             = 5 // after Pause command
export const SCB_halted            = 6 // after trap 0
export const SCB_blocked           = 7 // during blocking read trap
export const SCB_relinquish        = 8 // emwt relinquished control temporarily

export function showSCBstatus (es) {
    switch (readSCB (es, SCB_status)) {
    case 0: return "Reset"
    case 1: return "Ready"
    case 2: return "Running"  // run in gui
    case 3: return "Running"  // run in emwt
    case 4: return "Paused"
    case 5: return "Break"
    case 6: return "Halted"
    case 7: return "Blocked"
    case 8: return "Relinquish"  // relinquish
    default: ""
    }
}

export function resetSCB (es) {
    writeVec32 (es, 0, 0) // use sym const for address 0

    writeSCB (es, SCB_status, SCB_reset)
    writeSCB (es, SCB_nInstrExecuted, 0)
    writeSCB (es, SCB_cur_instr_addr, 0)
    writeSCB (es, SCB_next_instr_addr, 0)
    writeSCB (es, SCB_emwt_run_mode, 0)
    writeSCB (es, SCB_emwt_trap, 0)
    writeSCB (es, SCB_pause_request, 0)
}

export function writeSCB (es, code, x) {
    let i = EmSCBOffset + code
    es.shm [i] = x
}

export function readSCB (es, code) {
    let i = EmSCBOffset + code
    let x = es.shm [i]
    com.mode.devlog (`st.readSCB code=${code} i=${i} x=${x}`)
    return x
}

export function writeVec32 (es, a, x) {
    com.mode.devlog (`*** writeVec32 [${a}] := ${x}`)
    es.vec32 [a] = x
}

export function readVec32 (es, a) {
    let x = es.vec64 [a]
    com.mode.devlog (`*** readVec32 [${a}] => ${x}`)
    return x
}

// Instruction count

export function clearInstrCount (es) {
    es.vec32[0] = 0
}

export function readInstrCount (es) {
    return es.vec32[0]
}

export function incrInstrCount (es) {
    es.vec32[0] = es.vec32[0] + 1
}

// If an instruction has been relinquished, the instruction count was
// incremented already, so the reexecution of the instruction would
// cause it to be counted twice.

export function decrInstrCount (es) {
    es.vec32[0] = es.vec32[0] - 1
}

export function incrSCB (es, code) {
    let i = EmSCBOffset + code
    let x = es.shm[i]
    x++
    es.shm[i] = x
    return x
}
