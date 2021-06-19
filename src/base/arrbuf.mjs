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
// Memory map of the emulator state array
//-------------------------------------------------------------------------------

// There is an emulator state object which is accessible as gst.es.

// Most of the emulator's variables are stored in a system state
// vector, which is an element of gst.es.  The system state vector
// contains several arrays (breakpoint, registers, memory) as well as
// individual variables (instruction count, ir fields, etc.).

// The system state vector is a flat array buffer, so it requires
// calculating numerical indices to access the individual values,
// which is more complicated than just accessing them by name in a
// JavaScript object.  However, the system state vector has several
// advantages: it can be stored in a shared array buffer which is
// accessible from both the main gui thread and a worker thread, and
// it is accessible to both the JavaScript emulator and the Web
// Assembly emulator.

// The system state vector consists of several sections.  The memory
// section comes last so its size can be adjusted without changing the
// offsets of any of the sections.

//   SCB   System control block   32 and 64
//   BP    Breakpoint             32
//   REG   Registers              32
//   MEM   Memory                 16

// Sizes of state vector sections are specified in W64 (64-bit words),
// ensuring that each section begins at an aligned location.  The
// section sizes are defined as the preferred logical number of
// elements divided by the element size.  Notation: W8 = byte, W16 =
// word, W32 = long word, W64 = extended word.

export const SCBsize   = 1000   / 2  // emulator variables
export const BPsize    = 1000   / 2  // abstract syntax tree
export const RegSize   = 32     / 2  // 16 general and 16 system registers
export const MemSize   = 65536  / 4  // each location is 16 bits

// Size of the shared memory (words and bytes)
// export const EmStateSizeWord  =  EmSCB32size + EmRegBlockSize + EmMemSize
// export const EmStateSizeByte  =  2 * EmStateSizeWord

// Offsets of state vector sections

export const SCBoffset64  = 0
export const BPoffset64   = SCBoffset64 + SCBsize
export const RegOffset64  = BPoffset64 + BPsize
export const MemOffset64  = RegOffset64 + RegSize

export const SCBoffset32  = 2 * SCBoffset64
export const BPoffset32   = 2 * BPoffset64
export const RegOffset32  = 2 * RegOffset64
export const MemOffset32  = 2 * MemOffset64

export const SCBoffset16  = 2 * SCBoffset32
export const BPoffset16   = 2 * BPoffset32
export const RegOffset16  = 2 * RegOffset32
export const MemOffset16  = 2 * MemOffset32

//-------------------------------------------------------------------------------
// Access functions
//-------------------------------------------------------------------------------

// Elements of each section may be accessed using any word size.  To
// access an element of size k in JavaScript, the entire state vector
// is treated as an array of values of size k.  Thus x [i] with word
// size k in Sec is accessed as vec32 [i + SectionOffset/k], where k
// is the offset (in 32-bit words) of the section that contains this
// element.

export function read16  (es, a, k)     { return limit16 (es.vec16 [a + k]) }
export function write16 (es, a, k, x)  { es.vec16 [a+k] = limit16(x) }
export function read32  (es, a, k)     { return limit32 (es.vec32 [a + k]) }
export function write32 (es, a, k, x)  { es.vec32 [a+k] = limit32(x) }
export function read64  (es, a, k)     { return es.vec64 [a + k] }
export function write64 (es, a, k, x)  { es.vec64 [a+k] = x }

export function readReg16 (es, r)      { return read16 (es, r, RegOffset16) }
export function writedReg16 (es, r, x) { write16 (es, r, RegOffset16) }
export function readReg32 (es, r)      { return read32 (es, r, RegOffset32) }
export function writedReg32 (es, r, x) { write32 (es, r, RegOffset32) }

export function readMem16 (es, a)      { return read16 (es, a, MemOffset16) }
export function writedMem16 (es, r, x) { write16 (es, r, RegOffset16) }
export function readMem32 (es, a)      { return read32 (es, a, MemOffset32) }
export function writedMem32 (es, r, x) { write32 (es, r, RegOffset32) }

export function writeVec32 (es, a, x) {
    com.mode.devlog (`*** writeVec32 [${a}] := ${x}`)
    es.vec32 [a] = x
}

export function readVec32 (es, a) {
    let x = es.vec64 [a]
    com.mode.devlog (`*** readVec32 [${a}] => ${x}`)
    return x
}

// In Web Assembly, the index for accessing an element is the byte
// index.  Thus read16 i in wa must push 2*i and then fetch.

//-------------------------------------------------------------------------------
// System control block
//-------------------------------------------------------------------------------

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
    let i = EmSCB32Offset + code
    es.shm [i] = x
}

export function readSCB (es, code) {
    let i = EmSCB32Offset + code
    let x = es.shm [i]
    com.mode.devlog (`st.readSCB code=${code} i=${i} x=${x}`)
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
    let i = EmSCB32Offset + code
    let x = es.shm[i]
    x++
    es.shm[i] = x
    return x
}

