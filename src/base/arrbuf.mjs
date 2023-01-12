// Sigma16: arrbuf.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3 or later
// See Sigma16/README, LICENSE, and https://jtod.github.io/home/Sigma16

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

// arrbuf.mjs defines the system state vector, comprising views at
// word sizes 16, 32, and 64 of the array buffer (which may be shared
// if the platform supports this).

import * as com from './common.mjs'
import * as arith from './arithmetic.mjs'
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

// Notation: W8 = byte, W16 = word, W32 = full word, W64 = extended
// word.

// Sizes of state vector sections are specified in W64 (64-bit words),
// ensuring that each section begins at an aligned location regardless
// of its element word size.  The section sizes are defined as the
// preferred logical number of elements divided by the number of
// elements per extended word.  Each register is allocated a full
// word, to allow for both S16 and S32 architectures.  Each memory
// location is a short word (16 bits), although S32 memory operations
// can load or store a full word.

export const SCBsize   =   512 / 2   // emulator variables
export const BPsize    =   512 / 2   // abstract syntax tree
export const RegSize   =    32 / 2   // 16 general and 16 system registers
export const MemSize   = 65536 / 4   // each location is 16 bits
export const Mem32Size = MemSize * 4 // make this a changeable option >= MemSize

// The array buffers are allocated with a specified size in bytes
export const StateVecSizeBytes = 8 * (SCBsize + BPsize + RegSize + Mem32Size)

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
// General access functions
//-------------------------------------------------------------------------------


// Each section consists of elemens of a specific word size, except
// the SCB which contains some 64-bit elements but is mostly 32-bit
// elements.  To access an element of size k bits in JavaScript, the
// entire state vector is treated as an array of k-bit words.  Thus
// x[i] with word size k in Section is accessed as vec_k [i +
// SectionOffset_k].

// Raw access functions read or write a word of size k in the state
// vector.  These are read16, write16, read32, write32, read64,
// wite64.

// A section may contain elements of different word sizes.  The SCB
// contains mostly 32-bit elements, but the count of instructions
// executed is 64 bits.

export function read16  (es, a, k)    { return arith.limit16 (es.vec16 [a + k]) }
export function write16 (es, a, k, x) { es.vec16 [a+k] = arith.limit16(x) }
export function read32  (es, a, k)    { return arith.limit32 (es.vec32 [a + k]) }
export function write32 (es, a, k, x) { es.vec32 [a+k] = arith.limit32(x) }
export function read64  (es, a, k)    { return es.vec64 [a + k] }
export function write64 (es, a, k, x) { es.vec64 [a+k] = x }

// In Web Assembly, the index for accessing an element is the byte
// index.  Thus read16 i in wa must push 2*i and then fetch.

//-------------------------------------------------------------------------------
// System control block
//-------------------------------------------------------------------------------

// The system control block is stored in (shared) memory and contains
// information that pertains to the entire system, including all
// running emulators.  Any information that is specific to a
// particular emulator (either the main gui thread or a worker thread)
// is kept in the EmulatorState belonging to that thread.  Each
// element of the block has an index which is used relative to the
// element's word size

// Indices for 64-bit elements
export const SCB_nInstrExecuted   =   0  // count instructions executed

// Indices for 32-bit elements, which follow the 64-bit elements
export const SCB_status           =   8  // condition of the entire system
export const SCB_cur_instr_addr   =   9  // address of instruction executing now
export const SCB_next_instr_addr  =  10  // address of next instruction
export const SCB_emwt_run_mode    =  11
export const SCB_emwt_trap        =  12
export const SCB_pause_request    =  13  // pause request is pending

// SCB access functions

export function writeSCB (es, elt, x) { write32 (es, elt, SCBoffset32, x) }
export function readSCB (es, elt) { return read32 (es, elt, SCBoffset32) }

// SCB_status codes specify the condition of the processor

export const SCB_reset             = 0 // after initialization or Reset command
export const SCB_ready             = 1 // after boot
export const SCB_running_gui       = 2 // running in main gui thread
export const SCB_running_emwt      = 3 // running in emulator worker thread
export const SCB_paused            = 4 // after Pause command
export const SCB_break             = 5 // after Pause command
export const SCB_halted            = 6 // after trap 0
export const SCB_blocked           = 7 // during blocking read trap
export const SCB_relinquish        = 8 // emwt relinquished control temporarily

// Clear the SCB, putting the system into initial state

export function resetSCB (es) {
    clearInstrCount (es)
    writeSCB (es, SCB_status, SCB_reset)
    writeSCB (es, SCB_nInstrExecuted, 0)
    writeSCB (es, SCB_cur_instr_addr, 0)
    writeSCB (es, SCB_next_instr_addr, 0)
    writeSCB (es, SCB_emwt_run_mode, 0)
    writeSCB (es, SCB_emwt_trap, 0)
    writeSCB (es, SCB_pause_request, 0)
}

// Convert the numeric status to a descriptive string; this is shown
// in the processor display

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

// The instruction count is a 32 bit integer. It is cleared when a
// program starts and is incremented when an instruction is executed.
// Furthermore, decrInstrCount is needed because of interaction
// between the worker thread and the main thread.  If an instruction
// has been relinquished, the instruction count was incremented
// already, and reexecution of the instruction in the main thread
// causes it to be counted twice.  When this happens, the count is
// decremented.  (Non-urgent plan for the future: represent the
// instruction count using the full range of integer representations
// allowed by JavaScript, to avoid overflow for long running
// programs.)

function writeInstrCount (es, n)     { write32 (es, 0, SCBoffset32, n) }
export function readInstrCount (es)  { return read32 (es, 0, SCBoffset32) }
export function clearInstrCount (es) { writeInstrCount (es, 0) }
export function incrInstrCount (es) { writeInstrCount (es, readInstrCount(es) + 1) }
export function decrInstrCount (es) { writeInstrCount (es, readInstrCount(es) - 1) }

//-------------------------------------------------------------------------------
// Registers
//-------------------------------------------------------------------------------

// In S16, all registers contain 16 bits.  In S32 all registers
// contain 32 bits (apart from the 16-bit ir), and the 16-bit
// load/store instructions operate on the least signifcant half of a
// 32 bit register.
                                         
// Sigma16 uses a 32-bit representation of all registers to simplify
// the transition from S16 to S32 instructions.  The representation of
// registers needs to take into account both S16 and S32 instructions.
// Every register is stored in the state vector as a 32 bit word.
// Instructions that access 16-bit registers mus ensure that they put
// a valid 16-bit word, and similar for 32 bit registers.  The
// constraints may be checked by assert16 and assert32.

// Each byte should be accessed using only one word size, because
// JavaScript uses the byte endianness of the platform it's running
// on.  This affects the representation of registers and memory, as
// noted below.

// Therefore every register is accessed as a 32-bit element, for both
// the S16 and S32 instruction sets, even if it is being accessed
// using a 16-bit instruction.

// Layout of registers in array buffer:

//              ......R0.....   ......R1.....   ......R2.....   ......R3.....
//              x   x   x   x   x   x   x   x   x   x   x   x   x   x   x   x
// byte index   0   1   2   3   4   5   6   7   8   9   a   b   c   d   e   f
// W16 index    0       1       2       3       4       5       6       7
// W32 index    0               1               1               2


export function readReg16 (es, r) {
    return r===0 ? 0 : read16 (es, r << 1, RegOffset16) }

export function writeReg16 (es, r, x) {
    if (r!==0) write16 (es, r << 1, RegOffset16, x) }

export function readReg32 (es, r) {
    return r===0 ? 0 : read32 (es, r , RegOffset32) }

export function writeReg32 (es, r, x) {
    if (r!==0) write32 (es, r, RegOffset32, x) }

//-------------------------------------------------------------------------------
// Memory
//-------------------------------------------------------------------------------

// access Mem16 uses address of 16-bit word

export function readMem16 (es, a)      { return read16 (es, a, MemOffset16) }
export function writeMem16 (es, a, x) { write16 (es, a, MemOffset16, x) }

// access Mem32 uses big end representation: the value of the word at
// address a is mem[a] * 2^16 + mem[a+1].  The emulator accesses the
// two words independently in order to guarantee Sigma16 semantics,
// regardless of whether the host computer uses big end or little end
// representation.

export function readMem32 (es, a) {
    console.log (`readMem32 a=${a}...`)
    const b = a & 0xfffffffe
    const x = read16 (es, b, MemOffset16)
    const y = read16 (es, b+1, MemOffset16)
    const result = x << 16 | y
    console.log (`...readMem32 b=${b}`
                 + ` x = ${x} (${arith.wordToHex4(x)})`
                 + ` y = $(y} (${arith.wordToHex4(y)})`
                 + ` result = ${result} (${arith.wordToHex8(result)})`)
    return result
}

export function writeMem32 (es, a, x) {
    console.log (`writeMem32 a=${a} x=${arith.wordToHex8(x)}...`)
    const b = a & 0xfffffffe
    const y = x >>> 16
    const z = x & 0x0000ffff
    write16 (es, b, MemOffset16, y)
    write16 (es, b+1, MemOffset16, z)
    console.log (`...writeMem32 b=${b} x split into:`
                 + ` y = ${y} (${arith.wordToHex4(y)})`
                 + ` z = ${z} (${arith.wordToHex4(z)})`)
}

//-------------------------------------------------------------------------------
// Testing
//-------------------------------------------------------------------------------

export function testSysStateVec (es) {
    console.log (`%cTESTING: testSysStateVec starting in thread ${es.thread_host}`,
                 'color:red')
    console.log (`es.arch = ${es.arch.description}`)
    console.log (`es.emRunThread = ${es.emRunThread}`)
    let xs = ""
    let n = 3
    let x, y, z // testing results

    console.log (`MemOffset16=${MemOffset16}`)
    console.log (`MemOffset32=${MemOffset32}`)
    console.log (`MemOffset64=${MemOffset64}`)

    for (let i = 0; i < n; i++) es.vec16[i] = i
    for (let i = 0; i < n; i++) es.vec16[i] += 100
    for (let i = 0;  i < n; i++) xs += ` ${i}->${es.vec16[i]}`
    console.log (`thread host ${es.thread_host}: ${xs} finished`)

    console.log (`es.vec16[25] = ${es.vec16[25]}`)
    es.vec16[25] = 25
    es.vec16[25] = es.vec16[25] * 2
    console.log (`es.vec16[25] (expect 50) = ${es.vec16[25]}`)

    console.log (`es.vec32[45] = ${es.vec32[45]}`)
    es.vec32[45] = 85
    es.vec32[45] = es.vec32[45] * 2
    console.log (`es.vec32[45] (expect 170) = ${es.vec32[45]}`)


    console.log ('%caccess vec16', 'color:blue')
    es.vec16[7] = 423
    x = es.vec16[7]
    console.log (`x=${x} expect 423`)

    console.log ('%cwrite16 and read16', 'color:blue')
    x = read16 (es, 29, MemOffset16)
    console.log (`x=${x} init expect 0`)
    write16 (es, 29, MemOffset16, 83)
    y = read16 (es, 29, MemOffset16)
    console.log (`y=${y} expect 83`)
    write16 (es, 29, MemOffset16, 0)
    z = read16 (es, 29, MemOffset16)
    console.log (`z=${z} expect 0`)

    console.log ('%cwrite32 and read32', 'color:blue')
    x = read32 (es, 5, MemOffset32)
    console.log (`x=${x} init expect 0`)
    write32 (es, 5, MemOffset32, 109)
    y = read32 (es, 5, MemOffset32)
    console.log (`y=${y} expect 109`)
    write32 (es, 5, MemOffset32, 0)
    z = read32 (es, 5, MemOffset32)
    console.log (`z=${z} expect 0`)

    console.log ('%cwriteSCB and readSCB ', 'color:blue')
    x = readSCB (es, SCB_cur_instr_addr)
    console.log (`SCB_cur_instr_addr= ${SCB_cur_instr_addr} => ${x} init expect 0`)
    writeSCB (es, SCB_cur_instr_addr, 34)
    y = readSCB (es, SCB_cur_instr_addr)
    console.log (`SCB_cur_instr_addr=${y} expect 34`)
    writeSCB (es, SCB_cur_instr_addr, 0)
    z = readSCB (es, SCB_cur_instr_addr)
    console.log (`SCB_cur_instr_addr=${z} expect 0`)
    clearInstrCount (es)
    incrInstrCount (es)
    x = readInstrCount (es)
    console.log (`instrCount = ${x} expect 1`)

    console.log ('%cRegisters', 'color:blue')
    
    console.log ('%cwriteReg16 and readReg16', 'color:blue')
    x = readReg16 (es, 4)
    console.log (`R4 = ${x} expect 0`)
    writeReg16 (es, 4, 123)
    y = readReg16 (es, 4)
    console.log (`R4 = ${y} expect 123`)

    console.log ('%cwriteReg32 and readReg32 ', 'color:blue')
    x = readReg32 (es, 5)
    console.log (`R5 = ${x} expect 0`)
    writeReg32 (es, 5, 456)
    y = readReg32 (es, 5)
    console.log (`R5 = ${y} expect 456`)

    console.log ('%cR0 must remain 0', 'color:blue')
    x = readReg16 (es, 0)
    console.log (`R0 = ${x} expect 0`)
    writeReg16 (es, 0, 987)
    y = readReg16 (es, 0)
    console.log (`R0 = ${y} expect 0 (after writing R0=)`)

    console.log ('%cMemory', 'color:blue')

    console.log ('%cwrite/read Mem16', 'color:blue')
    writeMem16 (es, 3, 42)
    x = readMem16 (es, 3)
    console.log (`x=${x} expect 42`)

    console.log ('%cwrite/read Mem32', 'color:blue')
    writeMem32 (es, 6, 0xf0f000ff)
    x = readMem32 (es, 6)
    y = readMem32 (es, 7)
    console.log (`x=${arith.wordToHex8(x)} y=${arith.wordToHex8(y)}`
                 + ' expect f0f000ff both times')
    
    console.log ('%cwriteMem16 (odd address) / readMem32', 'color:blue')
    writeMem16 (es, 9, 34)
    x = readMem16 (es, 8)
    y = readMem16 (es, 9)
    z = readMem32 (es, 9)
    console.log (`x=${x} y=${y} z=${z} expect 0 34 34`)

    console.log ('%cwriteMem16 (even address) / readMem32', 'color:blue')
    writeMem16 (es, 12, 1)
    x = readMem16 (es, 12)
    y = readMem32 (es, 13)  // locations 6,7
    z = readMem32 (es, 12)
    console.log (`x=${x} y=${y} z=${z} expect 1 0 2^16`)

    console.log ('%cRegister snapshot', 'color:blue')
    console.log ('Expect R4 = 123, R5 = 456')
    for (let i = 0; i < 16; i++) {
        let x = readReg16 (es, i)
        console.log (`R${i} = ${x} ${arith.wordToHex4(x)} `)
    }

    console.log ('%cMemory snapshot', 'color:blue')
    console.log ('Expect m[3] = 42, m[6] = f0f0, m[7] =  00ff,')
    console.log ('Expect m[9] = 34, m[12] = 1')
    for (let i = 0; i < 16; i++) {
        let x = readMem16 (es, i)
        console.log (`mem[${i}] = ${x} ${arith.wordToHex4(x)} `)
    }
    
    console.log (`%cTESTING: testSysStateVec finished`, 'color:red')
}

