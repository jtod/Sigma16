// Sigma16: architecture.mjs
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

//----------------------------------------------------------------------
// architecture.mjs defines global constants and tables specifying
// formats, opcodes, mnemonics, and flag bits
// ---------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';

//----------------------------------------------------------------------
// Bit indexing
//----------------------------------------------------------------------

// There are two conventions for indexing bits in a word that contains
// k bits. Then
//   - Little end (LE): the most significant (leftmost) bit has index k,
//     and the least significant (rightmost bit) has index 0.
//   - Big end (BE): the most significant (leftmost) bit has index 0,
//     and the least significant (rightmost bit) has index k.

// Functions are defined for accessing bits in a word using either
// Little End (LE) or Big End (BE) notation.  For k-bit words:
//   Bit i BE = bit (k-i) LE
//   Bit i LE = bit (k-i) BE

// Earlier versions of Sigma16 (prior to 3.4) used Big End bit
// indexing.  Version 3.4 switches to Little End bit indexing because
// this alows a more elegant extension to 32-bit architecture.  In
// particular, a function to access bit i needs to know the wordsize k
// if Big End indexing is used, but not with Little End indexing.

// Get bit i from k-bit word w

export function getBitInWordLE (w,i)   { return (w >>> i)     & 0x0001 }
export function getBitInWordBE (k,w,i) { return (w >>> (k-i)) & 0x0001 }

// Put bit b into word x of size k in bit position i

export function putBitInWordLE (k,x,i,b) {
    return b==0 ? x & maskToClearBitLE(i)   : x | maskToSetBitLE(i)
}
export function putBitInWordBE (k,x,i,b) {
    return b==0 ? x & maskToClearBitBE(k,i) : x | maskToSetBitBE(k,i)
}

// Generate mask to clear/set bit i in a k-bit word

export function maskToClearBitLE (i)   { return ~(1<<i)      & 0xffff }
export function maskToSetBitLE   (i)   { return (1 << i)     & 0xffff }
export function maskToClearBitBE (k,i) { return ~(1<<(k-i))  & 0xffff }
export function maskToSetBitBE   (k,i) { return (1 << (k-i)) & 0xffff }

// Access bit i in register r with k-bit words

export function getBitInRegLE (r,i) {
    return (r.get() >>> i) & 0x0001
}
export function clearBitInRegLE (r,i) {
    r.put (r.get() & maskToClearBitLE(i))
}
export function setBitInRegLE (r,i) {
    r.put (r.get() | maskToSetBitLE(i))
}
export function getBitInRegBE (k,r,i) {
    return (r.get() >>> (k-i)) & 0x0001
}
export function clearBitInRegBE (k,r,i) {
    r.put (r.get() & maskToClearBitBE(k,i))
}
export function setBitInRegBE (k,r,i) {
    r.put (r.get() | maskToSetBitBE(k,i))
}

// Return Boolean from bit i in word x

export function extractBoolLE (x,i) {
    return getBitInWordLE (x,i) === 1
}

//----------------------------------------------------------------------
// Architecture constants
//----------------------------------------------------------------------

// Should make memSize adjustable in settings, with default = 65536

export const memSize = 65536; // number of memory locations = 2^16

// Sigma16 has a standard 16-bit architecture S16, and an extended
// architecture S32 that has 32-bit registers and addresses

export const S16 = Symbol ('S16')
export const S32 = Symbol ('S32')

// Instruction formats

export const iRRR   = Symbol ("RRR");
export const iRX    = Symbol ("RX");
export const iEXP   = Symbol ("EXP");

// Return the size of an instruction given its format

export function formatSize (ifmt) {
    return ifmt==iRRR  ? 1
        : (ifmt==iRX || ifmt==iEXP) ? 2
        : 0
}

//----------------------------------------------------------------------
// Assembly language statement formats
//----------------------------------------------------------------------

// Statement formats include directives as well as syntax for
// instructions.  R is a general register, C is a system control
// register, X is an index-isplacement address, k is a constant.  The
// statement formats allow "don't care" fields to be omitted, and
// fields to be used for either register numbers or constants.

export const iData    = Symbol ("data")
export const iDir     = Symbol ("iDir")
export const iEmpty   = Symbol ("iEmpty")

// Assembly language statement operand formats
export const a0       = Symbol ("");         // resume
export const aRR      = Symbol ("RR");       // cmp      R1,R2
export const aRRR     = Symbol ("RRR");      // add      R1,R2,R3
export const aRC      = Symbol ("RC");       // putctl   R1,status
export const aK    = Symbol ("K");     // brf      loop
export const aX       = Symbol ("X");        // jump     loop[R0]
export const aRX      = Symbol ("RX");       // load     R1,xyz[R2]
export const aRRX     = Symbol ("RRX");      // save     R4,R7,5[R13]
export const aRK   = Symbol ("RK");       // brfnz    R1,xyz
export const akX      = Symbol ("kX");       // jumpc0   3,next[R0]
export const aRRk     = Symbol ("RRk");      // invb     R1,R2,7
export const aRkk     = Symbol ("Rkk"); // field R1,3,12  ?? should be RRkk
export const aRkK  = Symbol ("RkK");      // brfc0  R2,4,230
export const aRkkkk   = Symbol ("Rkkkk");    // logicb   R1,3,8,2,xor
export const aRkRkk   = Symbol ("RkRkk");    // logicbcc R5,4,R9,3,and
export const aRkkk    = Symbol ("Rkkk");     // xorb     R1,3,8,2
export const aRRRk    = Symbol ("RRRk");     // logicw   R1,R2,R3,xor
export const aRkkRk   = Symbol ("RkkRk");    // extract  R1,7,4,R2,12
export const aRRRkk   = Symbol ("RRRkk");    // inject   R1,R2,R3,5,7
export const aData    = Symbol ("data");     // data     34
export const aModule  = Symbol ("module");   // module
export const aImport  = Symbol ("import");   // import   Mod1,x
export const aExport  = Symbol ("export");   // export   fcn
export const aOrg     = Symbol ("org");      // org      arr+5
export const aEqu     = Symbol ("equ");      // equ      rcd+4
export const aBlock   = Symbol ("block");    // block    100

// export const aRRkkk   = Symbol ("RRk");      // extract  Rd,Rs,di,si,size

//----------------------------------------------------------------------
// Instruction mnemonics
//----------------------------------------------------------------------

// These arrays are indexed by an opcode to give the corresponding
// mnemonic

export const mnemonicRRR =
  ["add",      "sub",      "mul",       "div",        // 0-3
   "cmp",      "addc",     "muln",      "divn",       // 4-7
   "rrr1",     "rrr2",     "rrr3",      "rrr4",       // 8-11
   "trap",     "EXP3",     "EXP2",      "RX"]         // 12-15

export const mnemonicRX =
  ["lea",      "load",     "store",     "jump",       // 0-3
   "jumpc0",   "jumpc1",   "jal",       "jumpz",      // 4-7
   "jumpnz",   "brc0",     "brc1",      "testset",    // 8-b
   "leal",     "loadl",    "storel",    "noprx"]      // c-f

export const mnemonicEXP =
    ["brf",     "brb",      "brfi",     "brfc0",   // 00-03
     "brbc0",   "brfc1",    "brbc1",    "brfz",    // 04-07
     "brbz",    "brfnz",    "brbnz",    "save",    // 08-0b
     "restore", "push",     "pop",      "top",     // 0c-0f
     "shiftl",  "shiftr",   "logicw",   "logicb",  // 10-13
     "logiccc", "extract",  "extracti", "getctl",  // 14-17
     "putctl",  "resume"]                          // 18-19

//-------------------------------------
// Mnemonics for control registers
//-------------------------------------

// The getctl and putctl instructions contain a field indicating which
// control register to use. This record defines the names of those
// control registers (used in the assembly language) and the numeric
// index for the control register (used in the machine language).

export let ctlReg = new Map();

ctlReg.set ("status",   {ctlRegIndex:0});
ctlReg.set ("mask",     {ctlRegIndex:1});
ctlReg.set ("req",      {ctlRegIndex:2});
ctlReg.set ("rstat",    {ctlRegIndex:3});
ctlReg.set ("rpc",      {ctlRegIndex:4});
ctlReg.set ("vect",     {ctlRegIndex:5});
ctlReg.set ("psegBeg",  {ctlRegIndex:6});
ctlReg.set ("psegEnd",  {ctlRegIndex:7});
ctlReg.set ("dsegBeg",  {ctlRegIndex:8});
ctlReg.set ("dsegEnd",  {ctlRegIndex:9});

//----------------------------------------------------------------------
// Condition code
//----------------------------------------------------------------------

// The condition code is a word of individual Boolean flags giving the
// results of comparisons and other conditions.  R15 contains the
// condition code, except that R15 is used for an additional result
// for multiply and divide instructions.

// A word is defined for each condition code flag.  An instruction may
// 'or' several of these words together to produce the final condition
// code.  Bits are numbered from right to left, starting with 0.  Thus
// the least significant bit has index 0, and the most significant bit
// has index 15.

// Each flag in the condition code has a symbolic name used in the
// implementation, and a display name used in the "instruction decode"
// panel on the emulator GUI.  The usual relations < = > are used for
// integers (binary representation) , while L = G are used for natural
// numbers (two's complement representation).  The code display
// characters are sSCVv<L=G>

// index  val  code  display   type and relation
// ----------------------------------------------
// bit 0  0001  g      >        int >
// bit 1  0002  G      G        nat >
// bit 2  0004  E      =        nat,int =
// bit 3  0008  L      L        nat <
// bit 4  0010  l      <        int <
// bit 5  0020  v      v        int overflow
// bit 6  0040  V      V        int overflow
// bit 7  0080  C      C        bin carry out, carry in (addc)
// bit 8  0100  S      S        bin carry out, carry in (addc)
// bit 9  0200  s      s        bin carry out, carry in (addc)
// bit 10 0400  f      f        logicc function result

export const bit_ccg = 0   // 0001 > greater than integer (two's complement)
export const bit_ccG = 1   // 0002 G greater than natural (binary)
export const bit_ccE = 2   // 0004 = equal all types
export const bit_ccL = 3   // 0008 L less than natural (binary)

export const bit_ccl = 4   // 0010 < less than integer (two's complement)
export const bit_ccv = 5   // 0020 v overflow integer (two's complement)
export const bit_ccV = 6   // 0040 V overflow natural (binary)
export const bit_ccC = 7   // 0080 C carry propagation natural (binary)

export const bit_ccS = 8   // 0100 S stack overflow
export const bit_ccs = 9   // 0200 s stack underflow
export const bit_ccf = 10  // 0400 f logicc instruction function result

// Define a mask with 1 in specified bit position
export const ccg = maskToSetBitLE (bit_ccg)
export const ccG = maskToSetBitLE (bit_ccG)
export const ccE = maskToSetBitLE (bit_ccE)
export const ccL = maskToSetBitLE (bit_ccL)
export const ccl = maskToSetBitLE (bit_ccl)
export const ccv = maskToSetBitLE (bit_ccv)
export const ccV = maskToSetBitLE (bit_ccV)
export const ccC = maskToSetBitLE (bit_ccC)
export const ccS = maskToSetBitLE (bit_ccS)
export const ccs = maskToSetBitLE (bit_ccs)
export const ccf = maskToSetBitLE (bit_ccf)

// Return a string giving symbolic representation of the condition
// code; this is used in the instruction display

export function showCC (c) {
    com.mode.devlog (`showCC ${c}`);
    return (extractBoolLE (c,bit_ccs) ? 's' : '')
	+ (extractBoolLE (c,bit_ccS) ? 'S' : '')
	+ (extractBoolLE (c,bit_ccC) ? 'C' : '')
	+ (extractBoolLE (c,bit_ccV) ? 'V' : '')
	+ (extractBoolLE (c,bit_ccv) ? 'v' : '')
	+ (extractBoolLE (c,bit_ccl) ? '&lt;' : '')
	+ (extractBoolLE (c,bit_ccL) ? 'L' : '')
	+ (extractBoolLE (c,bit_ccE) ? '=' : '')
	+ (extractBoolLE (c,bit_ccG) ? 'G' : '')
	+ (extractBoolLE (c,bit_ccg) ? '>' : '')
	+ (extractBoolLE (c,bit_ccf) ? 'f' : '')
}



//----------------------------------------------------------------------
// Status register bits
//----------------------------------------------------------------------

// Define the bit index for each flag in the status register.  "Big
// endian" notation is used, where 0 indicates the most significant
// (leftmost) bit, and index 15 indicates the least significant
// (rightmost) bit.

// When the machine boots, the registers are initialized to 0.  The
// user state flag is defined so that userStateBit=0 indicates that
// the processor is in system (or supervisor) state.  The reason for
// this is that the machine should boot into a state that enables the
// operating system to initialize itself, so privileged instructions
// need to be executable.  Furthermore, interrupts are disabled when
// the machine boots, because interrupts are unsafe to execute until
// the interrupt vector has been initialized.

export const userStateBit     = 0;   // 0 = system state,  1 = user state
export const intEnableBit     = 1;   // 0 = disabled,      1 = enabled

//----------------------------------------------------------------------
// Interrupt request and mask bits
//----------------------------------------------------------------------

export const timerBit            = 0;   // timer has gone off
export const segFaultBit         = 1;   // access invalid virtual address
export const stackOverflowBit    = 2;   // invalid memory virtual address
export const stackUnderflowBit   = 3;   // invalid memory virtual address
export const userTrapBit         = 4;   // user trap
export const overflowBit         = 5;   // overflow occurred
export const zDivBit             = 6;   // division by 0

//----------------------------------------------------------------------
// Assembly language data definitions for control bits
//----------------------------------------------------------------------

// A systems program can use the following canonical data definitions
// to access the control bits.  These statements can be copied and
// pasted into an assembly language program (removing, of course, the
// // on each line).

// ; Define status register control bits
// userStateBit    data   $8000
// intEnableBit    data   $4000

// ; Define interrupt control bits
// timerBit        data   $8000   ; bit 0
// segFaultBit     data   $4000   ; bit 1
// stackFaultBit   data   $2000   ; bit 2
// userTrapBit     data   $1000   ; bit 3
// overflowBit     data   $0800   ; bit 4
// zDivBit         data   $0400   ; bit 5

//----------------------------------------------------------------------
// Assembly language statements
//----------------------------------------------------------------------

// The instruction set is defined by a map from mnemonic to statement
// specification. The assembler uses the map to generate the machine
// language for an assembly language statement. Each entry specifies
// the instruction format, the assembly language statement format, and
// the opcode, which isrepresented as a list of expanding opcodes.

export let statementSpec = new Map()
export const emptyOperation = {ifmt:iEmpty, afmt:a0, opcode:[]}

// Primary opcodes (in the op field) of 0-11 denote RRR instructions.

statementSpec.set("add",   {ifmt:iRRR, afmt:aRRR, opcode:[0]})
statementSpec.set("sub",   {ifmt:iRRR, afmt:aRRR, opcode:[1]})
statementSpec.set("mul",   {ifmt:iRRR, afmt:aRRR, opcode:[2]})
statementSpec.set("div",   {ifmt:iRRR, afmt:aRRR, opcode:[3]})
statementSpec.set("cmp",   {ifmt:iRRR, afmt:aRR,  opcode:[4]})
statementSpec.set("addc",  {ifmt:iRRR, afmt:aRRR, opcode:[5]})
statementSpec.set("muln",  {ifmt:iRRR, afmt:aRRR, opcode:[6]})
statementSpec.set("divn",  {ifmt:iRRR, afmt:aRRR, opcode:[7]})
statementSpec.set("rrr1",  {ifmt:iRRR, afmt:aRRR, opcode:[8]})
statementSpec.set("rrr2",  {ifmt:iRRR, afmt:aRRR, opcode:[9]})
statementSpec.set("rrr3",  {ifmt:iRRR, afmt:aRRR, opcode:[10]})
statementSpec.set("rrr4",  {ifmt:iRRR, afmt:aRRR, opcode:[11]})
statementSpec.set("trap",  {ifmt:iRRR, afmt:aRRR, opcode:[12]})

// statementSpec.set("push",  {ifmt:iRRR, afmt:aRRR, opcode:[8]})
// statementSpec.set("pop",   {ifmt:iRRR, afmt:aRRR, opcode:[9]})
// statementSpec.set("top",   {ifmt:iRRR, afmt:aRRR, opcode:[10]})

// The following primary opcodes do not indicate RRR instructions:
//   13: escape to EXP3
//   14: escape to EXP
//   15: escape to RX

// RX instructions have primary opcode f and secondary opcode in b field

statementSpec.set("lea",    {ifmt:iRX,   afmt:aRX, opcode:[15,0]})
statementSpec.set("load",   {ifmt:iRX,   afmt:aRX, opcode:[15,1]})
statementSpec.set("store",  {ifmt:iRX,   afmt:aRX, opcode:[15,2]})
statementSpec.set("jump",   {ifmt:iRX,   afmt:aX,  opcode:[15,3]})
statementSpec.set("jumpc0", {ifmt:iRX,   afmt:akX, opcode:[15,4]})
statementSpec.set("jumpc1", {ifmt:iRX,   afmt:akX, opcode:[15,5]})
statementSpec.set("jal",    {ifmt:iRX,   afmt:aRX, opcode:[15,6]})
statementSpec.set("jumpz",  {ifmt:iRX,   afmt:aRX, opcode:[15,7]})
statementSpec.set("jumpnz", {ifmt:iRX,   afmt:aRX, opcode:[15,8]})
statementSpec.set("tstset", {ifmt:iRX,   afmt:aRX, opcode:[15,11]})

// EXP1 instructions are represented in 1 word, with primary opcode e
// and an 8-bit secondary opcode in the ab field.  The secondary
// opcode ab is between 0 and 7.  (If 8 <= ab then the instruction is
// EXP format.)  [Considering abandoning EXP1; no harm in wasting a
// word by using EXP for the resume instruction.]
   
// EXP instructions are represented in 2 words, with primary opcode e
// and an 8-bit secondary opcode in the ab field, where ab >= 8.  (If
// 0 <= ab <8 then the instruction is EXP1 format.)

// brf  loop            16-bit offset
// brfc0 R5,7,loop      8-bit offset
// brfz  R3,loop        12-bit offset

statementSpec.set("brf",    {ifmt:iEXP,  afmt:aK,    opcode:[14,0]})
statementSpec.set("brb",    {ifmt:iEXP,  afmt:aK,    opcode:[14,1]})
statementSpec.set("brfc0",  {ifmt:iEXP,  afmt:aRkK,  opcode:[14,2]})
statementSpec.set("brbc0",  {ifmt:iEXP,  afmt:aRkK,  opcode:[14,3]})
statementSpec.set("brfc1",  {ifmt:iEXP,  afmt:aRkK,  opcode:[14,4]})
statementSpec.set("brbc1",  {ifmt:iEXP,  afmt:aRkK,  opcode:[14,5]})
statementSpec.set("brfz",   {ifmt:iEXP,  afmt:aRK,   opcode:[14,6]})
statementSpec.set("brbz",   {ifmt:iEXP,  afmt:aRK,   opcode:[14,7]})
statementSpec.set("brfnz",  {ifmt:iEXP,  afmt:aRK,   opcode:[14,8]})
statementSpec.set("brbnz",  {ifmt:iEXP,  afmt:aRK,   opcode:[14,9]})
statementSpec.set("dsptch", {ifmt:iEXP,  afmt:aRkK,  opcode:[14,10]})
statementSpec.set("save",   {ifmt:iEXP,  afmt:aRRX,     opcode:[14,11]})
statementSpec.set("restor", {ifmt:iEXP,  afmt:aRRX,     opcode:[14,12]})
statementSpec.set("push",   {ifmt:iEXP,  afmt:aRRR,     opcode:[14,13]})
statementSpec.set("pop",    {ifmt:iEXP,  afmt:aRRR,     opcode:[14,14]})
statementSpec.set("top",    {ifmt:iEXP,  afmt:aRRR,     opcode:[14,15]})
statementSpec.set("shiftl", {ifmt:iEXP,  afmt:aRRk,     opcode:[14,16]})
statementSpec.set("shiftr", {ifmt:iEXP,  afmt:aRRk,     opcode:[14,17]})
statementSpec.set("logicw", {ifmt:iEXP,  afmt:aRRRk,    opcode:[14,18]})
statementSpec.set("logicb", {ifmt:iEXP,  afmt:aRkkkk,   opcode:[14,19]})
statementSpec.set("logicc", {ifmt:iEXP,  afmt:aRkRkk,   opcode:[14,20]})
statementSpec.set("extrc",  {ifmt:iEXP,  afmt:aRkkRk,   opcode:[14,21]})
statementSpec.set("extrci", {ifmt:iEXP,  afmt:aRkkRk,   opcode:[14,22]})
statementSpec.set("getctl", {ifmt:iEXP,  afmt:aRC,      opcode:[14,23]})
statementSpec.set("putctl", {ifmt:iEXP,  afmt:aRC,      opcode:[14,24]})
statementSpec.set("resume", {ifmt:iEXP,  afmt:a0,       opcode:[14,25]})

// Assembler directives

statementSpec.set("data",   {ifmt:iData, afmt:aData,   opcode:[]})
statementSpec.set("module", {ifmt:iDir,  afmt:aModule, opcode:[]})
statementSpec.set("import", {ifmt:iDir,  afmt:aImport, opcode:[]})
statementSpec.set("export", {ifmt:iDir,  afmt:aExport, opcode:[]})
statementSpec.set("org",    {ifmt:iDir,  afmt:aOrg,    opcode:[]})
statementSpec.set("equ",    {ifmt:iDir,  afmt:aEqu,    opcode:[]})
statementSpec.set("block",  {ifmt:iDir,  afmt:aBlock,  opcode:[]})

// Possible additional instructions...
// statementSpec.set("execute",  {ifmt:iEXP, afmt:aRR,  opcode:[14,12]});

// -------------------------------------
// Pseudoinstructions
// -------------------------------------

// JX is a pseudoinstruction format: an assembly language statement
// format which omits the d field, but the machine language format is
// RX, where R0 is used for the d field.  For example, jump loop[R5]
// doesn't require d field in assembly language, but the machine
// language uses d=R0.

// Pseudoinstructions that generate jumpc0 (secondary opcode = 4)

statementSpec.set("jumple",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccg],   pseudo:true})
statementSpec.set("jumpne",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccE],   pseudo:true})
statementSpec.set("jumpge",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccl],   pseudo:true})
statementSpec.set("jumpnv",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccv],   pseudo:true})
statementSpec.set("jumpnco",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccC],   pseudo:true})

// Pseudoinstructions that generate jumpc1 (secondary opcode = 5)

statementSpec.set("jumplt",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccl],
                   pseudo:true})
statementSpec.set("jumpeq",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccE],
                   pseudo:true})
statementSpec.set("jumpgt",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccg],
                   pseudo:true})
statementSpec.set("jumpv",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccv],
                   pseudo:true})
statementSpec.set("jumpco",
                  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccC],
                   pseudo:true})

// Mnemonics for logic instructions

statementSpec.set("invw",    {ifmt:iEXP, afmt:aRR,    opcode:[14,18,12],
                              pseudo:true});
statementSpec.set("andw",    {ifmt:iEXP, afmt:aRRR,    opcode:[14,18,1],
                              pseudo:true});
statementSpec.set("orw",     {ifmt:iEXP, afmt:aRRR,    opcode:[14,18,7],
                              pseudo:true});
statementSpec.set("xorw",    {ifmt:iEXP, afmt:aRRR, opcode:[14,18,6],
                              pseudo:true});

// Mnemonics for logicb instructions

statementSpec.set("invb",    {ifmt:iEXP, afmt:aRkk, opcode:[14,8,12],
                              pseudo:true});
statementSpec.set("andb",    {ifmt:iEXP, afmt:aRkkk, opcode:[14,8,1],
                              pseudo:true});
statementSpec.set("orb",     {ifmt:iEXP, afmt:aRkkk, opcode:[14,8,7],
                              pseudo:true});
statementSpec.set("xorb",    {ifmt:iEXP, afmt:aRkkk, opcode:[14,8,6],
                              pseudo:true});

// Mnemonic for bit field

statementSpec.set("field",   {ifmt:iEXP, afmt:aRkk,  opcode:[14,8],
                              pseudo:true});

export const clearIntEnable = maskToClearBitBE (intEnableBit);
export const setSystemState = maskToClearBitBE (userStateBit);

// deprecated or in progress...
// execute, dispatch
// export const mnemonicEXP3 = ["shiftll", "shiftrl"]
// export const iEXP3  = Symbol ("EXP3");
//        : ifmt==iEXP3 ? 3
// EXP3 instructions are represented in 3 words, with primary opcode d
// and an 8-bit secondary opcode in the ab field.

// statementSpec.set("shiftll", {ifmt:iEXP3, afmt:aRRk, opcode:[13,0]})
// statementSpec.set("shiftrl", {ifmt:iEXP3, afmt:aRRk, opcode:[13,1]})

// The following instructions are in the S32 extended architecture,
// and are not currently implemented
// statementSpec.set("addl",     {ifmt:iEXP,  afmt:aRRR,  opcode:[14,17]})
// statementSpec.set("subl",     {ifmt:iEXP,  afmt:aRRR,  opcode:[14,18]})
// statementSpec.set("mull",     {ifmt:iEXP,  afmt:aRRR,  opcode:[14,19]})
// statementSpec.set("divl",     {ifmt:iEXP,  afmt:aRRR,  opcode:[14,20]})
// statementSpec.set("cmpl",     {ifmt:iEXP,  afmt:aRR,   opcode:[14,21]})
