// Sigma16: architecture.mjs
// Copyright (C) 2020 John T. O'Donnell
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

//-----------------------------------------------------------------------------
// architecture.mjs defines global constants and tables specifying
// formats, opcodes, mnemonics, and flag bits
// -----------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';

//-----------------------------------------------------------------------------
// Instruction formats
//-----------------------------------------------------------------------------

export const iRRR     = Symbol ("RRR");
export const iRX      = Symbol ("RX");
export const iEXP1    = Symbol ("EXP1");
export const iEXP2    = Symbol ("EXP2");

export const iData    = Symbol ("data");
export const iDir     = Symbol ("iDir");
export const iEmpty   = Symbol ("iEmpty");

// Return the size of instruction in given format
export function formatSize (ifmt) {
    return (ifmt==iRRR || ifmt==iEXP1) ? 1
        : (ifmt==iRX || ifmt==iEXP2) ? 2
        : 0
}

//-----------------------------------------------------------------------------
// Assembly language statement formats
//-----------------------------------------------------------------------------

// Statement formats include directives as well as syntax for
// instructions.  R is a general register, C is a system control
// register, X is an index-isplacement address, k is a constant.  The
// statement formats allow "don't care" fields to be omitted, and
// fields to be used for either register numbers or constants.

export const a0       = Symbol ("no operand");

export const aRR      = Symbol ("RR - e.g. R1,R2");
export const aRRR     = Symbol ("RRR - e.g. R1,R2,R3");
export const aRC      = Symbol ("RC - e.g. R1,status");

export const aX       = Symbol ("JX - e.g. loop[R0]");
export const aRX      = Symbol ("RX - e.g. R1,xyz[R2]");
export const aRRX     = Symbol ("RRX - e.g. R1,R2,x[R5]");
export const akX      = Symbol ("KX - e.g. 3,next[R0]");

export const aRRk     = Symbol ("RRk - e.g. R1,R2,7");
export const aRRRk    = Symbol ("RRRk - e.g. R1,R2,R3,9");
export const aRkk     = Symbol ("Rkk - e.g. R1,3,12");
export const aRRkk    = Symbol ("RRkk - e.g. R1,R2,3,7");
export const aRRRkk   = Symbol ("RRRkk - e.g. R1,R2,R3,5,7");

export const aData    = Symbol ("data - e.g. 34 -17 $0b2a");
export const aModule  = Symbol ("module - no operand");
export const aImport  = Symbol ("import - e.g. Mod1,x");
export const aExport  = Symbol ("export - e.g. xyz");
export const aOrg     = Symbol ("org - e.g. $f000");
export const aEqu     = Symbol ("equ - e.g. 13");

export const aEmpty   = Symbol ("aEmpty");

//-----------------------------------------------------------------------------
// Instruction mnemonics
//-----------------------------------------------------------------------------

// These arrays can be indexed by opcode to give corresponding mnemonic

export const mnemonicRRR =
  ["add",      "sub",      "mul",      "div",       // 0-3
   "addc",     "cmp",      "nop",      "nop",       // 4-7
   "nop",      "nop",      "nop",      "nop",       // 8-b
   "nop",      "trap",     "EXP",      "RX"];       // c-f

export const mnemonicRX =
  ["lea",      "load",     "store",    "jump",      // 0-3
   "jal",      "jumpc0",   "jumpc1",   "jumpz",     // 4-7
   "jumpnz",   "testset",  "nop",      "nop",       // 8-b
   "nop",      "nop",      "nop",      "nop"];      // c-f

export const mnemonicEXP1 = ["rfi"];

export const mnemonicEXP2 =
  ["exp1",     "getctl",   "putctl",   "shiftl",    // 00-03
   "shiftr",   "logicb",   "logicw",   "extract",   // 04-07
   "extracti", "inject",   "injecti",  "push",      // 08-0b
   "pop",      "top",      "save",     "restore",   // 0c-0f
   "execute",  "dispatch", "nop",      "nop"];      // 10-13

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
ctlReg.set ("istat",    {ctlRegIndex:3});
ctlReg.set ("ipc",      {ctlRegIndex:4});
ctlReg.set ("vect",     {ctlRegIndex:5});
ctlReg.set ("psegBeg",  {ctlRegIndex:6});
ctlReg.set ("psegEnd",  {ctlRegIndex:7});
ctlReg.set ("dsegBeg",  {ctlRegIndex:8});
ctlReg.set ("dsegEnd",  {ctlRegIndex:9});

//------------------------------------------------------------------------------
// Condition codes
//------------------------------------------------------------------------------

// Bits are numbered from right to left, starting with 0.  Thus the
// least significant bit has index 0, and the most significant bit has
// index 15.

// Define a word for each condition that is representable in the
// condition code.  The arithmetic operations may or several of these
// together to produce the final condition code.

// These definitions give the bit index
export const bit_ccG = 0;   //    G   >          binary
export const bit_ccg = 1;   //    >   >          two's complement
export const bit_ccE = 2;   //    =   =          all types
export const bit_ccl = 3;   //    <   <          two's complement
export const bit_ccL = 4;   //    L   <          binary
export const bit_ccV = 5;   //    V   overflow   binary
export const bit_ccv = 6;   //    v   overflow   two's complement
export const bit_ccC = 7;   //    c   carry      binary

//-----------------------------------------------------------------------------
// Status register bits
//-----------------------------------------------------------------------------

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

//-----------------------------------------------------------------------------
// Interrupt request and mask bits
//-----------------------------------------------------------------------------

export const timerBit         = 0;   // timer has gone off
export const segFaultBit      = 1;   // access invalid virtual address
export const stackFaultBit    = 2;   // invalid memory virtual address
export const userTrapBit      = 3;   // user trap
export const overflowBit      = 4;   // overflow occurred
export const zDivBit          = 5;   // division by 0

//-----------------------------------------------------------------------------
// Assembly language data definitions for control bits
//-----------------------------------------------------------------------------

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

//-----------------------------------------------------------------------------
// Assembly language statements
//-----------------------------------------------------------------------------

// The instruction set is defined by a map from mnemonic to statement
// specification.  Each entry specifies the instruction format, the
// assembly language statement format, and the opcode (represented as
// a list of expanding opcodes).

export const emptyOperation = {ifmt:iEmpty, afmt:aEmpty, opcode:[]}

export let statementSpec = new Map();

// Primary opcodes (in the op field) of 0-13 denote RRR instructions.
// If op=14, escape to EXP1/EXP2 format, and if op=15 escape to RX.

statementSpec.set("add",      {ifmt:iRRR,  afmt:aRRR,    opcode:[0]});
statementSpec.set("sub",      {ifmt:iRRR,  afmt:aRRR,    opcode:[1]});
statementSpec.set("mul",      {ifmt:iRRR,  afmt:aRRR,    opcode:[2]});
statementSpec.set("div",      {ifmt:iRRR,  afmt:aRRR,    opcode:[3]});
statementSpec.set("addc",     {ifmt:iRRR,  afmt:aRRR,    opcode:[4]});
statementSpec.set("cmp",      {ifmt:iRRR,  afmt:aRR,     opcode:[5]});
statementSpec.set("trap",     {ifmt:iRRR,  afmt:aRRR,    opcode:[13]});

// RX instructions have primary opcode f and secondary opcode in b field
statementSpec.set("lea",      {ifmt:iRX,   afmt:aRX,     opcode:[15,0]});
statementSpec.set("load",     {ifmt:iRX,   afmt:aRX,     opcode:[15,1], pseudo:false});
statementSpec.set("store",    {ifmt:iRX,   afmt:aRX,     opcode:[15,2]});
statementSpec.set("jump",     {ifmt:iRX,   afmt:aX,      opcode:[15,3]});
statementSpec.set("jal",      {ifmt:iRX,   afmt:aRX,     opcode:[15,4]});
statementSpec.set("jumpc0",   {ifmt:iRX,   afmt:akX,     opcode:[15,5]});
statementSpec.set("jumpc1",   {ifmt:iRX,   afmt:akX,     opcode:[15,6]});
statementSpec.set("jumpz",    {ifmt:iRX,   afmt:aRX,     opcode:[15,7]});
statementSpec.set("jumpnz",   {ifmt:iRX,   afmt:aRX,     opcode:[15,8]});
statementSpec.set("testset",  {ifmt:iRX,   afmt:aRX,     opcode:[15,9]});

// EXP instructions have primary opcode e and 8-bit secondary opcode
// in ab field

// EXP1 - instruction is 1 word
statementSpec.set("resume",   {ifmt:iEXP1, afmt:a0,     opcode:[14,0]});

// EXP2 - instruction is 2 words
statementSpec.set("getctl",   {ifmt:iEXP2, afmt:aRC,     opcode:[14,1]});
statementSpec.set("putctl",   {ifmt:iEXP2, afmt:aRC,     opcode:[14,2]});
statementSpec.set("shift",    {ifmt:iEXP2, afmt:aRRR,    opcode:[14,3]});
statementSpec.set("shiftl",   {ifmt:iEXP2, afmt:aRRk,    opcode:[14,4]});
statementSpec.set("shiftr",   {ifmt:iEXP2, afmt:aRRk,    opcode:[14,5]});
statementSpec.set("extract",  {ifmt:iEXP2, afmt:aRRkk,   opcode:[14,6]});
statementSpec.set("extracti", {ifmt:iEXP2, afmt:aRRkk,   opcode:[14,7]});
statementSpec.set("inject",   {ifmt:iEXP2, afmt:aRRRkk,  opcode:[14,8]});
statementSpec.set("injecti",  {ifmt:iEXP2, afmt:aRRRkk,  opcode:[14,9]});
statementSpec.set("logicw",   {ifmt:iEXP2, afmt:aRRRk,   opcode:[14,22]});
statementSpec.set("logicb",   {ifmt:iEXP2, afmt:aRRRkk,  opcode:[14,23]});
statementSpec.set("save",     {ifmt:iEXP2, afmt:aRRX,    opcode:[14,8]});
statementSpec.set("restore",  {ifmt:iEXP2, afmt:aRRX,    opcode:[14,9]});
statementSpec.set("execute",  {ifmt:iEXP2, afmt:aRR,     opcode:[14,12]});
statementSpec.set("push",     {ifmt:iEXP2, afmt:aRRR,    opcode:[14,13]});
statementSpec.set("pop",      {ifmt:iEXP2, afmt:aRRR,    opcode:[14,14]});
statementSpec.set("top",      {ifmt:iEXP2, afmt:aRRR,    opcode:[14,15]});

// Assembler directives

statementSpec.set("data",     {ifmt:iData, afmt:aData,   opcode:[]});
statementSpec.set("module",   {ifmt:iDir,  afmt:aModule, opcode:[]}); 
statementSpec.set("import",   {ifmt:iDir,  afmt:aImport, opcode:[]});
statementSpec.set("export",   {ifmt:iDir,  afmt:aExport, opcode:[]});
statementSpec.set("org",      {ifmt:iDir,  afmt:aOrg,    opcode:[]});
statementSpec.set("equ",      {ifmt:iDir,  afmt:aEqu,    opcode:[]});

// -------------------------------------
// Pseudoinstructions
// -------------------------------------

// JX is a pseudoinstruction format: an assembly language statement
// format which omits the d field, but the machine language format is
// RX, where R0 is used for the d field.  For example, jump loop[R5]
// doesn't require d field in assembly language, but the machine
// language uses d=R0.

// Mnemonics for jumpc0 based on signed comparisons, overflow, carry

statementSpec.set("jumple",  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccg],
                              pseudo:true});
statementSpec.set("jumpne",  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccE],
                              pseudo:true});
statementSpec.set("jumpge",  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccl],
                              pseudo:true});
statementSpec.set("jumpnv",  {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccv],
                              pseudo:true});
statementSpec.set("jumpnvu", {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccV],
                              pseudo:true});
statementSpec.set("jumpnco", {ifmt:iRX,  afmt:aX,  opcode:[15,4,bit_ccC],
                              pseudo:true});

// Mnemonics for jumpc1 based on signed comparisons

statementSpec.set("jumplt",  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccl]});
statementSpec.set("jumpeq",  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccE]});
statementSpec.set("jumpgt",  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccg]});
statementSpec.set("jumpv",   {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccv]});
statementSpec.set("jumpvu",  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccV]});
statementSpec.set("jumpco",  {ifmt:iRX,  afmt:aX,  opcode:[15,5,bit_ccC]});

// Mnemonics for logic instructions

statementSpec.set("inv",     {ifmt:iEXP2, afmt:aRR,    opcode:[14,22,12],
                              pseudo:true});
statementSpec.set("and",     {ifmt:iEXP2, afmt:aRRR,    opcode:[14,22,1],
                              pseudo:true});
statementSpec.set("or",      {ifmt:iEXP2, afmt:aRRR,    opcode:[14,22,7],
                              pseudo:true});
statementSpec.set("xor",     {ifmt:iEXP2, afmt:aRRR, opcode:[14,22,6],
                              pseudo:true});

// Mnemonics for logicb instructions

statementSpec.set("invb",    {ifmt:iEXP2, afmt:aRRk, opcode:[14,23,12],
                              pseudo:true});
statementSpec.set("andb",    {ifmt:iEXP2, afmt:aRRRk, opcode:[14,23,1],
                              pseudo:true});
statementSpec.set("orb",     {ifmt:iEXP2, afmt:aRRRk, opcode:[14,23,7],
                              pseudo:true});
statementSpec.set("xorb",    {ifmt:iEXP2, afmt:aRRRk, opcode:[14,23,6],
                              pseudo:true});

// Mnemonic for bit field

statementSpec.set("field",   {ifmt:iEXP2, afmt:aRkk,  opcode:[14,22], pseudo:true});
