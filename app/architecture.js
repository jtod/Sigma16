// Sigma16: architecture.js
// Copyright (c) 2019 John T. O'Donnell.  john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. Sigma16/LICENSE.txt,NOTICE.txt

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

//-------------------------------------------------------------------------------
// architecture.js defines tables specifying opcodes, mnemonics, flag
// bits, and other aspects of the architecture.
//-------------------------------------------------------------------------------

// Instruction mnemonics

const mnemonicRRR =
  ["add",    "sub",    "mul",     "div",
   "cmp",    "cmplt",  "cmpeq",   "cmpgt",
   "inv",    "and",    "or",      "xor",
   "nop",    "trap",   "EXP",     "RX"]

const mnemonicRX =
  ["lea",    "load",   "store",   "jump",
   "jumpc0", "jumpc1", "jumpf",   "jumpt",
   "jal",    "nop",    "nop",     "nop",
   "nop",    "nop",    "nop",     "nop"]

const mnemonicEXP =
  ["shiftl",  "shiftr",  "getctl",  "putctl",
   "push",    "pop",     "top",     "rfi",
   "save",    "restore", "extract", "execute",
   "getbit",  "getbiti", "putbit",  "putbiti",
   "nop",     "nop",     "nop",     "nop",
   "nop",     "nop",     "nop",     "nop"]

//-------------------------------------------------------------------------------
// Instruction set and assembly language formats
//-------------------------------------------------------------------------------

// Assembly language statement formats (machine language format)

const RRR         = 0     // R1,R2,R3    (RRR)
const RR          = 1     // R1,R2       (RRR, omitting d or b field
const RX          = 2;    // R1,xyz[R2]  (RX)
const JX          = 3;    // loop[R0]    (RX omitting d field)
const RRREXP      = 4;    // R1,R2,R3    (EXP) like RRR instruction but expand op
const RRKEXP      = 5;    // R1,R2,3    (EXP)
const RRXEXP      = 6;    // save R4,R7,3[R14]   (EXP)
const RCEXP       = 7;    // getctl R4,imask register,control (EXP)

const EXP0        = 8;    // EXP format with no operand
const DATA        = 9;    // -42
const COMMENT     = 10;    // ; full line comment, or blank line

const DirModule   = 12;
const DirImport   = 13;
const DirExport   = 14;
const DirOrg      = 15;
const DirEqu      = 16;
const UNKNOWN     = 17;
const EMPTY       = 18;

const NOOPERATION = 19;    // error
const NOOPERAND   = 20;    // statement has no operand

// kil these
// const ASMDIR      = 12;    // fcn module    (no operand)
// const ASMDIRX     = 13;    // org $f000     (operand is expression)
// const ASMDIRIDENT = 14;   // export x,y    (operand is list of names)

// Need to update ???
function showFormat (n) {
    let f = ['RRR','RR','RX','JX','RRREXP', 'RRKEXP', 'RRXEXP',
             'RCEXP', 'DATA','COMMENT','NOOPERATION', 'NOOPERAND',
             'ASMDIR', 'ASMDIRX', 'ASMDIRIDENT'] [n];
    let r = f ? f : 'UNKNOWN';
    return r;
}

// Give the size of generated code for an instruction format
function formatSize (fmt) {
    if (fmt==RRR || fmt==RR || fmt==EXP0 || fmt==DATA) {
	return 1
    } else if (fmt==RX | fmt==JX
               | fmt==RRREXP | fmt==RRKEXP | fmt==RRXEXP | fmt==RCEXP) {
	return 2
    } else if (fmt==NOOPERAND) {
	return 1
    } else {
	return 0
    }
}

//-------------------------------------------------------------------------------
// Assembly language statements
//-------------------------------------------------------------------------------

// The instruction set is represented by a map from mnemonic to
// statementSpec spec

var statementSpec = new Map();

// Each statement is initialized as noOperation; this is overridden if a
// valid operation field exists (by the parseOperation function)

// const noOperation = {format:NOOPERATION, opcode:[]};


// Data statements
statementSpec.set("data",  {format:DATA, opcode:[]});

// Empty statement
statementSpec.set("",   {format:EMPTY, opcode:[]});
let emptyStmt = statementSpec.get("");

// Opcodes (in the op field) of 0-13 denote RRR instructions
statementSpec.set("add",   {format:RRR, opcode:[0]});
statementSpec.set("sub",   {format:RRR, opcode:[1]});
statementSpec.set("mul",   {format:RRR, opcode:[2]});
statementSpec.set("div",   {format:RRR, opcode:[3]});
statementSpec.set("cmp",   {format:RR,  opcode:[4]});
statementSpec.set("cmplt", {format:RRR, opcode:[5]});
statementSpec.set("cmpeq", {format:RRR, opcode:[6]});
statementSpec.set("cmpgt", {format:RRR, opcode:[7]});
statementSpec.set("inv",   {format:RR,  opcode:[8]});
statementSpec.set("and",   {format:RRR, opcode:[9]});
statementSpec.set("or",    {format:RRR, opcode:[10]});
statementSpec.set("xor",   {format:RRR, opcode:[11]});
statementSpec.set("nop",   {format:RRR, opcode:[12]});
statementSpec.set("trap",  {format:RRR, opcode:[13]});

// If op=14, escape to EXP format
// arithmetic with control of carry, shifting, privileged instructions

// If op=15, escape to RX format.  JX is an assembly language
// statement format which omits the d field, but the machine language
// format is RX, where R0 is used for the d field.  For example, jump
// loop[R5] doesn't require d field in assembly language, but the
// machine language uses d=R0.

// Core instructions
statementSpec.set("lea",      {format:RX,  opcode:[15,0]});
statementSpec.set("load",     {format:RX,  opcode:[15,1]});
statementSpec.set("store",    {format:RX,  opcode:[15,2]});
statementSpec.set("jump",     {format:JX,  opcode:[15,3]});
statementSpec.set("jumpc0",   {format:RX,  opcode:[15,4]});
statementSpec.set("jumpc1",   {format:RX,  opcode:[15,5]});
statementSpec.set("jumpf",    {format:RX,  opcode:[15,6]});
statementSpec.set("jumpt",    {format:RX,  opcode:[15,7]});
statementSpec.set("jal",      {format:RX,  opcode:[15,8]});

// Mnemonics for jumpc0 based on signed comparisons, overflow, carry
statementSpec.set("jumple",   {format:JX,  opcode:[15,4,bit_ccg]});
statementSpec.set("jumpne",   {format:JX,  opcode:[15,4,bit_ccE]});
statementSpec.set("jumpge",   {format:JX,  opcode:[15,4,bit_ccl]});
statementSpec.set("jumpnv",   {format:JX,  opcode:[15,4,bit_ccv]});
statementSpec.set("jumpnvu",  {format:JX,  opcode:[15,4,bit_ccV]});
statementSpec.set("jumpnco",  {format:JX,  opcode:[15,4,bit_ccc]});

// Mnemonics for jumpc1 based on signed comparisons
statementSpec.set("jumplt",   {format:JX,  opcode:[15,5,bit_ccl]});
statementSpec.set("jumpeq",   {format:JX,  opcode:[15,5,bit_ccE]});
statementSpec.set("jumpgt",   {format:JX,  opcode:[15,5,bit_ccg]});
statementSpec.set("jumpv",    {format:JX,  opcode:[15,5,bit_ccv]});
statementSpec.set("jumpvu",   {format:JX,  opcode:[15,5,bit_ccV]});
statementSpec.set("jumpco",   {format:JX,  opcode:[15,5,bit_ccc]});

// Mnemonics for EXP instructions
statementSpec.set("shiftl",   {format:RRKEXP,    opcode:[14,0]});
statementSpec.set("shiftr",   {format:RRKEXP,    opcode:[14,1]});
statementSpec.set("getctl",   {format:RCEXP,     opcode:[14,2]});
statementSpec.set("putctl",   {format:RCEXP,     opcode:[14,3]});
statementSpec.set("push",     {format:RRREXP,    opcode:[14,4]});
statementSpec.set("pop",      {format:RRREXP,    opcode:[14,5]});
statementSpec.set("top",      {format:RRREXP,    opcode:[14,6]});
statementSpec.set("rfi",      {format:EXP0,      opcode:[14,7]});
statementSpec.set("save",     {format:RRXEXP,    opcode:[14,8]});
statementSpec.set("restore",  {format:RRXEXP,    opcode:[14,9]});
statementSpec.set("getbit",   {format:RRKEXP,    opcode:[14,10]});
statementSpec.set("getbiti",  {format:RRKEXP,    opcode:[14,11]});
statementSpec.set("putbit",   {format:RRKEXP,    opcode:[14,12]});
statementSpec.set("putbiti",  {format:RRKEXP,    opcode:[14,13]});


// Assembler directives
statementSpec.set("module",  {format:DirModule,  opcode:[]});
statementSpec.set("import",  {format:DirImport,  opcode:[]});
statementSpec.set("export",  {format:DirExport,  opcode:[]});
statementSpec.set("org",     {format:DirOrg,     opcode:[]});
statementSpec.set("equ",     {format:DirEqu,     opcode:[]});

//statementSpec.set("module", {format:ASMDIR, opcode:[]})
//statementSpec.set("import", {format:ASMDIRIDENT, opcode:[]})
//statementSpec.set("export", {format:ASMDIRIDENT, opcode:[]})
//statementSpec.set("org",    {format:ASMDIRX, opcode:[]})
// Mnemonics for assembler directives

// Mnemonics for control registers

// The getctl and putctl instructions contain a field indicating which
// control register to use. This record defines the names of those
// control registers (used in the assembly language) and the numeric
// index for the control register (used in the machine language).

var ctlReg = new Map();
ctlReg.set ("status",   {ctlRegIndex:0});
ctlReg.set ("imask",    {ctlRegIndex:1});
ctlReg.set ("ireq",     {ctlRegIndex:2});
ctlReg.set ("istat",    {ctlRegIndex:3});
ctlReg.set ("ipc",      {ctlRegIndex:4});
ctlReg.set ("ivect",    {ctlRegIndex:5});
ctlReg.set ("prog",     {ctlRegIndex:6});
ctlReg.set ("progEnd",  {ctlRegIndex:7});
ctlReg.set ("data",     {ctlRegIndex:8});
ctlReg.set ("dataEnd",  {ctlRegIndex:9});

//-------------------------------------------------------------------------------
// Status register bits
//-------------------------------------------------------------------------------

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

const userStateBit     = 0;   // 0 = system state,  1 = user state
const intEnableBit     = 1;   // 0 = disabled,      1 = enabled


// These constants provide a faster way to set or clear the flags

const clearIntEnable = maskToClearBitBE (intEnableBit);
const setSystemState = maskToClearBitBE (userStateBit);

//-------------------------------------------------------------------------------
// Interrupt irequest and imask bits
//-------------------------------------------------------------------------------

const timerBit         = 0;   // timer has gone off
const segFaultBit      = 1;   // access invalid virtual address
const stackFaultBit    = 2;   // invalid memory virtual address
const userTrapBit      = 3;   // user trap
const overflowBit      = 4;   // overflow occurred
const zDivBit          = 5;   // division by 0

//-------------------------------------------------------------------------------
// Assembly language data definitions for control bits
//-------------------------------------------------------------------------------

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
