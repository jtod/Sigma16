// Sigma16: architecture.js
// Copyright (c) 2019 John T. O'Donnell.  john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later.  Sigma16/LICENSE.txt,NOTICE.txt

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


//-------------------------------------------------------------------------------
// Instruction table
//-------------------------------------------------------------------------------

const mnemonicRRR =
  ["add",     "sub",     "mul",     "div",
   "cmp",     "cmplt",   "cmpeq",   "cmpgt",
   "inv",     "and",     "or",      "xor",
   "nop",     "trap",    "EXP",     "RX"]

const mnemonicRX =
  ["lea",     "load",    "store",   "jump",
   "jumpc0",  "jumpc1",  "jumpf",   "jumpt",
   "jal",     "testset", "nop",     "nop",
   "nop",     "nop",     "nop",     "nop"]

const mnemonicEXP =
  [
// EXP0
   "rfi",
// RREXP
   "execute", "invnew",
// RCEXP
   "getctl",  "putctl",
// RRREXP
   "push",    "pop",     "top",
// RRKEXP
      "shiftl",  "shiftr", "invb",
// RRKKEXP
      "extract", "extracti", "inject", "injecti",
// RRRKEXP
   "logicw",      
// RRRKKEXP
   "logicb",
// RRXEXP
   "save",    "restore"
  ]

//-------------------------------------------------------------------------------
// Instruction set and assembly language formats
//-------------------------------------------------------------------------------

// Assembly language statement formats (machine language format)

const RRR         =  0;    // R1,R2,R3    (RRR)
const RR          =  1;    // R1,R2       (RRR, omitting d or b field
const RX          =  2;    // R1,xyz[R2]  (RX)
const KX          =  3;    // R1,xyz[R2]  (RX)
const JX          =  4;    // loop[R0]    (RX omitting d field)
const EXP0        =  5;    // EXP format with no operand
const RREXP       =  6;    // R1,R2      (EXP)
const RRREXP      =  7;    // R1,R2,R3    (EXP) like RRR instruction but expand op
const RRKEXP      =  8;    // R1,R2,3    (EXP)
const RKKEXP      =  9;    // R1,3,5     (EXP)
const RRRKEXP     = 10;    // R1,R2,R3,k    (EXP) logicw
const RRKKEXP     = 11;    // R1,R2,3    (EXP)
const RRRKKEXP    = 12;    // R1,R2,R3,g,h    (EXP) logicb
const RRXEXP      = 13;    // save R4,R7,3[R14]   (EXP)
const RCEXP       = 14;    // getctl R4,mask register,control (EXP)
const DATA        = 15;    // -42
const COMMENT     = 16;    // ; full line comment, or blank line

const DirModule   = 17;    // module directive
const DirImport   = 18;    // import directive
const DirExport   = 19;    // export directive
const DirOrg      = 20;    // org directive
const DirEqu      = 21;    // equ directive
const UNKNOWN     = 22;    // statement format is unknown
const EMPTY       = 23;    // empty statement

const NOOPERATION = 24;    // error
const NOOPERAND   = 25;    // statement has no operand

// Need to update ???
function showFormat (n) {
    let f = ['RRR','RR','RX', 'KX', 'JX','EXP0', 'RREXP', 'RRREXP', 'RRKEXP',
             'RKKEXP', 'RRRKEXP',  'RRKKEXP', 'RRRKKEXP', 'RRXEXP',
             'RCEXP', 'DATA','COMMENT',
             'DirModule', 'DirImport', 'DirExport', 'DirOrg', 'DirEqu',
             'UNKNOWN', 'EMPTY'] [n];
    let r = f ? f : 'UNKNOWN';
    return r;
}

// Give the size of generated code for an instruction format
function formatSize (fmt) {
    if (fmt==RRR || fmt==RR || fmt==EXP0 || fmt==DATA) {
	return 1
    } else if (fmt==RX | fmt==KX | fmt==JX
               | fmt==RREXP |  fmt==RCEXP | fmt==RRREXP | fmt==RRKEXP
               | fmt==RRRKEXP | fmt==RRRKKEXP | fmt==RKKEXP
               | fmt==RRKKEXP | fmt==RRXEXP) {
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
statementSpec.set("invold",   {format:RR,  opcode:[8]});
statementSpec.set("andold",   {format:RRR, opcode:[9]});
statementSpec.set("orold",    {format:RRR, opcode:[10]});
statementSpec.set("xorold",   {format:RRR, opcode:[11]});
statementSpec.set("nop",   {format:RRR, opcode:[12]});
statementSpec.set("trap",  {format:RRR, opcode:[13]});

// If op=14, escape to EXP format
// If op=15, escape to RX format.

// RX instructions
statementSpec.set("lea",      {format:RX,  opcode:[15,0]});
statementSpec.set("load",     {format:RX,  opcode:[15,1]});
statementSpec.set("store",    {format:RX,  opcode:[15,2]});
statementSpec.set("jump",     {format:JX,  opcode:[15,3,0]});
statementSpec.set("jumpc0",   {format:KX,  opcode:[15,4]});
statementSpec.set("jumpc1",   {format:KX,  opcode:[15,5]});
statementSpec.set("jumpf",    {format:RX,  opcode:[15,6]});
statementSpec.set("jumpt",    {format:RX,  opcode:[15,7]});
statementSpec.set("jal",      {format:RX,  opcode:[15,8]});
statementSpec.set("testset",  {format:RX,  opcode:[15,9]});

// Mnemonics for EXP instructions
// Expanded instructions with one word: opcode 0,...,7
// EXP0
statementSpec.set("rfi",      {format:EXP0,      opcode:[14,0]});
// Expanded instructions with two words: opcode >= 8
// RRXEXP
statementSpec.set("save",     {format:RRXEXP,    opcode:[14,8]});
statementSpec.set("restore",  {format:RRXEXP,    opcode:[14,9]});
// RCEXP
statementSpec.set("getctl",   {format:RCEXP,     opcode:[14,10]});
statementSpec.set("putctl",   {format:RCEXP,     opcode:[14,11]});
// RREXP
statementSpec.set("execute",  {format:RREXP,     opcode:[14,12]}); // to do ????
// RRREXP
statementSpec.set("push",     {format:RRREXP,    opcode:[14,13]});
statementSpec.set("pop",      {format:RRREXP,    opcode:[14,14]});
statementSpec.set("top",      {format:RRREXP,    opcode:[14,15]});
// RRKEXP
statementSpec.set("shiftl",   {format:RRKEXP,    opcode:[14,16]});
statementSpec.set("shiftr",   {format:RRKEXP,    opcode:[14,17]});
// RRKKEXP
statementSpec.set("extract",  {format:RRKKEXP,   opcode:[14,18]});
statementSpec.set("extracti", {format:RRKKEXP,   opcode:[14,19]});
statementSpec.set("inject",   {format:RRRKKEXP,   opcode:[14,20]});
statementSpec.set("injecti",  {format:RRRKKEXP,   opcode:[14,21]});
// RRRKEXP
statementSpec.set("logicw",   {format:RRRKEXP,   opcode:[14,22]});
// RRRKKEXP
statementSpec.set("logicb",  {format:RRRKKEXP,   opcode:[14,23]});
// Assembler directives
statementSpec.set("data",    {format:DATA,       opcode:[]});
statementSpec.set("module",  {format:DirModule,  opcode:[]});
statementSpec.set("import",  {format:DirImport,  opcode:[]});
statementSpec.set("export",  {format:DirExport,  opcode:[]});
statementSpec.set("org",     {format:DirOrg,     opcode:[]}); // to do ????
statementSpec.set("equ",     {format:DirEqu,     opcode:[]}); // to do ????

// -------------------------------------
// Pseudoinstructions
// -------------------------------------

// JX is a pseudoinstruction format: an assembly language statement
// format which omits the d field, but the machine language format is
// RX, where R0 is used for the d field.  For example, jump loop[R5]
// doesn't require d field in assembly language, but the machine
// language uses d=R0.

// Mnemonics for jumpc0 based on signed comparisons, overflow, carry
statementSpec.set("jumple",   {format:JX,  opcode:[15,4,bit_ccg]});
statementSpec.set("jumpne",   {format:JX,  opcode:[15,4,bit_ccE]});
statementSpec.set("jumpge",   {format:JX,  opcode:[15,4,bit_ccl]});
statementSpec.set("jumpnv",   {format:JX,  opcode:[15,4,bit_ccv]});
statementSpec.set("jumpnvu",  {format:JX,  opcode:[15,4,bit_ccV]});
statementSpec.set("jumpnco",  {format:JX,  opcode:[15,4,bit_ccC]});

// Mnemonics for jumpc1 based on signed comparisons
statementSpec.set("jumplt",   {format:JX,  opcode:[15,5,bit_ccl]});
statementSpec.set("jumpeq",   {format:JX,  opcode:[15,5,bit_ccE]});
statementSpec.set("jumpgt",   {format:JX,  opcode:[15,5,bit_ccg]});
statementSpec.set("jumpv",    {format:JX,  opcode:[15,5,bit_ccv]});
statementSpec.set("jumpvu",   {format:JX,  opcode:[15,5,bit_ccV]});
statementSpec.set("jumpco",   {format:JX,  opcode:[15,5,bit_ccC]});


// RREXP pseudo logicw
statementSpec.set("inv",      {format:RREXP,  opcode:[14,22,12], pseudo:true});
// RRKEXP pseudo: logicb
statementSpec.set("invb",     {format:RRKEXP, opcode:[14,23,12], pseudo:true});
// RRREXP pseudo logic
statementSpec.set("and",      {format:RRREXP, opcode:[14,22,1], pseudo:true});
statementSpec.set("or",       {format:RRREXP, opcode:[14,22,7], pseudo:true});
statementSpec.set("xor",      {format:RRREXP, opcode:[14,22,6], pseudo:true});
// RRRKEXP pseudo logicb
statementSpec.set("andb",     {format:RRRKEXP,    opcode:[14,23,1], pseudo:true});
statementSpec.set("orb",      {format:RRRKEXP,    opcode:[14,23,7], pseudo:true});
statementSpec.set("xorb",     {format:RRRKEXP,    opcode:[14,23,6], pseudo:true});

// Mnemonic for field
// RKKEXP pseudo injecti
statementSpec.set("field",    {format:RKKEXP,   opcode:[14,22],
                               pseudo:true});

// -------------------------------------
// Mnemonics for control registers
// -------------------------------------

// The getctl and putctl instructions contain a field indicating which
// control register to use. This record defines the names of those
// control registers (used in the assembly language) and the numeric
// index for the control register (used in the machine language).

var ctlReg = new Map();
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
// Interrupt request and mask bits
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
