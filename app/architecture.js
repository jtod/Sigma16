
//------------------------------------------------------------------------------
// Instruction mnemonics
//------------------------------------------------------------------------------

const mnemonicRRR =
  ["add",    "sub",    "mul",    "div",
   "cmp",    "cmplt",  "cmpeq",  "cmpgt",
   "inv",    "and",    "or",     "xor",
   "nop",    "trap",   "EXP",     "RX"]

const mnemonicRX =
  ["lea",    "load",   "store",  "jump",
   "jumpc0", "jumpc1", "jumpf",  "jumpt",
   "jal",    "nop",    "nop",    "nop",
   "nop",    "nop",    "nop",    "nop"]

const mnemonicEXP =
  ["shl",    "shr",    "getctl",    "putctl",
   "rfi",    "nop",    "nop",    "nop",
   "nop",    "nop",    "nop",    "nop",
   "nop",    "nop",    "nop",    "nop"]

//----------------------------------------------------------------------
// Instruction set and assembly language formats
// ---------------------------------------------------------------------

// Assembly language statement formats (machine language format)

const RRR         = 0     // R1,R2,R3    (RRR)
const RR          = 1     // R1,R2       (RRR, omitting d or b field
const RX          = 2;    // R1,xyz[R2]  (RX)
const JX          = 3;    // loop[R0]    (RX omitting d field)
const DATA        = 4;    // -42
const COMMENT     = 5;    // ; full line comment, or blank line
const DIRECTIVE   = 6;    // assembler directive
const NOOPERATION = 7;    // error
const NOOPERAND   = 8;    // statement has no operand

const RRRX        = 9;    // R1,R2,R3    (EXP)
const RRKX        = 10;    // R1,R2,3    (EXP)
const RCX         = 11;    // getcth R4,mask

function showFormat (n) {
    let f = ['RRR','RR','RX','JX','DATA','COMMENT','NOOPERATION'] [n];
    let r = f ? f : 'UNKNOWN';
    return r;
}

// Give the size of generated code for an instruction format
function formatSize (fmt) {
    if (fmt==RRR || fmt==RR || fmt==DATA) {
	return 1
    } else if (fmt==RRKX | fmt==RX | fmt==JX | fmt==RCX) {
	return 2
    } else {
	return 0
    }
}

//------------------------------------------------------------------------------
// Assembly language statements
//------------------------------------------------------------------------------

// The instruction set is represented by a map from mnemonic to
// statementSpec spec

var statementSpec = new Map();

// Each statement is initialized as noOperation; this is overridden if a
// valid operation field exists (by the parseOperation function)

const noOperation = {format:NOOPERATION, opcode:[]};

statementSpec.set("module", {format:DIRECTIVE, opcode:[]})
statementSpec.set("import", {format:DIRECTIVE, opcode:[]})
statementSpec.set("export", {format:DIRECTIVE, opcode:[]})
statementSpec.set("org",    {format:DIRECTIVE, opcode:[]})

// Data statements
statementSpec.set("data",  {format:DATA, opcode:[]});

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
statementSpec.set("shl",      {format:RRKX, opcode:[14,0]});
statementSpec.set("shr",      {format:RRKX, opcode:[14,1]});
statementSpec.set("putctl",   {format:RCX,  opcode:[14,2]});
statementSpec.set("getctl",   {format:RCX,  opcode:[14,3]});
statementSpec.set("rfi",      {format:NOOPERAND,  opcode:[14,4]});

// Mnemonics for control registers

var ctlReg = new Map();
ctlReg.set ("system",  {ctlRegIndex:0});
ctlReg.set ("mask",    {ctlRegIndex:1});
ctlReg.set ("req",     {ctlRegIndex:2});
ctlReg.set ("save",    {ctlRegIndex:3});
ctlReg.set ("vector",  {ctlRegIndex:4});
ctlReg.set ("prog",    {ctlRegIndex:5});
ctlReg.set ("progEnd", {ctlRegIndex:6});
ctlReg.set ("data",    {ctlRegIndex:7});
ctlReg.set ("dataEnd", {ctlRegIndex:8});


//------------------------------------------------------------------------------
// System register bits
//------------------------------------------------------------------------------

// Define the bit index for each flag in the system register, where 0
// indicates the least significant bit.  In addition, constants are
// defined for setting or clearing the flags

// When the machine boots, the registers are initialized to 0.  The
// system state flag is defined so that sysStateBit=0 indicates system
// (or supervisor) state.  The reason for this is that the system
// should boot into a state that enables the operating system to
// initialize itself.

const sysStateBit  = 0;   // 0 - system state,  1 - user state
const intEnableBit = 1;   // 0 - disabled,      1 - enabled

const clearIntEnable = maskToClearBit (intEnableBit);
const setSystemState = maskToClearBit (sysStateBit);

//------------------------------------------------------------------------------
// Interrupt request and mask bits
//------------------------------------------------------------------------------

const timerBit         = 0;   // timer has gone off
const segFaultBit      = 1;   // access invalid virtual address
const memFaultBit      = 2;   // invalid memory virtual address
const trapBit          = 3;   // user trap
const overflowBit      = 4;   // overflow occurred
const zDivBit          = 5;   // division by 0

const setTimerMask     = maskToSetBit (timerBit);
const setSegFaultMask  = maskToSetBit (segFaultBit);
const setMemFaultMask  = maskToSetBit (memFaultBit);
const setOverflowMask  = maskToSetBit (overflowBit);
const setZDivMask      = maskToSetBit (zDivBit);
const setTrapMask      = maskToSetBit (trapBit);

//Example: to signal overflow: setBitInRegMask (req,setOverflowMask);
