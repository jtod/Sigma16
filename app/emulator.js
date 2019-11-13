//------------------------------------------------------------------------------
// emulator.js
//------------------------------------------------------------------------------

// This module defines the emulator, which interprets machine language
// programs and displays effects in the gui.

// should be in es
// let nInstructionsExecuted = 0;
// let instrLooperDelay = 1000;
// let instrLooperShow = true;



//------------------------------------------------------------------------------
// Interface to the gui
//------------------------------------------------------------------------------

// The functions receive a parameter 'es' which carries the current
// emulator state.  The gui, when it calls one of the main interface
// functions, passes the global emulatorState.

// The machine state (registers and memory) are global and initialized
// by gui.  The emulator state is a global variable named
// emulatorState, defined in state.js.

// The interface to the emulator consists of the following functions,
// which are called directly by the gui. When they call other
// functions, es is passed as a parameter.  Thus only the following
// interface functions access a global variable.

// Called by ?
//   parseCopyObjectModuleToMemory (es)
    
// Called by gui.js
//   clearInstrDecode ()

// Called by events in Sigma16.html
//   procReset(es)       -- put processor into initial state
//   boot(es)            -- copy the executable into memory
//   procStep(es)        -- execute one instruction
//   procRun(es)         -- execute instructions repeatedly until halted
//   procPause(es)       -- halt execution (can resume execution later)
//   procInterrupt()   -- not implemented yet
//   procBreakpoint()  -- not implemented yet

//------------------------------------------------------------------------------
// Architecture
//------------------------------------------------------------------------------

const mnemonicRRR =
  ["add",    "sub",    "mul",    "div",
   "cmp",    "cmplt",  "cmpeq",  "cmpgt",
   "inv",    "and",    "or",     "xor",
   "addc",   "trap",   "XX",     "RX"]

const mnemonicRX =
  ["lea",    "load",   "store",  "jump",
   "jumpc0", "jumpc1", "jumpf",  "jumpt",
   "jal",    "nop",    "nop",    "nop",
   "nop",    "nop",    "nop",    "nop"]

const mnemonicxX =
  ["nop",    "nop",    "nop",    "nop",
   "nop",    "nop",    "nop",    "nop",
   "nop",    "nop",    "nop",    "nop",
   "nop",    "nop",    "nop",    "nop"]

//------------------------------------------------------------------------------
// Instruction decode
//------------------------------------------------------------------------------

function showInstrDecode (es) {
    es.instrCodeStr = (instrCode ? wordToHex4 (instrCode) : "")
	+ " " + (es.instrDisp ? wordToHex4 (es.instrDisp) : "");
    es.instrEAStr = es.instrEA ? wordToHex4 (es.instrEA) : "";
    console.log (`showInstrDecode fmt = ${es.instrFmtStr}`);
    refreshInstrDecode (es);
}

function clearInstrDecode (es) {
    es.instrOpCode = null;
    es.instrDisp = null;
    es.instrCodeStr  = "";
    es.instrFmtStr   = "";
    es.instrOp    = "";
    es.instrArgs  = "";
    es.instrEA = null;
    es.instrEAStr    = "";
    es.instrEffect = [];
}

function refreshInstrDecode (es) {
    console.log ("refreshInstrDecode");
    instrCodeElt.innerHTML = es.instrCodeStr;
    instrFmtElt.innerHTML  = es.instrFmtStr;
    instrOpElt.innerHTML   = es.instrOpStr;
    instrArgsElt.innerHTML = showArgs(es); // instrArgsStr;
    instrEAElt.innerHTML   = es.instrEAStr;
    let ccval = regFile[15].val;
    instrCCElt.innerHTML      = showCC(ccval);
    instrEffect1Elt.innerHTML = showEffect(es,0);
    instrEffect2Elt.innerHTML = showEffect(es,1);
}

function showArgs (es) {
    if (es.instrFmtStr==="RRR") {
	return `R${ir_d},R${ir_a},R${ir_b}`;
    } else if (es.instrFmtStr==="RX") {
	return `R${ir_d},${wordToHex4(es.instrDisp)}[R${ir_a}]`;
    } else {
	return "?";
    }
}

function showEffect (es,i) {
    console.log(`showEffect ${i}`);
    if (es.instrEffect[i]) {
	let [dest,idx,val,name] = es.instrEffect[i];
	if (dest==="R") {
	    console.log (`showEffect ${i} ${es.instrEffect[i]}`);
//	    console.log (`showEffect ${i}  ${dest} ${idx} := ${val});
	    return `${name} := ${wordToHex4(val)}`;
	} else if (dest==="M") {
	    console.log ("showEffect M");
	    return `M[${wordToHex4(idx)}]:=${wordToHex4(val)}`;
	}
    } else { return ""; }
}

//------------------------------------------------------------------------------
// Boot
//------------------------------------------------------------------------------

// Attempt to copy an executable into memory for execution.  A module
// is executable if it contains object code but doesn't have any
// imports; in this case isExecutable is true.  The object code can
// come from either the assembler, or it can be read in as an object
// file.  The presence of an assembly listing and asmap is optional.

// (Interface: called from gui) Boot from either the current module
// (without linking) or the load module (if the linker has been run).

function boot(es) {
    console.log ('boot');
    let m = getCurrentModule ();
    exMod = m;
    if (m.isExecutable) {
	console.log ('Current module is executable: booting');
	resetRegisters ();
	memClearAccesses();
	copyExecutableToMemory (es,m);
	es.asmListingPlain = m.asmListingPlain;
	es.asmListingDec = m.asmListingDec;
	for (let i = 0; i < es.asmListingDec.length; i++) { // copy the array
	    es.asmListingCurrent[i] = es.asmListingDec[i];
	}
	initListing (es);
	es.nInstructionsExecuted = 0;
	document.getElementById("nInstrExecuted").innerHTML =
	    es.nInstructionsExecuted;
    } else {
	console.log ('cannot boot')
    }
}

// Copy a module's object code into memory.  Should use objectCode
// rather than codeWord, but objectCode will need to support org.  So
// far it's just a list of words.  This version doesn't yet support
// org and it requires the module to be assembled (rather than read
// in).

function copyExecutableToMemory (es,m) {
    console.log ('copyExecutableToMemory');
    let stmt = m.asmStmt;
    let locationCounter = 0;
    for (let i = 0; i < stmt.length; i++) {
	console.log('bootCM ' + i + ' => ' + stmt[i].codeWord1
		    + ' ' + stmt[i].codeWord2 );
	if (stmt[i].codeWord1 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord1);
	    locationCounter++;
	}
	if (stmt[i].codeWord2 >= 0) {
	    memStore (locationCounter, stmt[i].codeWord2);
	    locationCounter++;
	}
    }
    memShowAccesses();
    memDisplay();
    es.curAsmap = m.asmap;
    showAsmap (es.curAsmap);
    setProcStatus (es,"Ready");
    console.log ('copyExecutableToMemory done');
}

function parseCopyObjectModuleToMemory (es) {
    console.log('boot');
    let objText = document.getElementById("LinkerText").value;
    console.log('objText = ' + objText);
    let xs = objText.split("\n");
    console.log("linker boot: " + xs.length + " lines");
//    console.log(xs);
//    line1 = xs[0];
//    console.log('line1 = ' + line1);
//    fields = line1.split(" ");
//    console.log('fields = ' + fields);
//    console.log('field0 = ' + fields[0]);
//    console.log('field1 = ' + fields[1]);
    //    console.log('field2 = ' + fields[2]);
    bootCurrentLocation = 0;
    for (var i = 0; i < xs.length; i++) {
	linkerBootLine(es, i, xs[i]);
//	experiment(xs[i]);
    }
}

// Should check the operation, implement org, and provide suitable
// error messages, but that's for later.  For now, just assume it is
// hexdata with valid argument

function linkerBootLine (es,m,i,x) {
    let y = parseAsmLine (m,i,x);
//    printAsmLine (y);
    let w = y.fieldOperands;
    let n =  hex4ToWord(w);
//    console.log('linkerBootLine ' + w + ' = ' + n);
    console.log('linkerBootLine ' + i + ' ---' + x + '--- = ' + n);
    updateMem2(bootCurrentLocation,n);
    bootCurrentLocation++;
}

// Parse the object code
function parseObject () {
    console.log('parseObject');
}

//------------------------------------------------------------------------------
// Highlighting current and next instruction in processor assembly listing
//------------------------------------------------------------------------------

// The assembler provides an array of source lines, which it passes on
// to the linker and thence to the emulator.  There are two strings
// for each source line: one contains <span> elements to enable the
// fields to be highlighted, just as in the assembly listing.  The
// other omits these elements, so the entire line can be highlighted
// to indicate (with just one color for the line) the instruction that
// has just executed and the instruction that will be executed next.

// The assembler produces listing lines with <span> elements to allow
// the fields of the line to be highlighted. These are stored in
// listingHighlightedFields.  However, the emulator highlights an
// entire listing line to indicate the instruction that is currently
// executing, or that will execute next.  In order to prevent the
// highlighting of fields from overriding the highlighting of the
// current/next instruction, that is done using listingPlain.

// Given address a, the corresponding source statement is
//   i = es.curAsmap[a]
//   if i=null then no source is avaiable
//     else currentModule.asmStmt[i].srcLine

function initListing (es) {
    es.curInstrAddr = 0;
    es.curInstrLineNo = -1;  // -1 indicates no line has been highlighted
    es.nextInstrAddr = 0;
    es.nextInstrLineNo = es.curAsmap[0];
    highlightListingLine (es, es.nextInstrLineNo, "NEXT");
    setProcAsmListing (es,m);
}

function showListingParameters (es) {
    console.log ('showListingParameters'
		 + ' es.curInstrAddr=' + es.curInstrAddr
		 + ' es.curInstrLineNo=' + es.curInstrLineNo
		 + ' es.nextInstrAddr=' + es.nextInstrAddr
		 + ' es.nextInstrLineNo=' + es.nextInstrLineNo);
}

// Prepare the running listing before starting instructionby removing
// any existing highlighting of current and next instruction

function prepareListingBeforeInstr (es) {
    console.log ('prepareListingBeforeInstr');
    if (es.curInstrLineNo >= 0) {
	es.asmListingCurrent[es.curInstrLineNo] =
	    es.asmListingDec[es.curInstrLineNo];
    }
    if (es.nextInstrLineNo >= 0) {
	es.asmListingCurrent[es.nextInstrLineNo] =
	    es.asmListingDec[es.nextInstrLineNo];
    }
    es.curInstrLineNo = -1;
    es.nextInstrLineNo = -1;
    showListingParameters(es);
}

// As it executes an instruction, the emulator sets curInstrAddr and
// nextInstrAddr.  After the instruction has finished, these
// instructions are highlighted in the listing

function highlightListingAfterInstr (es) {
    console.log ('highlightListingAfterInstr');
    console.log ('  curInstrAddr = ' + es.curInstrAddr);
    console.log ('  nextInstrAddr = ' + es.nextInstrAddr);

    es.curInstrLineNo = es.curAsmap[es.curInstrAddr];
    console.log ('  curInstrLineNo = ' + es.curInstrLineNo);
    if (es.curInstrLineNo >= 0) {
	highlightListingLine (es, es.curInstrLineNo, "CUR");
    }

    es.nextInstrLineNo = es.curAsmap[es.nextInstrAddr];
    console.log ('  nextInstrLineNo = ' + es.nextInstrLineNo);
    if (es.nextInstrLineNo >= 0) {
	highlightListingLine (es, es.nextInstrLineNo, "NEXT");
    }

    if (memDisplayModeFull) {
	highlightListingFull (es)
    } else {
	highlightListingFull (es)    // temp ?????
    }
}

function highlightListingFull (es,m) {
    console.log ('highlightListingFull');
    setProcAsmListing (es);
    let xa = es.curInstrLineNo - asmScrollOffsetAbove;
    xa = xa < 0 ? 0 : xa;
    let scrollOffset = xa * pxPerChar;
    console.log ('curInstrLineNo = ' + es.curInstrLineNo
		 + '  scrollOffset = ' + scrollOffset);
    procAsmListingElt.scroll (0, scrollOffset);
}


function highlightListingLine (es,i,highlight) {
    es.asmListingCurrent[i] =
	"<span class='" + highlight + "'>" + es.asmListingPlain[i] + "</span>";
}

// scrolling doesn't work if it just uses <pre> but not <code>

function setProcAsmListing (es) {
    console.log ('setProcAsmListing');
    let xs = "<pre><code class='HighlightedTextAsHtml'>"
    	+ es.asmListingCurrent.join('\n')
	+ "</code></pre>";
    document.getElementById('ProcAsmListing').innerHTML = xs;
}

//------------------------------------------------------------------------------

function clearCtlRegs () {
}

// deprecated use boot from linker instead
// function procBoot () {
//     console.log('procBoot');
//     bootCurrentModule ();
// }

function procStep(es) {
    console.log ('procStep');
    if (es.procStatus==="Stopped") { setProcStatus (es,"Ready"); }
    if (es.procStatus==="Ready") {
	clearInstrDecode (es);
	executeInstruction (es);
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    }
}

function procRun(es) {
    console.log ("procRun");
    if (es.procStatus==="Stopped") { setProcStatus (es,"Ready"); }
    clearInstrDecode (es);
    regClearAccesses (es);
    memClearAccesses (es);
    clearInstrDecode (es);
    instructionLooper (es);
}

// Promises don't allow pause button click to break in, need setTimer for that

//    return new Promise (function (resolve,reject) {
//	resolve (instructionLooper (es));
//    });

//------------------------------------------------------------------------------
// Processor status
//------------------------------------------------------------------------------

// Reset   -- registers and memory have been cleared
//               result of Reset button
//               can Boot or Reset
// Ready   -- can execute instruction
//               result of boot, step
//               can Step, Run, Reset, Boot
// Stopped -- processor would be running but emulator has stopped
//               result of pause or breakpoint
//               can restart with step or run
// Halted  -- processor has halted because trap 0 was executed
//               result of trap 0
//               can boot or reset

function setProcStatus (es,s) {
    es.procStatus = s;
    document.getElementById("procStatus").innerHTML=s;
}

function procReset(es) {
    console.log ("reset the processor");
    setProcStatus (es,"Reset");
    resetRegisters ();
    refreshRegisters ();
    memClear ();
    memClearAccesses ();
    memDisplay ();
    document.getElementById('ProcAsmListing').innerHTML = "";
    clearInstrDecode (es);
    refreshInstrDecode (es);
    es.nInstructionsExecuted = 0;
    document.getElementById("nInstrExecuted").innerHTML = es.nInstructionsExecuted;
}

//------------------------------------------------------------------------------
// Controlling instruction execution
//------------------------------------------------------------------------------


function procPause(es) {
    console.log ("procPause");
    setProcStatus (es,"Stopped");
}

function instructionLooper (es) {
    if (es.procStatus==="Ready") {
	if (es.instrLooperShow) {
	    clearInstrDecode (es);
	}
	executeInstruction (es);
	if (es.instrLooperShow) {
	    showInstrDecode (es);
	    highlightListingAfterInstr (es);
	}
	setTimeout (instructionLooper, es.instrLooperDelay);
    } else { return; }
}


//    while (es.procStatus==="Ready") {
//	executeInstruction ();
//    }
// memClearAccesses, memShowAccesses, and memDisplay are very slow...

//------------------------------------------------------------------------------
// Machine language semantics
//------------------------------------------------------------------------------

function executeInstruction (es) {
    console.log ('executeInstruction');
    es.nInstructionsExecuted++;
    document.getElementById("nInstrExecuted").innerHTML = es.nInstructionsExecuted;
    prepareListingBeforeInstr (es);
    memClearAccesses ();
    memDisplay ();
    regClearAccesses ();

    es.curInstrAddr = pc.get();
    instrCode = memFetchInstr (es.curInstrAddr);
    ir.put (instrCode);
    es.nextInstrAddr = binAdd (es.curInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    console.log('pc = ' + wordToHex4(pc.get()) + ' ir = ' + wordToHex4(instr));
    let tempinstr = ir.get();
    ir_b = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_a = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_d = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_op = tempinstr & 0x000f;
    console.log('instr fields = ' + ir_op + ' ' + ir_d + ' ' + ir_a + ' ' + ir_b);

    es.instrFmtStr = "RRR";             // Replace if opcode expands to RX or XX
    es.instrOpStr = mnemonicRRR[ir_op]  // Replace if opcode expands to RX or XX
    dispatch_RRR [ir_op] (es);
    
    regShowAccesses()
    memShowAccesses();
    memDisplay ();
}

// RRR instructions have three specified registers, and may also use
// the condition code cc, which is R15.

// a = reg[ir_sa]
// b = reg[ir_sb]
// d = reg[ir_d]
// c = reg[15] (condition code)

//  There are some variations in how different instructions use the
//  register operands, and there is a generic function for each
//  variation (e.g rrd) which takes a function argument (e.g add) that
//  performs the specific operation for that instruction.  The generic
//  functions access the registers, and the operation functions
//  perform the calculations.  This approach ensures that the correct
//  registers are highlighted: for example, an instruction that
//  doesn't need rb will not highlight rb.

// Convention: rr refers to the normal operands a and b; c refers to
// the condition code which is in R15; d refers to the destination
// register.

// rrd   -- use ra and rb as operands, place result in rd
// rrdc  -- use ra and rb as operands, place results in rd and cc
// crrdc -- use cc, ra and rb as operands, place results in rd and cc
// rd    -- use ra as operand, place result in rd, ignore rb and cc
// rrc   -- use ra and rb as operands, place result in cc, ignore rd
// rrrc  -- use ra, rb, and cc as operands, place results in rd and cc
// trap  -- perform trap; instruction ignores all registers but OS may use them


// demonstrate lambda expressions, and a curried lambda
/*
const foobar = (x) => (y) => x+y;
let baz = foobar (5);
let bar = foobar (41);
let abc = foobar (7) (50);
*/

// Apply f to a and b, load primary result into d, and load secondary
// result into c (e.g. add)

const rrdc = (f) => (es) => {
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let [primary, secondary] = f (a,b);
    regFile[ir_d].put(primary);
    if (ir_d<15) { regFile[15].put(secondary) }
}

// Apply f to a and load primary result into d (e.g. inv)

const rd = (f) => (es) => {
    let a = regFile[ir_a].get();
    let primary = f (a);
    regFile[ir_d].put(primary);
}

// Apply f to a and b, and load primary result into d (e.g. cmplt)

const rrd = (f) => (es) => {
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let primary = f (a,b);
    regFile[ir_d].put(primary);
}

// Apply f to a and b, and load primary result into c (e.g. cmp)

const rrc = (f) => (es) => {
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let cc = f (a,b);
    regFile[15].put(cc);
}

// Apply f to c, a and b, load primary result into d, and load
// secondary result into c (e.g. addc)

const crrdc = (f) => (es) => {
    let c = regFile[15].get();
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let [primary, secondary] = f (c,a,b);
    regFile[ir_d].put(primary);
    if (ir_d<15) { regFile[15].put(secondary) }
}

const op_trap = (es) => {
    let d = regFile[ir_d].get();
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    console.log (`trap ${d} ${a} ${b}`);
    if (d===0) {
	console.log ("Trap: halt");
	setProcStatus (es,"Halted");
    }
}

const handle_xx = (es) => {
    console.log ('xx');
    es.instrFmtStr = "XX";
}

const handle_rx = (es) => {
    console.log ('handle rx' + ir_b);
    es.instrFmtStr = "RX";
    dispatch_RX [ir_b] (es);
}

const dispatch_RRR =
      [ rrdc (op_add),     // 0
        rrdc (op_sub),     // 1
        rrdc (op_mul),     // 2
        rrdc (op_div),     // 3
        rrc (op_cmp),      // 4
        rrd (op_cmplt),    // 5
        rrd (op_cmpeq),    // 6
        rrd (op_cmpgt),    // 7
        rd  (op_inv),      // 8
        rrd (op_and),      // 9
        rrd (op_or),       // a
        rrd (op_xor),      // b
        crrdc (op_addc),   // c
        op_trap,           // d
        handle_xx,         // e
        handle_rx ]        // f
	
// Some instructions load the primary result into rd and the secondary
// into cc (which is R15).  If the d field of the instruction is 15,
// the primary result is loaded into rd and the secondary result is
// discarded.

// It is legal for the destiation register rd to be R0.  However, R0
// always contains 0; i.e. any time it is fetched the result is 0.  In
// effect, any value loaded into R0 is discarded.  Any implementation
// that satisfies these rules is conformant.  Any of the following
// approaches is acceptable: (1) don't load into R0; (2) go ahead and
// load into R0 but produce 0 on readout; (3) don't even implement R0
// with state bits, but ensure that its readout produces 0.

const rx = (f) => (es) => {
    console.log('rx');
    es.instrOpStr = mnemonicRX[ir_b];
    es.instrDisp = memFetchInstr (pc.get());
    adr.put (es.instrDisp);
    es.nextInstrAddr = binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    ea = binAdd (regFile[ir_a].get(), adr.get());
    es.instrEA = ea;
    console.log('rx ea = ' + wordToHex4(ea));
    f();
}

const dispatch_RX =
    [ rx (rx_lea),       // 0
      rx (rx_load),      // 1
      rx (rx_store),     // 2
      rx (rx_jump),      // 3
      rx (rx_jumpc0),    // 4
      rx (rx_jumpc1),    // 5
      rx (rx_jumpf),     // 6
      rx (rx_jumpt),     // 7
      rx (rx_jal),       // 8
      rx (rx_nop),       // 9
      rx (rx_nop),       // a
      rx (rx_nop),       // b
      rx (rx_nop),       // c
      rx (rx_nop),       // d
      rx (rx_nop),       // e
      rx (rx_nop) ];     // f

function rx_lea () {
    console.log('rx_lea');
    regFile[ir_d].put(ea);
}

function rx_load () {
    console.log('rx_load');
    regFile[ir_d].put(memFetchData(ea));
}

function rx_store () {
    console.log('rx_store');
    memStore (ea, regFile[ir_d].get());
}

function rx_jump () {
    console.log('rx_jump');
    es.nextInstrAddr = ea;
    pc.put(es.nextInstrAddr);
}

function rx_jumpc0 () {
    console.log('rx_jumpc0');
    let cc = regFile[15].get();
    if (extractBit (cc,ir_d)===0) {
	es.nextInstrAddr = ea;
	pc.put(es.nextInstrAddr);
    }
}

function rx_jumpc1 () {
    console.log('rx_jumpc1');
    let cc = regFile[15].get();
    if (extractBit (cc,ir_d)===1) {
	es.nextInstrAddr = ea;
	pc.put(es.nextInstrAddr);
    }
}

function rx_jumpf () {
    console.log('rx_jumpf');
    if (! wordToBool (regFile[ir_d].get())) {
	es.nextInstrAddr = ea;
	pc.put (es.nextInstrAddr);
    }
}

function rx_jumpt () {
    console.log('rx_jumpt');
    if (wordToBool (regFile[ir_d].get())) {
	es.nextInstrAddr = ea;
	pc.put (es.nextInstrAddr);
    }
}

function rx_jal () {
    console.log('rx_jal');
    regFile[ir_d].put (pc.get());
    es.nextInstrAddr = ea;
    pc.put (es.nextInstrAddr);
}

function rx_nop () {
    console.log ('rx_nop');
}

// function xx(f) {
const xx = (f) => (es) => {
    console.log('xx');
    es.instrOpStr = mnemonicXX[ir_b];
    es.instrDisp = memFetchInstr (pc.get());
    adr.put (es.instrDisp);
    es.nextInstrAddr = binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    f();
}

//------------------------------------------------------------------------------
// Test pane
//------------------------------------------------------------------------------

// From emulator.js

// In the mem display, the formatting is ok when the container
// specifies the style class.  However, when <pre> ... </pre> are
// added around the text, the font and size are wrong and the
// specified style is ignored.  Perhaps <pre> has an inappropriate
// default style that overrides the existing font.  Solution is to use
// <pre class="HighlightedTextAsHtml"> but don't put it inside a div
// with HighlightedTExtAsHtml


function testpane1() {
    console.log ('testpane 1 clicked')
    let xs = ["<pre class='HighlightedTextAsHtml'>", 'line 1 text',
	      "<span class='CUR'>this is line 2 text</span>",
	      'and finally line 3', '</pre>'];
    console.log ('xs = ' + xs);
    let ys = xs.join('\n');
    console.log ('ys = ' + ys);

    let qs = ys;
    console.log ('qs = ' + qs);
    document.getElementById('TestPaneBody').innerHTML = qs;
}

function testpane2 () {
    console.log ('testpane 2 clicked');
}

function testpane3 () {
    console.log ('testpane 3 clicked');
}
