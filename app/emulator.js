// Sigma16: emulator.js
// Copyright (c) 2019 John T. O'Donnell.  john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. Sigma16/ LICENSE.txt NOTICE.txt

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
// emulator.js interprets machine language programs and displays
// effects in the gui.

//-------------------------------------------------------------------------------

function timerInterrupt() {
    console.log ("Timer Interrupt clicked");
    setBitInRegBE (req, timerBit);
    req.refresh();
}

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
	return `R${es.ir_d},R${es.ir_a},R${es.ir_b}`;
    } else if (es.instrFmtStr==="RX") {
	return `R${es.ir_d},${wordToHex4(es.instrDisp)}[R${es.ir_a}]`;
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
        document.getElementById('ProcAsmListing').innerHTML = "";
	copyExecutableToMemory (es,m);
	es.asmListingPlain = m.asmListingPlain;
	es.asmListingDec = m.asmListingDec;
        es.asmListingCurrent = [];
	for (let i = 0; i < es.asmListingDec.length; i++) { // copy the array
	    es.asmListingCurrent[i] = es.asmListingDec[i];
	}
	initListing (m,es);
	es.nInstructionsExecuted = 0;
	document.getElementById("nInstrExecuted").innerHTML =
	    es.nInstructionsExecuted;
	ioLogBuffer = "";
	refreshIOlogBuffer();
        getListingDims(es);
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

function initListing (m,es) {
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
//    let curline = procAsmListingElt.getElementById('CUR');
//    curline.scrollIntoView();
}


function highlightListingLine (es,i,highlight) {
    es.asmListingCurrent[i] =
        //	"<span class='" + highlight + "'>" + es.asmListingPlain[i] + "</span>";
        "<div class='" + highlight + "'>" + es.asmListingPlain[i] + "</span>";
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

// function clearCtlRegs () {
// }

function procStep(es) {
    console.log ('procStep');
    if (es.procStatus==="Stopped") { setProcStatus (es,"Ready"); }
    if (es.procStatus==="Ready") {
	execInstrPrepareFull (es);
	executeInstruction (es);
        console.log ("procStep: executeInstruction finished");
        execInstrPostDisplay (es);
    }
}

function procRun(es) {
    console.log ("procRun");
    if (es.procStatus==="Stopped") { setProcStatus (es,"Ready"); }
    execRunPrepare (es);
    instructionLooper (es);
    runInstrPostDisplay (es);
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
	console.log ('instructionLooper');
        execInstrPrepareFast (es);
	executeInstruction (es);
// Check for halt or breakpoint
	if (es.procStatus=="Halted") {
	    console.log ("looper: halted");
            displayFullState();
        } else if (es.breakEnabled && pc.get() === es.breakPCvalue) {
	    console.log ("looper: breakpoint");
	    setProcStatus (es,"Break");
            displayFullState();
	} else {
	    setTimeout (function () {instructionLooper(es)});
        }
    }
    console.log ('instructionLooper terminated');
}

//---------------------------------------------------------------------------
// Breakpoint
//---------------------------------------------------------------------------

// Temporary: enter a hex constant e.g. $02c9 into the text area and
// click Refresh.  The emulator will break when the pc reaches this
// value.  Spaces before the constant are not allowed, and the $ is
// required.  Later this will be replaced by a richer language for
// specifying the break condition.

function breakRefresh (es) {
    console.log ("breakRefresh");
    let x = document.getElementById('BreakTextArea').value;
    if (x.search(hexParser) == 0) {
	let w = hex4ToWord (x.slice(1));
	es.breakPCvalue = w;
	console.log (`breakPCvalue = + ${w}`);
    } else {
	console.log (`breakRefresh cannot parse + x`);
    }
}

function breakEnable (es) {
    console.log ("breakEnable");
    es.breakEnabled = true;
    console.log (`breakEnable ${es.breakPCvalue}`);
}

function breakDisable (es) {
    console.log ("breakDisable");
    es.breakEnabled = false;
}

function breakClose () {
    console.log ("breakClose");
    hideBreakDialogue ();
}

//------------------------------------------------------------------------------
// Wrapper around instruction execution
//------------------------------------------------------------------------------

// For single stepping, we want to keep display of registers and
// memory up to date and show access by highlighting the fetched and
// updated locations.  For Run mode, we want to avoid updating the
// memory continuosly, although it may be useful to keep the register
// displays updated.

// The strategy is: (1) for stepping, there is a function to prepare
// before executing an instruction, and another to update the displays
// after execution, with the expectation that the user will spend some
// time looking at the displays before steppign again.  (2) For Run,

// (for running) Prepare the displays before running sequence of
// instructions (the Run button).

function execRunPrepare (es) {
    console.log ("execRunPrepare");
    regClearAccesses ();             // remove register highlighting, clear logs
    regShowAccesses()
    memClearAccesses ();             // remove mem highlighting, clear logs
    memDisplay ();                   // refresh memory display
    clearInstrDecode (es);           // remove decoding of last instruction
    prepareListingBeforeInstr (es);  //remove any instruction highlighting
    setProcAsmListing (es);
}

// (for runing) Prepare to execute an instruction while in Run mode: clear
// the logs but don't keep memory updated.  This assumes that the
// displays have already been put into a suitable state fur the
// duration of the run.

function execInstrPrepareFast (es) {
    console.log ("execInstrPrepareFast");
// don't refresh the registers (no regClearAccesses), just clear logs
    regFetched = [];  // clear reg fetch log
    regStored = [];   // clear reg update log
// don't refresh memory (no memClearAccesses), just clear logs
    memFetchInstrLog = [];
    memFetchDataLog = [];
    memStoreLog = [];
// don't need to clear instrDecode as we aren't updating it in fast mode
// don't need to prepareListing as we aren't updating it in fast mode
}

// (for stepping) Prepare to execute an instruction with full logging:
// clear the logs and update all the displays.

function execInstrPrepareFull (es) {
    console.log ("execInstrPrepareFast");
    regClearAccesses ();
    memClearAccesses ();
    clearInstrDecode (es);
    prepareListingBeforeInstr (es);
}

// (for stepping) Display the effects of the instruction

function execInstrPostDisplay (es) {
    if (es.procStatus=="Halted") { // do a full display
	regShowAccesses()
        refreshRegisters ();
	memShowAccesses();
        memDisplayFull ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    } else { // do normal display
	regShowAccesses()
	memShowAccesses();
	memDisplay ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    }
}

function runInstrPostDisplay (es) {
    console.log("runInstrPostDisplay");
    memClearAccesses ();
    memDisplayFull ();
    clearRegisterHighlighting ();
    refreshRegisters ();
}

// Prepare to execute an instruction by clearing the buffers holiding
// log information.

function prepareExecuteInstruction (es) {
    console.log ("prepareExecuteInstruction");

// Preparations before the instruction
    regClearAccesses ();
    memClearAccesses ();
    memDisplay ();
    clearInstrDecode (es);
    prepareListingBeforeInstr (es);
}

// Execute the instruction
//    executeInstruction (es);

function finalizeExecuteInstruction (es) {
// Final actions after the instruction
    if (es.procStatus=="Halted") {
        console.log ("procStep: execute instruction: halted")
	regShowAccesses()
	memShowAccesses();
	memDisplayFull ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    } else if (es.breakEnabled && pc.get() === es.breakPCvalue) {
	console.log ("Breakpoint");
	setProcStatus (es,"Break");
        displayFullState();
    } else {
	regShowAccesses()
	memShowAccesses();
	memDisplay ();
	showInstrDecode (es);
	highlightListingAfterInstr (es);
    }

    console.log ("runOneInstruction: end");
}

//------------------------------------------------------------------------------
// Machine language semantics
//------------------------------------------------------------------------------

// Execute one instruction and return

// kludge check... remove
//    if ((getBitInReg (statusreg,intEnableBit) ? (mr ?req.get() & mask.get()) : 0) != 0) {
//	while (i<16 && getBitInReg(req,i)==0 && getBitInReg(mask,i)==0) {

// console.log (`interrupt priority search mask=${wordToHex4(mask.get())} req=${wordToHex4(req.get())}`);
//	    console.log(`find interrupt trying i=${i} r=${getBitInReg(req,i)} m=${getBitInReg(mask,i)}`);

function executeInstruction (es) {
    console.log ('executeInstruction');
    es.nInstructionsExecuted++;
    document.getElementById("nInstrExecuted").innerHTML = es.nInstructionsExecuted;

// Check for interrupt
    let mr = mask.get() & req.get();
//    console.log (`interrupt mr = ${wordToHex4(mr)}`);
    if (getBitInRegBE (statusreg,intEnableBit) && mr) {
	let i = 0; // interrupt that is taken
	while (i<16 && getBitInWordBE(mr,i)==0) { i++ };
	console.log (`\n*** Interrupt ${i} ***`);
	ipc.put(pc.get());           // save the pc
	istat.put(statusreg.get());   // save the status register
//	console.log (`ipc=${ipc.get()}`);
//	console.log (`req=${wordToHex4(req.get())}`);
	clearBitInRegBE (req,i);        // clear the interrupt that was taken
//	console.log (`req=${wordToHex4(req.get())}`);
//	console.log (`pc=${wordToHex4(pc.get())}`);
//	console.log (`vect=${wordToHex4(vect.get())} i=${i}`);
	pc.put (vect.get() + 2*i);  // jump to handler
//	console.log (`pc=${wordToHex4(pc.get())}`);
        // Disable interrupts and enter system state
//	console.log (`status=${wordToHex4(statusreg.get())}`);
	statusreg.put (statusreg.get()
		       & maskToClearBitBE(intEnableBit)
		       & maskToClearBitBE(userStateBit));
//	console.log (`statusreg=${wordToHex4(statusreg.get())}`);
//	regShowAccesses();
	return;
    };

// No interrupt, go ahead with instruction
    es.curInstrAddr = pc.get();
    instrCode = memFetchInstr (es.curInstrAddr);
    ir.put (instrCode);
    es.nextInstrAddr = binAdd (es.curInstrAddr, 1);
    pc.put (es.nextInstrAddr);
//    console.log('pc = ' + wordToHex4(pc.get()) + ' ir = ' + wordToHex4(instr));
    let tempinstr = ir.get();
    es.ir_b = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_a = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_d = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.ir_op = tempinstr & 0x000f;
    console.log('instr fields = ' + es.ir_op + ' ' + es.ir_d + ' ' + es.ir_a + ' ' + es.ir_b);
    es.instrFmtStr = "RRR";  // Replace if opcode expands to RX or EXP
    es.instrOpStr = mnemonicRRR[es.ir_op]  // Replace if opcode expands
    dispatch_RRR [es.ir_op] (es);
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


/* Example of a lambda expression and a curried lambda
  const foobar = (x) => (y) => x+y;
  let baz = foobar (5);
  let bar = foobar (41);
  let abc = foobar (7) (50);
*/

// Apply f to a and b, load primary result into d, and load secondary
// result into c (e.g. add)

const rrdc = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let [primary, secondary] = f (a,b);
    regFile[es.ir_d].put(primary);
    if (es.ir_d<15) { regFile[15].put(secondary) }
}

// Apply f to a and load primary result into d (e.g. inv)

const rd = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let primary = f (a);
    regFile[es.ir_d].put(primary);
}

// Apply f to a and b, and load primary result into d (e.g. cmplt)

const rrd = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let primary = f (a,b);
    regFile[es.ir_d].put(primary);
}

// Apply f to a and b, and load primary result into c (e.g. cmp)

const rrc = (f) => (es) => {
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let cc = f (a,b);
    console.log (`rrc cc=${cc}`);
    regFile[15].put(cc);
}

// Apply f to c, a and b, load primary result into d, and load
// secondary result into c (e.g. addc)

const crrdc = (f) => (es) => {
    let c = regFile[15].get();
    let a = regFile[es.ir_a].get();
    let b = regFile[es.ir_b].get();
    let [primary, secondary] = f (c,a,b);
    regFile[es.ir_d].put(primary);
    if (es.ir_d<15) { regFile[15].put(secondary) }
}

const op_trap = (es) => {
    let code = regFile[es.ir_d].get();
    console.log (`trap code=${code}`);
    if (code===0) { // Halt
	console.log ("Trap: halt");
	setProcStatus (es,"Halted");
//        displayFullState();
//        console.log ("Trap: displayed full state after halt");
    } else if (code==1) { // Read
        trapRead(es);
    } else if (code==2) { // Write
        trapWrite(es);
    } else { // Undefined trap is nop
        console.log (`trap with undefined code=${code}`)
    }
}

// trapRead performs in input from the contents of the input buffer: a
// = address of the buffer, and b = size of buffer.  If the number of
// characters entered by the user into the input buffer exceeds b,
// then only b characters are stored into the buffer, and thse b
// characters are removed from the input buffer.  The user types input
// into the IOinputBuffer, and the input operation obtains up to b
// characters, stores them into memory starting from location a, and
// removes those characters from IOinputBuffer.  Two registers are
// updated: Ra := address just after last word stored, Rb := numbr of
// characters (words) read.

function trapRead (es) {
    let inputElt = document.getElementById("IOinputBuffer")
    let xs = inputElt.value;
    let a = regFile[es.ir_a].get(); // buffer address
    let b = regFile[es.ir_b].get(); // buffer size
    let n = xs.length; // number of chars available in buffer
    let m = n <= b ? n : b; // number of chars actually input
    let xs2 = xs.substring (m,n); // excess chars from input window are not used
    let ys = xs.substring (0,m);  // input string to store into memory
    let charcode = 0;
    document.getElementById("IOinputBuffer").value = xs2;
    console.log (`Read: a=${a} b=${b} m=${m} >>> /${ys}/`);
    console.log (`Read: n=${n} m=${m}`);
    console.log (`Read: xs2=/${xs2}/ ys=/${ys}/`);
    for (let i = 0; i<m; i++) {
	charcode = ys.charCodeAt(i);
	memStore (a, charcode);
	console.log (`Read: mem[${a}] := ${charcode}`);
	a++;
    }
    regFile[es.ir_a].put(a); // just after last address stored
    regFile[es.ir_b].put(m); // number of chars actually input
    ioLogBuffer += highlightField(ys,"READ");   // display chars that were read
    refreshIOlogBuffer ();
    inputElt.value = xs2; // leave unread characters in input buffer
}

// Write b characters starting from address a
function trapWrite (es) {
    let a = regFile[es.ir_a].get(); // buffer address
    let b = regFile[es.ir_b].get(); // buffer size
    let xs = "";
    for (let i = 0; i<b; i++) {
	xs += String.fromCharCode(memFetchData(a));
	a++
    }
    console.log (`Write a=${a} b=${b} >>> /${xs}/`);
    ioLogBuffer += xs;
    console.log (ioLogBuffer);
    refreshIOlogBuffer();
}

const handle_rx = (es) => {
    console.log ('handle rx' + es.ir_b);
    es.instrFmtStr = "RX";
    dispatch_RX [es.ir_b] (es);
}

const handle_EXP = (es) => {
    console.log (`handle_EXP ${es.ir_d} ${es.ir_a} ${es.ir_b}`);
    es.instrFmtStr = "EXP";
    let code = 16*es.ir_a + es.ir_b;
    if (code < limitEXPcode) {
	console.log (`handle_EXP dispatch code=${code} ${es.ir_d} ${es.ir_a} ${es.ir_b}`);
	dispatch_EXP [code] (es);
    } else {
	console.log (`EXP bad code ${wordToHex4(code)}`);
    }
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
        handle_EXP,         // e
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
    es.instrOpStr = mnemonicRX[es.ir_b];
    es.instrDisp = memFetchInstr (pc.get());
    adr.put (es.instrDisp);
    es.nextInstrAddr = binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    //    es.ea = binAdd (regFile[es.ir_a].get(), adr.get());
    es.ea = binAdd (regFile[es.ir_a].get(), es.instrDisp);
    es.instrEA = es.ea;
    console.log (`rx ea, disp=${wordToHex4(es.instrDisp)}`);
    console.log (`rx ea, idx=${wordToHex4(regFile[es.ir_a].get())}`);
    console.log('rx ea = ' + wordToHex4(es.ea));
    f (es);
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

function rx_lea (es) {
    console.log('rx_lea');
    regFile[es.ir_d].put(es.ea);
}

function rx_load (es) {
    console.log('rx_load');
    regFile[es.ir_d].put(memFetchData(es.ea));
}

function rx_store (es) {
    console.log('rx_store');
    memStore (es.ea, regFile[es.ir_d].get());
}

function rx_jump (es) {
    console.log('rx_jump');
    es.nextInstrAddr = es.ea;
    pc.put(es.nextInstrAddr);
}

function rx_jumpc0 (es) {
    console.log('rx_jumpc0');
    let cc = regFile[15].get();
    if (extractBitBE (cc,es.ir_d)===0) {
	es.nextInstrAddr = es.ea;
	pc.put(es.nextInstrAddr);
    }
}

function rx_jumpc1 (es) {
    console.log('rx_jumpc1');
    let cc = regFile[15].get();
    if (extractBitBE (cc,es.ir_d)===1) {
	es.nextInstrAddr = es.ea;
	pc.put(es.nextInstrAddr);
    }
}

function rx_jumpf (es) {
    console.log('rx_jumpf');
    if (! wordToBool (regFile[es.ir_d].get())) {
	es.nextInstrAddr = es.ea;
	pc.put (es.nextInstrAddr);
    }
}

function rx_jumpt (es) {
    console.log('rx_jumpt');
    if (wordToBool (regFile[es.ir_d].get())) {
	es.nextInstrAddr = es.ea;
	pc.put (es.nextInstrAddr);
    }
}

function rx_jal (es) {
    console.log('rx_jal');
    regFile[es.ir_d].put (pc.get());
    es.nextInstrAddr = es.ea;
    pc.put (es.nextInstrAddr);
}

function rx_nop (es) {
    console.log ('rx_nop');
}

// EXP format


function exp1_nop (es) {
    console.log ('exp1_nop');
}

function exp1_rfi (es) {
    console.log ('exp1_rfi');
    statusreg.put (istat.get());
    pc.put (ipc.get());
}

function exp2_save (es) {
    console.log ('exp2_save');
    sr_looper ((a,r) => memStore (a, regFile[r].get()),
               regFile[es.ir_d].get()+es.field_gh, es.field_e, es.field_f)
}

function exp2_restore (es) {
    console.log ('exp2_restore');
    sr_looper ((a,r) => regFile[r].put(memFetchData(a)),
               regFile[es.ir_d].get()+es.field_gh, es.field_e, es.field_f)
}

function sr_looper (f,addr,first,last) {
    let done = false;
    let r = first;
    while (!done) {
        console.log (`save looper addr=${addr} r=${r}`);
        f(addr,r);
        done = r==last;
        addr += 1;
        r = bininc4(r);
    }
}

function bininc4 (x) { return x >= 15 ? 0 : x+1 }


// temp like put
function exp2_getctl (es) {
    console.log ('exp2_getctl');
    let cregn = es.field_f;
    let cregidx = cregn + ctlRegIndexOffset; // init in gui.js
    console.log (`exp_getctl cregn=${cregn} cregidx=${cregidx}`);
    regFile[es.field_e].put(register[cregidx].get());
}
function exp2_putctl (es) {
    console.log ('exp2_putctl');
    let cregn = es.field_f;
    let cregidx = cregn + ctlRegIndexOffset; // init in gui.js
    console.log (`exp_getctl cregn=${cregn} cregidx=${cregidx}`);
    console.log (`es.ir_d=${es.ir_d}`);
    register[cregidx].put(regFile[es.field_e].get());
    register[cregidx].refresh();
}

function exp2_execute (es) {
    console.log ("exp2_execute");
}

function exp2_push (es) {
    console.log ('exp2_push');
 //   console.log (`e=${es.field_e} f=${es.field_f} g=${es.field_g} h=${es.field_h} `);
    let top = regFile[es.field_f].get();
    let last = regFile[es.field_g].get();
//    console.log (`push: top=${top} last=${last}`);
    if (top < last) {
        top += 1;
        memStore (top, regFile[es.field_e].get());
        regFile[es.field_f].put(top);
    } else {
        console.log ("push: stack overflow")
    }
}

function exp2_pop (es) {
    console.log ('exp2_pop');
    let top = regFile[es.field_f].get();
    let first = regFile[es.field_g].get();
    if (top >= first) {
        regFile[es.field_e].put(memFetchData(top));
        top -= 1;
        regFile[es.field_f].put(top);
    } else {
        console.log ("pop: stack underflow")
    }
}

function exp2_top (es) {
    console.log ('exp2_top');
    let top = regFile[es.field_f].get();
    let first = regFile[es.field_g].get();
    if (top >= first) {
        regFile[es.field_e].put(memFetchData(top));
        regFile[es.field_f].put(top);
    } else {
        console.log ("top: stack underflow")
    }
}

function exp2_shiftl (es) {
    console.log ("exp2_shiftl");
    let x = regFile[es.field_e].get();
    let k = es.field_g;
    let result = x << k;  // logical shift
    console.log (`shiftl ${wordToHex4(x)} left by ${k} bits => ${wordToHex4(result)}`);
    regFile[es.ir_d].put(result);
}

function exp2_shiftr (es) {
    console.log ("exp2_shiftr");
    let x = regFile[es.field_e].get();
    let k = es.field_g;
    let result = x >>> k;  // logical shift
    console.log (`shiftr ${wordToHex4(x)} right by ${k} bits => ${wordToHex4(result)}`);
    regFile[es.ir_d].put(result);
}

// Instruction fields for extract and extractii:
//   d = destination
//   e = operand
//   (g,h) = field

function exp2_extract (es) {
    console.log ('exp2_extract');
    let x = regFile[es.field_e].get();
    let i = es.field_g;
    let j = es.field_h;
    let a = (x << i) & 0xffff;
    let b = a >> (15 - (j-i));
    regFile[es.ir_d].put(b);
}

function exp2_extracti (es) {
    console.log ('exp2_extracti');
    let x = wordInvert (regFile[es.field_e].get());
    let i = es.field_g;
    let j = es.field_h;
    let a = (x << i) & 0xffff;
    let b = a >> (15 - (j-i));
    regFile[es.ir_d].put(b);
}

function foobar (g,h) {
    let x = 0x0000;  // operand
    let i = g;  // field start, there will be this many 0s to left of field
    let j = h;  // field end
    let jjj = 15 - j; // there will be this many 0s to right of field
    let a = 0xffff;
    let b = (a << jjj) & 0xffff; // get jjj 0s on right side
    let c = b >> i; // get i 0s on left side
    console.log (`x=${x} i=${i} j=${j} a=${wordToHex4(a)}`);
    console.log (`a=${wordToHex4(a)} b=${wordToHex4(b)} c=${wordToHex4(c)}`);
    return 0
}

// Instruction fields for inject and injecti:
//   d = destination
//   e = operand
//   (g,h) = field

function exp2_inject(es){}

function exp2_inject (es) {
    console.log ('exp2_inject');
    let e = regFile[es.field_e].get(); // inject into this word
    let f = regFile[es.field_f].get(); // word contains the field to be injected
    let g = es.field_g;  // start bit index of field
    let h = es.field_h;  // end bit index of field
    let fieldsize = h-g+1;
    let shrdist = 15-h+g; // shift ffff right to get right-adjusted field
    let shldist = 15-h;   // shift left to put field into position
    let radjustedField = 0xffff >>> shrdist;
    let x = f & radjustedField; // value to be injected
    let field = radjustedField << shldist; // 1s in the field
    let fieldInv = (~field) & 0xffff; // mask to clear field in e
    let result = (e & fieldInv) | (x << shldist) ;
    regFile[es.ir_d].put(result);
}

//    console.log (`inject e=${wordToHex4(e)}`);
//    console.log (`inject f=${wordToHex4(f)}`);
//    console.log (`inject g=${g} h=${h}`);
//    console.log (`inject fieldsize=${fieldsize}`);
//    console.log (`inject shrdist=${shrdist} shldist=${shldist}`);
//    console.log (`inject field=${wordToHex4(field)}`);
//    console.log (`inject fieldInv=${wordToHex4(fieldInv)}`);
//    console.log (`inject result=${wordToHex4(result)}`);

function exp2_injecti (es) {
    console.log ('exp2_injecti');
    let e = regFile[es.field_e].get(); // inject into this word
    let f = regFile[es.field_f].get(); // word contains the field to be injected
    let g = es.field_g;  // start bit index of field
    let h = es.field_h;  // end bit index of field
    let fieldsize = h-g+1;
    let shrdist = 15-h+g; // shift ffff right to get right-adjusted field
    let shldist = 15-h;   // shift left to put field into position
    let radjustedField = 0xffff >>> shrdist;
    let x = (~f) & radjustedField; // value to be injected
    let field = radjustedField << shldist; // 1s in the field
    let fieldInv = (~field) & 0xffff; // mask to clear field in e
    let result = (e & fieldInv) | (x << shldist) ;
    regFile[es.ir_d].put(result);
}

function exp2_logicw (es) {
    console.log ('exp2_logicw');
    let x = regFile[es.field_e].get(); // operand 1
    let y = regFile[es.field_f].get(); // operand 2
    let fcn = es.field_g; // logic function
    let result = applyLogicFcnWord (fcn,x,y); // fcn x y
    regFile[es.ir_d].put(result);
}


function exp2_logicb (es) {
    console.log (`exp2_logicb`);
    let xw = regFile[es.field_e].get(); // operand 1
    let yw = regFile[es.field_f].get(); // operand 2
    let i = es.field_h; // bit index
    let x = getBitInWordBE(xw,i);
    let y = getBitInWordBE(yw,i);
    let fcn = es.field_g; // logic function
    let bresult = applyLogicFcnBit (fcn,x,y); // bit result
    let w = putBitInWord (regFile[es.ir_d].get(), i, bresult); // word result
    regFile[es.ir_d].put(w);
}


// EXP format instructions that use only one word
const exp1 = (f) => (es) => {
    let expCode = 16*es.ir_a + es.ir_b;
    es.instrOpStr = mnemonicEXP[expCode];
    es.instrDisp = memFetchInstr (pc.get());
    es.nextInstrAddr = binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    console.log(`EXPF code=${expCode} d=${es.ir_d} es.instrDisp=${wordToHex4(es.instrDisp)}`);
    f(es);
}

// EXP format instructions that require a second word
const exp2 = (f) => (es) => {
    console.log (`exp2 format instruction`);
    let expCode = 16*es.ir_a + es.ir_b;
    es.instrOpStr = mnemonicEXP[expCode];
    es.instrDisp = memFetchInstr (pc.get());
    adr.put (es.instrDisp);
    es.nextInstrAddr = binAdd (es.nextInstrAddr, 1);
    pc.put (es.nextInstrAddr);
    let tempinstr = es.instrDisp;
    es.field_gh = tempinstr & 0x00ff;
    es.field_h = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_g = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_f = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    es.field_e = tempinstr & 0x000f;
    console.log(`EXPF code=${expCode} d=${es.ir_d} es.instrDisp=${wordToHex4(es.instrDisp)}`);
    f(es);
}


const dispatch_EXP =
      [ exp1 (exp1_rfi),       // 0
        exp1 (exp1_nop),       // 1
        exp1 (exp1_nop),       // 2
        exp1 (exp1_nop),       // 3
        exp1 (exp1_nop),       // 4
        exp1 (exp1_nop),       // 5
        exp1 (exp1_nop),       // 6
        exp1 (exp1_nop),       // 7
        
        exp2 (exp2_save),      // 8
        exp2 (exp2_restore),   // 9
        exp2 (exp2_getctl),    // 10
        exp2 (exp2_putctl),    // 11
        exp2 (exp2_execute),   // 12
        exp2 (exp2_push),      // 13
        exp2 (exp2_pop),       // 14
        exp2 (exp2_top),       // 15

        exp2 (exp2_shiftl),    // 16
        exp2 (exp2_shiftr),    // 17
        exp2 (exp2_extract),   // 18
        exp2 (exp2_extracti),  // 19
        exp2 (exp2_inject),    // 20
        exp2 (exp2_injecti),   // 21
        exp2 (exp2_logicw),    // 22
        exp2 (exp2_logicb)     // 23
    ]
const limitEXPcode = dispatch_EXP.length;  // any code above this is nop


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
