// Emulator

// Find solution for problem of displaying highlighted text as html

// in mem display, the size and font is ok but when <pre> </pre> is
// added around the text, then the font and size are wrong.  Perhaps <pre>
// has a default style I don't like, and it overrides the existing font?

// use <pre class="HighlightedTextAsHtml"> but don't put it inside a
// div with HighlightedTExtAsHtml

var procAsmListingElt;  // set during onLoad event

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



//-----------------------------------------------------------------------
// boot

// Boot from either the current module (without linking) or the load
// module (if the linker has been run).


// Attempt to copy an executable into memory for execution.  A module
// is executable if it contains object code but doesn't have any
// imports; in this case isExecutable is true.  The object code can
// come from either the assembler, or it can be read in as an object
// file.  The presence of an assembly listing and asmap is optional.

function boot () {
    console.log ('boot');
    let m = getCurrentModule ();
    exMod = m;
    if (m.isExecutable) {
	console.log ('Current module is executable: booting');
	resetRegisters ();
	memClearAccesses();
	copyExecutableToMemory (m);
	initListing ();
    } else {
	console.log ('cannot boot')
    }
}

// Copy a module's object code into memory.  Should use objectCode
// rather than codeWord, but objectCode will need to support org.  So
// far it's just a list of words.  This version doesn't yet support
// org and it requires the module to be assembled (rather than read
// in).

function copyExecutableToMemory (m) {
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
    curAsmap = m.asmap;
}

function parseCopyObjectModuleToMemory () {
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
	linkerBootLine(i, xs[i]);
//	experiment(xs[i]);
    }
}

// Should check the operation, implement org, and provide suitable
// error messages, but that's for later.  For now, just assume it is
// hexdata with valid argument

function linkerBootLine (m,i,x) {
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



// ------------------------------------------------------------------
// Highlighting current and next instruction

// Global variables for handling listing display as program runs

// Uses global variables set by linker: exMod is executing module and
// curAsmap is map from addresses to source lines

var srcLine;        // copy of source statements

// -1 indicates no line has been highlighted
var curInstrAddr, curInstrLineNo, saveCurSrcLine;
var nextInstrAddr, nextInstrLineNo, saveNextSrcLine;

// Initialize the running listing display

function initListing () {
    curInstrAddr = 0;
    nextInstrAddr = 0;
    curInstrLineNo = -1;  // -1 indicates no line has been highlighted
    nextInstrLineNo = -1;
    saveCurSrcLine = "";
    saveNextSrcLine = "";
    srcLine = [];
//    srcLine.push("<pre class='HighlightedTextAsHtml'>");
    for (let i = 0; i < exMod.asmStmt.length; i++) {
	srcLine.push(exMod.asmStmt[i].srcLine);
    }
//    srcLine.push("</pre>");
    //    setProcAsmListing (srcLine.join('\n'));
    setProcAsmListing ();
}

// Prepare the running listing before starting instruction: remove any
// existing highlighting of current and next instruction

function prepareListingBeforeInstr () {
    console.log ('prepareListingBeforeInstr');
    if (curInstrLineNo >= 0) { srcLine[curInstrLineNo] = saveCurSrcLine };
    if (nextInstrLineNo >= 0) {	srcLine[nextInstrLineNo] = saveNextSrcLine };
    curInstrLineNo = -1;
    nextInstrLineNo = -1;
}

// As it executes an instruction, the emulator sets curInstrAddr and
// nextInstrAddr.  These two simple assignments are the only overhead
// during the interpretation of an instruction.

// After the instruction has finished, highlight the current and next
// instructions in the listing

function highlightListingAfterInstr () {
    console.log ('highlightListingAfterInstr');
    console.log ('  curInstrAddr = ' + curInstrAddr);
    console.log ('  nextInstrAddr = ' + nextInstrAddr);

    curInstrLineNo = curAsmap[curInstrAddr];
    console.log ('  curInstrLineNo = ' + curInstrLineNo);
    if (curInstrLineNo) {
	saveCurSrcLine = srcLine[curInstrLineNo];
	highlightListingLine (curInstrLineNo, "CUR");
    }

    nextInstrLineNo = curAsmap[nextInstrAddr];
    console.log ('  nextInstrLineNo = ' + nextInstrLineNo);
    if (nextInstrLineNo) {
	saveNextSrcLine = srcLine[nextInstrLineNo];
	highlightListingLine (nextInstrLineNo, "NEXT");
    }

    if (memDisplayModeFull) {
	highlightListingFull ()
    } else {
	highlightListingFull ()    // temp ?????
    }
}

const asmScrollOffsetAbove = 5;  // how many lines above scroll target

function highlightListingFull () {
    console.log ('highlightListingFull');
//    let xs = "<pre><code class='HighlightedTextAsHtml'>"
//	+ srcLine.join('\n')
//	+ "</code></pre>";
//    setProcAsmListing (xs);
    setProcAsmListing ();
    let xa = curInstrLineNo - asmScrollOffsetAbove;
    xa = xa < 0 ? 0 : xa;
    let scrollOffset = xa * pxPerChar;
    console.log ('curInstrLineNo = ' + curInstrLineNo
		 + '  scrollOffset = ' + scrollOffset);
    procAsmListingElt.scroll (0, scrollOffset);
    console.log ('   NOTE srcLine[0] = ' + srcLine[0]);
}

function highlightListingLine (i,highlight) {
    let xs = srcLine[i];
    srcLine[i] = "<span class='" + highlight + "'>" + xs + "</span>";
    console.log ('highlightListingLine : ' + srcLine[i]);
}

function setProcAsmListing () {
//    console.log ('setProcAsmListing');
// scrolling doesn't work if it just uses <pre> but not <code>
    let xs = "<pre><code class='HighlightedTextAsHtml'>"
	+ srcLine.join('\n')
	+ "</code></pre>";
    document.getElementById('ProcAsmListing').innerHTML = xs;
}

// Given address a, the corresponding source statement is
//   i = curAsmap[a]
//   if i=null then no source is avaiable
//     else currentModule.asmStmt[i].srcLine

// ------------------------------------------------------------------


var instr = 0;
var ir_op = 0, ir_d = 0, ir_a = 0, ir_b = 0;  // instruction fields
var ea = 0;  // effective address


function clearCtlRegs () {
}

// deprecated use boot from linker instead
// function procBoot () {
//     console.log('procBoot');
//     bootCurrentModule ();
// }

function procStep () {
    console.log ('procStep');
    executeInstruction();
    highlightListingAfterInstr ();
}

// memClearAccesses, memShowAccesses, and memDisplay are very slow...

function executeInstruction () {
    console.log ('executeInstruction');
    prepareListingBeforeInstr ();
    memClearAccesses ();
    memDisplay ();
    regClearAccesses ();

    curInstrAddr = pc.get();
    ir.put (memFetchInstr (curInstrAddr));
    nextInstrAddr = binAdd (curInstrAddr, 1);
    pc.put (nextInstrAddr);
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

    dispatch_RRR [ir_op] ();
    
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

var dispatch_RRR =
    [function () {rrdc (op_add)},     // 0
     function () {rrdc (op_sub)},     // 1
     function () {rrdc (op_mul)},     // 2
     function () {rrdc (op_div)},     // 3
     function () {rrc (op_cmp)},      // 4
     function () {rrd (op_cmplt)},    // 5
     function () {rrd (op_cmpeq)},    // 6
     function () {rrd (op_cmpgt)},    // 7
     function () {rd (op_inv)},       // 8
     function () {rrd (op_and)},      // 9
     function () {rrd (op_or)},       // a
     function () {rrd (op_xor)},      // b
     function () {crrdc (op_addc)},   // c
     function () {op_trap ()},        // d
     function () {handle_xx ()},      // e
     function () {handle_rx ()} ]     // f

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

// Apply f to a and load primary result into d (e.g. inv)

function rd (f) {
    let a = regFile[ir_a].get();
    let primary = f (a);
    regFile[ir_d].put(primary);
}

// Apply f to a and b, and load primary result into d (e.g. cmplt)

function rrd (f) {
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let primary = f (a,b);
    regFile[ir_d].put(primary);
}

// Apply f to a and b, and load primary result into c (e.g. cmp)

function rrc (f) {
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let primary = f (a,b);
    regFile[15].put(primary);
}

// Apply f to a and b, load primary result into d, and load secondary
// result into c (e.g. add)

function rrdc (f) {
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let [primary, secondary] = f (a,b);
    regFile[ir_d].put(primary);
    if (ir_d<15) { regFile[15].put(secondary) }
}

// Apply f to c, a and b, load primary result into d, and load
// secondary result into c (e.g. addc)

function op_crrdc (f) {
    let c = regFile[15].get();
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let [primary, secondary] = f (c,a,b);
    regFile[ir_d].put(primary);
    if (ir_d<15) { regFile[15].put(secondary) }
}

function op_trap () {
    let d = regFile[ir_d].get();
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    console.log (`trap ${d} ${a} ${b}`);
}

function handle_xx () {
    console.log ('xx');
}

function handle_rx () {
    console.log ('handle rx' + ir_b);
    rxDispatch[ir_b]();
}

var rxDispatch =
    [function () {rx(rx_lea)},       // 0
     function () {rx(rx_load)},      // 1
     function () {rx(rx_store)},     // 2
     function () {rx(rx_jump)},      // 3
     function () {rx(rx_jumpc0)},    // 4
     function () {rx(rx_jumpc1)},    // 5
     function () {rx(rx_jumpf)},     // 6
     function () {rx(rx_jumpt)},     // 7
     function () {rx(rx_jal)},       // 8
     function () {rx(rx_nop)},       // 0
     function () {rx(rx_nop)},       // 0
     function () {rx(rx_nop)},       // 0
     function () {rx(rx_nop)},       // 0
     function () {rx(rx_nop)},       // 0
     function () {rx(rx_nop)},       // 0
     function () {rx(rx_nop)}];       // 0

function rx(f) {
    console.log('rx');
    adr.put (memFetchInstr (pc.get()));
    nextInstrAddr = binAdd (nextInstrAddr, 1);
    pc.put (nextInstrAddr);
    ea = binAdd (regFile[ir_a].get(), adr.get());
    console.log('rx ea = ' + wordToHex4(ea));
    f();
}

function rx_nop () {
    console.log ('rx_nop');
}

function rx_lea () {
    console.log('rx_lea');
    regFile[ir_d].put(ea);
}

function rx_load () {
    console.log('rx_load');
    regFile[ir_d].put (memFetchData(ea));
}

function rx_store () {
    console.log('rx_store');
    memStore (ea, regFile[ir_d].get());
}

function rx_jump () {
    console.log('rx_jump');
    pc.put (ea);
}

function rx_jumpc0 () {
    console.log('rx_jumpc0');
    if (true) {
	pc.put (ea);
    }
}

function rx_jumpc1 () {
    console.log('rx_jumpc1');
    if (true) {
	pc.put (ea);
    }
}

function rx_jumpf () {
    console.log('rx_jumpf');
    if (! wordToBool (regFile[ir_d].get())) {
	pc.put (ea);
    }
}

function rx_jumpt () {
    console.log('rx_jumpt');
    if (wordToBool (regFile[ir_d].get())) {
	pc.put (ea);
    }
}

function rx_jal () {
    console.log('rx_jal');
    regFile[ir_d].put (pc.get());
    pc.put (ea);
}
