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

    curInstrLineNo = curAsmap[curInstrAddr] + 1; // +1 because 1st line is <pre>
    console.log ('  curInstrLineNo = ' + curInstrLineNo);
    if (curInstrLineNo) {
	saveCurSrcLine = srcLine[curInstrLineNo];
	highlightListingLine (curInstrLineNo, "CUR");
    }

    nextInstrLineNo = curAsmap[nextInstrAddr] + 1; // +1 because 1st line is <pre>
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

function highlightListingFull () {
    console.log ('highlightListingFull');
//    let xs = "<pre><code class='HighlightedTextAsHtml'>"
//	+ srcLine.join('\n')
//	+ "</code></pre>";
//    setProcAsmListing (xs);
    setProcAsmListing ();
    let scrollOffset = curInstrLineNo * pxPerChar;
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
//    console.log(' procAsmListing.innerHTML = xs = ' + xs);
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
    console.log('pc = ' + intToHex4(pc.get()) + ' ir = ' + intToHex4(instr));
    let tempinstr = ir.get();
    ir_b = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_a = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_d = tempinstr & 0x000f;
    tempinstr = tempinstr >>> 4;
    ir_op = tempinstr & 0x000f;
    console.log('instr fields = ' + ir_op + ' ' + ir_d + ' ' + ir_a + ' ' + ir_b);

    opDispatch [ir_op] ();
    
    regShowAccesses()
    memShowAccesses();
    memDisplay ();
}

var opDispatch =
    [function () {rrr(rrr_add)},
     function () {rrr(rrr_sub)},
     function () {rrr(rrr_mul)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {rrr(rrr_div)},
     function () {xx()},
     function () {handle_rx()} ]


function rrr (op) {
    console.log ('rrr');
    let a = regFile[ir_a].get();
    let b = regFile[ir_b].get();
    let cc = regFile[15].get();
    let [ primary, secondary ] = op (a,b,cc);
    console.log ('rrr primary = ' + primary + ' secondary = ' + secondary);
    regFile[ir_d].put(primary);
    if (ir_d<15) { regFile[15].put(secondary) }
}


function rrr_sub () {
    console.log ('rrr_sub');
}

function rrr_mul () {
    console.log ('rrr_mul');
}

function rrr_div () {
    console.log ('rrr_div');
}

function xx () {
    console.log ('xx');
}

function handle_rx () {
    console.log ('handle rx' + ir_b);
    rxDispatch[ir_b]();
}


var rxDispatch =
    [function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)},
     function () {rx(rx_lea)}]

function rx(f) {
    console.log('rx');
    adr.put (memFetchInstr (pc.get()));
    nextInstrAddr = binAdd (nextInstrAddr, 1);
    pc.put (nextInstrAddr);
    ea = binAdd (regFile[ir_a].get(), adr.get());
    console.log('rx ea = ' + intToHex4(ea));
    f();
}

function rx_lea () {
    console.log('rx_lea');
    regFile[ir_d].put(ea);
}
