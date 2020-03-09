// Sigma16: state.js
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
// state.js defines global state for the system, IDE, modules, and
// emulator
//-------------------------------------------------------------------------------

// The main components of the program avoid using global variables;
// instead the necessary state is organized into records and passed as
// needed to the functions.  This module defines those records.  The
// gui operations, defined in Sigma16.html, call the emulator's
// interface functios with the state, for example boot(emulatorState).


let highlightedRegisters = [];

// Update the display of all registers and memory (all of memory)
function displayFullState () {
    console.log ('displayFullState');
    clearRegisterHighlighting ();
    refreshRegisters ();
    memClearAccesses ();
    memDisplayFull ();
}

//------------------------------------------------------------------------------
// Emulator state
//------------------------------------------------------------------------------

// This is the global emulator state.  The functions in the emulator
// don't use it directly, in order to avoid excessive use of globals.
// The current emulator state is passed as needed to functions (the
// convention is that the parameter name is 'es').

let emulatorState =
    {
// Processor
	procStatus : "Reset",
	nInstructionsExecuted : 0,
	instrLooperDelay : 1000,
	instrLooperShow : false,
	breakEnabled : false,
	breakPCvalue : 0,
	// Instruction being executed
	doInterrupt   : 0,
	ir_op         : 0,
	ir_d          : 0,
	ir_a          : 0,
	ir_b          : 0,
        ea            : 0,
	instrDisp     : 0,
        field_e       : 0,
        field_f       : 0,
        field_g       : 0,
        field_h       : 0,
        field_gh       : 0,
	instrOpCode   : null,
	instrCodeStr  : "",
	instrFmtStr   : "",
	instrOp       : "",
	instrArgs     : "",
	instrEA       : null,
	instrEAStr    : "",
	instrEffect   : [],
	// Tracking source lines corresponding to current instruction
	curAsmap : [],
	asmListingPlain    : [], // plain listing shows address, code, source
	asmListingDec      : [], // decorated listing uses highlighting for fields
	asmListingCurrent  : [], // version of listing displayed in emulator pane
        asmListingHeight   : 0,   // height in pixels of the listing
	curInstrAddr       : 0,
	nextInstrAddr      : 0,
	curInstrLineNo     : -1,  // -1 indicates no line has been highlighted
	nextInstrLineNo    : -1,
	saveCurSrcLine     : "",
	saveNextSrcLine    : "",
//	srcLinePlain       : [],
//	srcLineHighlightedFields : []
    }



// Global variables for instruction decode; used in emulator

let displayInstrDecode = true;
let instrCode, instrDisp, instrCodeStr; // record the values
let instrFmtStr = "";
let instrOpStr = "";
let instrArgsStr = "";
let instrEA, instrEAStr;
let instrEffect = [];


//---------------------------------------------------------------------------
// State variables

// let st =
//     { ir_op : 0,
//       ir_d : 0,
//       ir_a : 0,
//       ir_b : 0
//    }

// Is it better to keep registers, memory, etc, as globals?  Or to
// make them fields in a state record?  Changed ir_d to st.ir_d in
// state.js and emulator.js.  Reconsider which approach is better, and
// make it consistent. ?????


// Processor elements: html elements for displaying instruction decode

let instrCodeElt;
let instrFmtElt;
let instrOpElt;
let instrArgsElt;
let instrEAElt;
let instrCCElt;
let instrEffect1Elt;
let instrEffect2Elt;

function initializeProcessorElements () {
    console.log ('initializeProcessorElements');
    instrCodeElt = document.getElementById("InstrCode");
    instrFmtElt  = document.getElementById("InstrFmt");
    instrOpElt   = document.getElementById("InstrOp");
    instrArgsElt = document.getElementById("InstrArgs");
    instrEAElt   = document.getElementById("InstrEA");
    instrCCElt   = document.getElementById("InstrCC");
    instrEffect1Elt = document.getElementById("InstrEffect1");
    instrEffect2Elt = document.getElementById("InstrEffect2");
}


// Fields of the current instruction

var instr = 0;
var ir_op = 0, ir_d = 0, ir_a = 0, ir_b = 0;  // instruction fields

// The value of the effective addresss pecified by an RX instruction

var ea = 0;  // effective address

// Global variables for handling listing display as program runs.
// Uses global variables set by linker: exMod is executing module and
// curAsmap is map from addresses to source lines

var srcLine;        // copy of source statements


// Keep track of the address of the currently executing instruction,
// the address of the next instruction, and the line numbers where
// these instructions appear in the assembly listing.  -1 indicates no
// line has been highlighted

var curInstrAddr, curInstrLineNo, saveCurSrcLine;
var nextInstrAddr, nextInstrLineNo, saveNextSrcLine;


function initializeSubsystems () {
    memDisplayModeFull = false;
    document.getElementById('FullDisplayToggleButton').value = "Fast display";
}

function toggleFullDisplay () {
    console.log ('toggleFullDisplay clicked');
    memDisplayModeFull = ! memDisplayModeFull;
    document.getElementById('FullDisplayToggleButton').value =
	memDisplayModeFull ? "Full display" : "Fast display";
    if (memDisplayModeFull) { memDisplayFull () }
    else { memDisplayFast ()
	 }  // loses info but makes tab switching faster
}

//----------------------------------------------------------------------
//  Registers
//----------------------------------------------------------------------

// The registers are defined as global variables.  These variables are
// declared here but their values are set only when window has been
// loaded, because the gui elements will exist at that time.

var modeHighlightAccess = true;  // for tracing: highlight reg/mem that is accessed


// Define the control registers as global variables
var pc, ir, adr, dat, sysStat;                 // instruction control
var enable, mask, req, isys, ipc, handle;           // interrupt control
var sEnable, sProg, sProgEnd, sDat, sDatEnd;   // segmentation control
var ctlRegIndexOffset = 0;  // update in gui.js when registers are created

var regFile = [];            // register file R0,..., R15
var nRegisters = 0;          // total number of registers
var sysCtlRegIdx = 0;        // index of first system control reg

function showSysStat (s) {
    return s===0 ? 'Usr' : 'Sys'
}

// Instructions refer to the system control registers by a 4-bit
// index, but the system control register that has instruction index 0
// will actually have a higher index (16) in the emulator's array of
// registers.  To refer to sysctl reg i, it can be accessed as
// register [sysCtlRegIdx + i].

// Global variables for accessing the registers

var register = [];              // all the registers, control and regfile
var registerIndex = 0;          // unique index for each reg
var regStored = [];
var regFetched = [];

// Each register is represented by an object that contains its current
// value, as well as methods to get and set the value, and to refresh
// the display.  The mkReg function makes a register corresponding to
// a gui element.  To remove highlight, use refresh.

// Arguments
//   regName is the name of the register, e.g. "pc"
//   eltName is the html id of the gui element
//   show(x) converts a value x to a string that can be displayed
// Set by creation loop
//   regIdx is the unique index in the array of all registers
// Local state
//   val is the current contents of the register
//   elt is gui element used to display the register
//   put(x) discards current val and replaces it with x (highlight if mode set)
//   get() returns current val (highlight if mode set)
//   refresh() puts the current val into display, without any highlighting

function testReg1 () {
    console.log("testReg1");
    regClearAccesses();
    pc.put(3);
    console.log ("ir = " + ir.get());
    console.log ("pc = " + pc.get());
    regShowAccesses();
}

function testReg2 () {
    console.log("testReg1");
    regClearAccesses();
    console.log ("ir = " + ir.get());
    console.log ("pc = " + pc.get());
    adr.put(20);
    regShowAccesses();
}


// The registers are actually created in gui.js in the window.onload
// function, because they need the gui display elements to be created
// first

function mkReg (rn,eltName,showfcn) {
    r = Object.create({
	regIdx : 0, // will be overwritten with actual index
	regName : rn,
	show : showfcn,
	val : 0,
	elt : document.getElementById(eltName),
	put : function (x) {
	    this.val = x;
	    console.log (`reg put rn=${rn} idx=${this.regIdx} x=${x}`);
	    if (this.regIdx<16) {
		// record regfile put
		instrEffect.push (["R", this.regIdx, x, this.regName]);
	    console.log (`mkReg put recording effect 0 ${instrEffect[0]}`);
	    console.log (`mkReg put recording effect 1 ${instrEffect[1]}`);
	    }
	    if (modeHighlight) { regStored.push(this) } },
        get : function () {
	        x = this.val;
	        if (modeHighlight) { regFetched.push(this) };
	        return x },
	refresh : function() {
	    console.log (`refresh register ${rn}`);
	    this.elt.innerHTML = this.show(this.val);
	},
	showNameVal: function() {return this.regName + '=' + this.show(this.val);}
    });
//    register[nRegisters] = this;
    nRegisters++;
    return r;
}

// R0 is special: it always contains 0 and cannot be changed
function mkReg0 (rn,eltName,showfcn) {
    r = Object.create({
	regName : rn,
	show : showfcn,
	val : 0,
	elt : document.getElementById(eltName),
	put : function (x) { },
        get : function () { return 0 },
	refresh : function() {this.elt.innerHTML = this.show(this.val);},
	showNameVal: function() {return this.regName + '=' + this.show(this.val);}
    });
    nRegisters++;
    return r;
}

function regShowAccesses () {
    let i, r;
    for (i = 0; i < regFetched.length; i++) {
	r = regFetched[i];
	r.elt.innerHTML = highlightText(r.show(r.val),'GET')
    }
    for (i = 0; i < regStored.length; i++) {
	r = regStored[i];
	r.elt.innerHTML = highlightText(r.show(r.val),'PUT')
    }
}

function regClearAccesses () {
    let r;
    for (let i = 0; i < regFetched.length; i++) {
	regFetched[i].refresh();
    }
    for (let i = 0; i <regStored.length; i++) {
	regStored[i].refresh();
    }
    regFetched = [];
    regStored = [];

}

// Resetting the registers sets them all to 0, 
function resetRegisters () {
    console.log('Resetting registers');
    for (var i = 0; i < nRegisters; i++) {
	register[i].val = 0;
	register[i].refresh();
    }
}

// Refresh all the registers.  This ensures the display corresponds to the
// current values, and it also removes any highlighting of the registers.

function refreshRegisters() {
    console.log('Refreshing registers');
    for (var i = 0; i < nRegisters; i++) {
	register[i].refresh();
    }
}

// ------------------------------------------------------------------------
// Highlighting registers to indicate accesses

// When a register is accessed, its display in the gui is highlighted
// by setting the text color.  If the register has not been used it
// has the default color black, if it has been read but not written
// its color is READ, and if it has been written its color is WRITE.
// The meanings of the tags for syntax highlighting are defined in
// Sigma16gui.css.  Normally we would use blue for READ and red for
// WRITE.

var modeHighlight = true;  // indicate get/put by setting text color

function setModeHighlight (x) {
    if (x) {
	console.log('Setting modeHighlight to True');
	modeHighlight = true;
    }
    else {
	console.log('Setting modeHighlight to False');
	modeHighlight = false;
	refreshRegisters();
    }
}

function highlightText (txt,tag) {
    return "<span class='" + tag + "'>" + txt + "</span>";
}

function clearRegisterHighlighting () {
    let n =  highlightedRegisters.length;
    var r;
    for (var i = 0; i < n; i++) {
	r = highlightedRegisters[i];
	console.log('clear highlight ' + i + ' reg = ' + r.regName);
	r.refresh();
    };
    highlightedRegisters = [];
}

//----------------------------------------------------------------------
//  Memory
//----------------------------------------------------------------------

// Usage
//   General operations
//     memInitialize              get html elements, clear refresh, display
//     memClear ()                set each location to 0
//     memRefresh ()              recalculate the memory strings

//   During instruction execution
//     memClearAccesses ()        remove get/put highligting
//     ir = memFetchInstr (a)     instruction fetch
//     adr = memFetchData (a)     data fetch
//     memStore (a,x)             store
//     memShowAccesses ()         update array of hex strings
//     memDisplayFast ()          render html elements showing only accessed area
//     memDisplayFull ()          render html elements showing full memory
//     memDisplay ()              use display mode to select fast or full


// The memory is represented as array of words (represented as an
// integer between 0 and 2^16-q) and a corresponding array of strings
// showing each word as a hex number.  There are html elements for
// displaying the memory contents, and two arrays to track memory
// accesses (fetches and stores) in order to generate the displays.

var memSize = 65536; // number of memory locations = 2^16
var memory = [];  // the memory contents, indexed by address
var memString = []; // a string for each location, to be displayed
var memElt1, memElt2;  // html elements for two views into the memory
var memFetchInstrLog = [];
var memFetchDataLog = [];
var memStoreLog = [];
var memDisplayModeFull = false;  // show entire memory? or just a little of it?
var memDisplayFastWindow = 16;   // how many locations to show in fast mode
var memDispOffset = 3;    // how many locations above highligted one

// Must wait until onload event

function memInitialize () {
    memElt1 = document.getElementById('MemDisplay1');
    memElt2 = document.getElementById('MemDisplay2');
    memClear();    // set each location to 0
    memRefresh();  // generate a string showing each location
    memDisplay();  // put the strings into the gui display elements
}

// There are two primary memory accesses: fetch and store.  These
// functions record the operation to enable the user interface to show
// the access by using colors to highlight the accessed location.

// There is just one memory, but the gui contains two windows into the
// memory: by convention, display 1 shows instruction fetches and
// display 2 shows data fetches and stores.  In the hardware (the
// digital circuit that implements the processor) there may be no
// distinction between memory and data accesses (although there could
// be if the machine has separate instruction and data caches).

// All memory stores are considered to be data stores.  Howver, there
// are two variants of fetch: instruction fetch and data fetch.  Both
// of these record the operation in the array memFetchInstrLog, but
// they record the address in separate scalar vairables to enable the
// gui to scroll the two displays to show the instruction access in
// disply 1 and the data access in display 2.

// Set all memory locations to 0

function memClear () {
    for (var i = 0; i < memSize; i++) {
	memory[i] = 0;
    }
    memFetchInstrLog = [];
    memFetchDataLog = [];
    memStoreLog = [];
    memRefresh ();
}

// Refresh all the memory strings; the memString array should be accurate
// but this function will recalculate all elements of that array

// Note on data structure.  I tried having a preliminary element [0]
// containing just "<pre class='HighlightedTextAsHtml'>", so address a
// is shown in memString[a+1].  The indexing was fine but the
// scrolling didn't work, possibly because of this dummy element with
// its newline inserted when the array is joined up.

function memRefresh () {
    memString = [];  // clear out and collect any existing elements
//    memString[0] =  "hello";  // "<pre class='HighlightedTextAsHtml'>"
    for (var i = 0; i < memSize; i++) {
	setMemString(i);
    }
//    memString.push("bye");    // "</pre>");

}

// Create a string to represent a memory location; the actual value is
// in the memory array, and the string is placed in the memString
// array.  memString[0] = <pre class="HighlightedTextAsHtml"> and
// mem[a] corresponds to memString[a+1].

function setMemString(a) {
    memString[a] = wordToHex4(a) + ' ' + wordToHex4(memory[a]);
}

// Fetch and return a word from memory at address a, and record the
// address so the display can show this access.

function memFetchInstr (a) {
    memFetchInstrLog.push(a);
    return memory[a];
}

function memFetchData (a) {
    memFetchDataLog.push(a);
    return memory[a];
}

// Store a word x into memory at address a, and record the address so
// the display can show this access.

function memStore (a,x) {
    memStoreLog.push(a);
    instrEffect.push(["M", a, x]);
    memory[a] = x;
}

// Update the memory string for each location that has been accessed,
// so that it contains an html div element which can be used to
// highlight the memory location.  Do the fetches first, then the
// stores: this ensures that if a location has both been fetched and
// stored, the highlighting for the store will take precedence.

function memShowAccesses () {
    let i, a;
    for (i = 0; i < memFetchInstrLog.length; i++) {
	a = memFetchInstrLog[i];
	highlightMemString(a,"GET");
    }
    for (i = 0; i < memFetchDataLog.length; i++) {
	a = memFetchDataLog[i];
	highlightMemString(a,"GET");
    }
    for (i = 0; i < memStoreLog.length; i++) {
	a = memStoreLog[i];
	highlightMemString(a,"PUT");
    }
}

// Remove the highlighting for the memory locations that have been accessed

function memClearAccesses () {
    let a;
    for (i=0; i<memFetchInstrLog.length; i++) {
	a = memFetchInstrLog[i];
	setMemString(a);
    }
    for (i=0; i<memFetchDataLog.length; i++) {
	a = memFetchDataLog[i];
	setMemString(a);
    }
    for (i=0; i<memStoreLog.length; i++) {
	a = memStoreLog[i];
	setMemString(a);
    }
    memFetchInstrLog = [];
    memFetchDataLog = [];
    memStoreLog = [];
}

// Create a string with a span class to represent a memory location
// with highlighting; the actual value is in the memory array, and the
// string is placed in the memString array.

function highlightMemString(a,highlight) {
    memString[a] =
	"<span class='" + highlight + "'>"
	+ wordToHex4(a) + " " + wordToHex4(memory[a])
        + "</span>";
}

// Set the memory displays, using the memString array.  Check mode to
// determine whether the display should be partial and fast or
// complete but slow.

function memDisplay () {
    if (memDisplayModeFull) { memDisplayFull () }
    else { memDisplayFast () }
}

// Set the memory displays, showing only part of the memory to save time

function memDisplayFast () {
//    console.log ('memDisplayFast');
    let xa, xb, xs1, xs, yafet, yasto, ya, yb, ys1, ys;

    xa = (memFetchInstrLog.length===0) ? 0 : (memFetchInstrLog[0] - memDispOffset);
    xa = xa < 0 ? 0 : xa;
    xb = xa + memDisplayFastWindow;
    xs = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
    	+ memString.slice(xa,xb).join('\n')
	+ "</code></pre>";

//    xs = "<pre><code class='HighlightedTextAsHtml'>TEST</code></pre>";
//    xs = "<pre><code>TEST</code></pre>";
//    xs = "<code>TEST</code>";
//    xs = "<pre>TEST</pre>";
    console.log ('  xa=' + xa + '  xb=' + xb);
    memElt1.innerHTML = xs;

    yafet = (memFetchDataLog.length===0) ? 0 : (memFetchDataLog[0] - memDispOffset);
    yasto = (memStoreLog.length===0) ? 0 :(memStoreLog[0] - memDispOffset);
    ya = yafet > 0 && yafet < yasto ? yafet : yasto;
    ya = ya < 0 ? 0 : ya;
    yb = ya + memDisplayFastWindow;
    ys = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	+ memString.slice(ya,yb).join('\n')
	+ "</code></pre>";
    console.log ('  ya=' + ya + '  yb=' + yb);
    memElt2.innerHTML = ys;
}

// Set the memory displays, showing the full memory

// Need <pre> to get the formatting correct with line breaks.  but
// <pre> prevents scrolling from working.  Could try not using pre,
// but putting <br> after each line, but that still wouldn't work
// because multiple spaces in code wouldn't work..  Try <code>; With
// <code class=... scrolling works, but the line breaks aren't
// there.. Is there a problem with HighlightedTextAsHtml?

// THE RIGHT WAY TO DO IT: code inside pre; class defined in code:

//    xs = "<pre><code class='HighlightedTextAsHtml'>"
//	+ memString.join('\n')
//	+ "</code></pre>";

function memDisplayFull () {
    let xs;                 // display text
    let xt, xo;             // display 1 targets and offsets
    let yafet, yasto, ya, yo
    console.log ('memDisplayFull');
    xs = "<pre class='CodePre'><code class='HighlightedTextAsHtml'>"
	+ memString.join('\n')
	+ "</code></pre>";
    memElt1.innerHTML = xs;
    xt = (memFetchInstrLog.length===0)? 0 : memFetchInstrLog[0] - memDispOffset;
    xo = xt * pxPerChar;
    console.log('  target1 xt = ' + xt + '   offset1 = ' + xo);
    memElt1.scroll(0,xo);
    
    memElt2.innerHTML = xs;
    yafet = (memFetchDataLog.length===0) ? 0 : (memFetchDataLog[0] - memDispOffset);
    yasto = (memStoreLog.length===0) ? 0 :(memStoreLog[0] - memDispOffset);
    yt = (yasto > 0 ? yasto : yafet) - memDispOffset;
    yt = yt < 0 ? 0 : yt;
    yo = yt * pxPerChar;
    console.log('  yafet=' + yafet + ' yasto=' + yasto
		+ '  target1 yt = ' + yt + '   offset1 = ' + yo);
    memElt2.scroll(0,yo);
}

function memTest1a () {
    console.log('testMem1a');
    memClear ();
    memStore(3,7);
    memStore(6,2);
    memShowAccesses ();
    memDisplay ();
}

function memTest1b () {
    console.log('testMem1b');
    memClearAccesses ();
    let y = memFetchInstr(6);
    memStore(300,20);
    memShowAccesses();
    memDisplay ();
//    console.log('testMem1 x = ' + x);  // should be 0, highlight Fetch
//    console.log('testMem1 y = ' + y);  // should be 7, highlight Store
}

function memTest2 () {
    console.log('testMem2');
    memClear ();
    let y = memFetchInstr (32768);
    let q = memFetchData (50);
    memStore (65520, 7);
    memShowAccesses ();
    memDisplay ();
}
