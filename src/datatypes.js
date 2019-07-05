// Sigma16, John O'Donnell, 2019
// Sigma16datatypes.js

// Hexadecimal, registers, memory

//----------------------------------------------------------------------
// Hexadecimal notation
//----------------------------------------------------------------------

// Character code constants for useful hex characters
const charCode0 = '0'.charCodeAt(0);
const charCode9 = '9'.charCodeAt(0);
const charCodea = 'a'.charCodeAt(0);
const charCodef = 'f'.charCodeAt(0);
const charCodeA = 'A'.charCodeAt(0);
const charCodeF = 'F'.charCodeAt(0);
const hexDigit =
      ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'];
const maxWord16 = 65535;  // max binary number in 16 bits = 2^16 - 1

// If an integer is negative, return the positive integer corresponding
// to its two's complement interpretation
function tcAdjust (x) { return x<0 ? x+maxWord16+1 : x }

var intToHex4 = function (x) {
    if (x < 0 || x > maxWord16) {
	console.log('intToHex4 invalid word: ' + x);
	return('####');
    }
    let a = x & 0x000f;
    x = x >>> 4;
    let b = x & 0x000f;
    x = x >>> 4;
    let c = x & 0x000f;
    x = x >>> 4;
    let d = x & 0x000f;
    let r = hexDigit[d] + hexDigit[c] + hexDigit[b] + hexDigit[a];
//    console.log('a=' + a + 'b=' + b +'c=' + c +'d=' + d);
//    console.log('r=' + r);
    return r;
}

var intToBit = function (x) {
    if (x < 0 || x > 1)
    {console.log('intToBit invalid int: ' + x);
     return('#');
    }
    return x===0 ? '0' : '1';
}

// Convert a hex character to an integer, or NaN if it isn't a valid
// hex character.  The valid hex characters are 0-9, a-f, and A-F.

function hexCharToInt (cx) {
    let c = cx.charCodeAt(0);
    let y = charCode0 <= c && c <= charCode9
	? c - charCode0
	: (charCodea <= c && c <= charCodef
	   ? 10 + c - charCodea
	   : (charCodeA <= c && c <= charCodeF
	      ? 10 + c - charCodeA
	      : NaN));
//    console.log("hexCharToInt " + cx + " c=" + c + " y=" + y)
    return y;
}

// Convert a string with four hex digits to an integer
// If the string doesn't contain exactly 4 characters, or if
// any character is not a hex digit, the result is NaN.

function hex4ToInt (h) {
//    console.log("hex4ToInt " + h.length);
    if (h.length != 4) return NaN;
    return (4096 * hexCharToInt(h[0])
  	    +  256  * hexCharToInt(h[1])
    	    +   16  * hexCharToInt(h[2])
       	    +         hexCharToInt(h[3]));
}

//----------------------------------------------------------------------
//  Registers
//----------------------------------------------------------------------

// The registers are defined as global variables.  These variables are
// declared here but their values are set only when window has been
// loaded, because the gui elements will exist at that time.

var modeHighlightAccess = true;  // for tracing: highlight reg/mem that is accessed

var pc, ir, adr, dat, spc;   // instruction control
var ien;                     // interrupt control
                             // segmentation control
var regFile = [];            // user registers R0,..., R15
var nRegisters = 0;          // total number of registers

// Global variables for accessing the registers

var register = [];              // all the registers, control and regfile
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

function mkReg (rn,eltName,showfcn) {
    r = Object.create({
	regName : rn,
	show : showfcn,
	val : 0,
	elt : document.getElementById(eltName),
	put : function (x) {
	    this.val = x;
	    if (modeHighlight) {
//		this.elt.innerHTML = highlightText(show(x),'PUT')
		regStored.push(this);
	    }
	},
	get : function () {
	    x = this.val;
	    if (modeHighlight) {
//		this.elt.innerHTML = highlightText(show(x),'GET')
		regFetched.push(this);
	    };
	    return x;
	},
	refresh : function() {this.elt.innerHTML = this.show(this.val);},
	showNameVal: function() {return this.regName + '=' + this.show(this.val);}
    });
    
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

// temporary version: change this to iterate over array of registers

function refreshRegisters() {
    pc.refresh();
    ir.refresh();
    adr.refresh();
    dat.refresh();
    spc.refresh();
    highlightedRegisters = [];
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

// The memory is represented with separate arrays, to enable the contents
// to be represented as a typed array

var memSize = 65536; // number of memory locations = 2^16
var memory = [];  // the memory contents, indexed by address
var memString = []; // a string for each location, to be displayed
var memStoredAddresses = []
var memFetchedAddresses = []
var memElt1, memElt2;  // html elements for two views into the memory

function memInitialize () {
    memElt1 = document.getElementById('MemDisplay1');
    memElt2 = document.getElementById('MemDisplay2');
    memClear();    // set each location to 0
    memRefresh();  // generate a string showing each location
    memDisplay();  // put the strings into the gui display elements
}

// Fetch and return a word from memory at address a, and record the
// address so the display can show this access.
function memFetch (a) {
    memFetchedAddresses.push(a);
    return memory[a];
}

// Store a word x into memory at address a, and record the address so
// the display can show this access.
function memStore (a,x) {
    memStoredAddresses.push(a);
    memory[a] = x;
}

// Update the memory string for each location that has been accessed,
// so that it contains an html div element which can be used to
// highlight the memory location.  Do the fetches first, then the
// stores: this ensures that if a location has both been fetched and
// stored, the highlighting for the store will take precedence.
function memShowAccesses () {
    let i, a;
    for (i = 0; i < memFetchedAddresses.length; i++) {
	a = memFetchedAddresses[i];
	highlightMemString(a,"GET");
    }
    for (i = 0; i < memStoredAddresses.length; i++) {
	a = memStoredAddresses[i];
	highlightMemString(a,"PUT");
    }
}

// Remove the highlighting for the memory locations that have been accessed
function memClearAccesses () {
    let a;
    for (i=0; i<memFetchedAddresses.length; i++) {
	a = memFetchedAddresses[i];
	setMemString(a);
    }
    for (i=0; i<memStoredAddresses.length; i++) {
	a = memStoredAddresses[i];
	setMemString(a);
    }
    memFetchedAddresses = [];
    memStoredAddresses = [];
}

// Set all memory locations to 0
function memClear () {
    for (var i = 0; i < memSize; i++) {
	memory[i] = 0;
    }
    memRefresh();
    memFetchedAddresses = [];
    memStoredAddresses = [];
}

// Refresh all the memory strings; the memString array should be accurate
// but this function will recalculate all elements of that array
function memRefresh () {
    for (var i = 0; i < memSize; i++) {
	setMemString(i);
    }
}

// Create a string to represent a memory location; the actual value is
// in the memory array, and the string is placed in the memString
// array.
function setMemString(a) {
    memString[a] = intToHex4(a) + ' ' + intToHex4(memory[a]);
}

// Create a string with a span class to represent a memory location
// with highlighting; the actual value is in the memory array, and the
// string is placed in the memString array.
function highlightMemString(a,highlight) {
    memString[a] =
	"<span class='" + highlight + "'>"
	+ intToHex4(a) + " " + intToHex4(memory[a])
        + "</span>";
}

// Set the memory displays, using the memString array
function memDisplay () {
    let xs = memString.join('\n');
    memElt1.innerHTML = xs;
    memElt2.innerHTML = xs;
}


//----------------------------------------------------------------------
//  Testing
//----------------------------------------------------------------------

function testMem1() {
    console.log('testMem1');
    memStore(3,7);
    memStore(6,2);
    let x = memFetch(1)
    let y = memFetch(3);
    memStore(7,20);
    memShowAccesses();  // put highligh spans into memString
    memDisplay();      // update the display
    console.log('testMem1 x = ' + x);  // should be 0, highlight Fetch
    console.log('testMem1 y = ' + y);  // should be 7, highlight Store
}

function testMem2() {
    console.log('testMem2');
    memClearAccesses();
    memStore(3,37);
    let a = memFetch(4);
    let b = memFetch(5);
    memStore(9,1);
    let x = memFetch(6);
    memStore(7,20);
    memStore(6,255);
    memShowAccesses();  // put highligh spans into memString
    memDisplay();      // update the display
    console.log('testMem1 x = ' + x);  // should be 2
}
