
/* An assembly language line consists of four fields separated by
white space.  Everything following a semicolon ; is a comment.

label
   anchored at beginning of line
   may be empty string (if first character is ; or space)
   contains any characters apart from whitespace or ;
spacesAfterLabel
operation
spacesAfterOperation
operands
comments field
   anchored at end of line
   contains any characters
*/

//----------------------------------------------------------------------
// Running and testing
//----------------------------------------------------------------------

// Run the current test case, called in Initialization
function run () {
    console.log('run()');
//    assembler();
}

//----------------------------------------------------------------------
// Symbol table
//----------------------------------------------------------------------

var symbolTable = new Map();;

function showSymbolTable() {
    m.asmListing.push('');
//    m.asmListing.push("<span class='ListingHeader'>Symbol table</span>");
    m.asmListing.push("<span class='ListingHeader'>Name      Val   Def   Used</span>");
    m.symbols =[ ...symbolTable.keys() ].sort();
    console.log('symbols = ' + m.symbols);
    for (let i = 0; i < m.symbols.length; i++) {
	let x = symbolTable.get(m.symbols[i]);
	m.asmListing.push(m.symbols[i].padEnd(10)
			+ intToHex4(x.val)
			+ x.defLine.toString().padStart(5));
    }
//    let xs = [];
//    symbolTable.forEach(function(sym,key,owner) {
//	console.log(key + ' val=' + sym.val + ' def=' + sym.defLine);
//    })
}

function showSymbol (s) {
    return (s.symbol + ' val=' + s.val + ' def ' + s.defLine);
}


//----------------------------------------------------------------------
// Instruction set architecture and assembly language
// ---------------------------------------------------------------------

// Instruction formats and assembly language statement codes

const RRR         = 0     // R1,R2,R3
const RR          = 1     // R1,R2        RRR format omitting b field (e.g. inv)
const RX          = 2;    // R1,xyz[R2]
const JX          = 3;    // loop[R0]     RX format omitting d field (e.g. jump)
const DATA        = 4;    // -42
const COMMENT     = 5;    // ; full line comment, or blank line
const NOOPERATION = 6;

function showFormat (n) {
    return ['RRR','RR','RX','JX','DATA','COMMENT','NOOPERATION'] [n]
}

// Give the size of generated code for an instruction format
function formatSize (fmt) {
    if (fmt==RRR || fmt==RR || fmt==DATA) {
	return 1
    } else if (fmt==RX | fmt==JX) {
	return 2
    } else {
	return 0
    }
}

// The instruction set is represented by a map from mnemonic to statementSpec spec
var statementSpec = new Map();

// Each statement is initialized as noOperation; this is overridden if a
// valid operation field exists (by the parseOperation function)
var noOperation = {format:NOOPERATION, opcode:[]};

// Data statements
statementSpec.set("data",  {format:DATA, opcode:[]});

// Opcodes (in the op field) of 0-13 denote RRR instructions
statementSpec.set("add",   {format:RRR, opcode:[0]});
statementSpec.set("sub",   {format:RRR, opcode:[1]});
statementSpec.set("mul",   {format:RRR, opcode:[2]});
statementSpec.set("div",   {format:RRR, opcode:[3]});
statementSpec.set("cmp",   {format:RRR, opcode:[4]});
statementSpec.set("cmplt", {format:RRR, opcode:[5]});
statementSpec.set("cmpeq", {format:RRR, opcode:[6]});
statementSpec.set("cmpgt", {format:RRR, opcode:[7]});
statementSpec.set("inv",   {format:RRR, opcode:[8]});
statementSpec.set("and",   {format:RRR, opcode:[9]});
statementSpec.set("or",    {format:RRR, opcode:[10]});
statementSpec.set("xor",   {format:RRR, opcode:[11]});
statementSpec.set("nop",   {format:RRR, opcode:[12]});  // reserved
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
statementSpec.set("jump",     {format:JX,  opcode:[15,3,0]});
statementSpec.set("jumpc0",   {format:RX,  opcode:[15,4]});
statementSpec.set("jumpc1",   {format:RX,  opcode:[15,5]});
statementSpec.set("jumpf",    {format:RX,  opcode:[15,6]});
statementSpec.set("jumpt",    {format:RX,  opcode:[15,7]});
statementSpec.set("jal",      {format:RX,  opcode:[15,8]});

// Mnemonics for jumpc0/jumpc1 based on signed comparisons
statementSpec.set("jumplt",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumple",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpeq",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpne",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpge",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpgt",   {format:JX,  opcode:[15,1]});

// Mnemonics for jumpc0/jumpc1 based on unsigned comparisons
statementSpec.set("jumplt",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumple",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpeq",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpne",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpge",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpgt",   {format:JX,  opcode:[15,1]});

// Mnemonics for jumpc0/jumpc1 based on signed/unsigned overflow and carry
statementSpec.set("jumpv",    {format:JX,  opcode:[15,1]});
statementSpec.set("jumpnv",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpvu",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpnvu",  {format:JX,  opcode:[15,1]});
statementSpec.set("jumpco",   {format:JX,  opcode:[15,1]});
statementSpec.set("jumpnco",  {format:JX,  opcode:[15,1]});

//----------------------------------------------------------------------
// Assembly language
//----------------------------------------------------------------------


// Representation of assembly language statement

function mkAsmStmt (lineNumber, address, srcLine) {
    return {lineNumber,                     // 1 + array index of statement
	    address,                        // address where code goes
	    srcLine,                        // source line
	    fieldLabel : '',                // label
	    fieldSpacesAfterLabel : '',     // white space
	    fieldOperation : '',            // operation mnemonic
	    fieldSpacesAfterOperation : '', // white space
	    fieldOperands : '',             // operands
	    fieldComments : '',             // comments are after operand or ;
	    hasLabel : false,               // statement has a valid label
	    operation : noOperation,        // spec of the operation if exists
	    hasOperand : false,             // statement contains an operand
	    operandRRR : false,             // statement contains RRR operand
	    operandRR : false,              // statement contains RR operand
	    operandRX : false,              // statement contains RX operand
	    operandJX : false,              // statement contains short form RX
	    operandDATA : false,            // statement contains data
	    op : 0,                         // operation
	    d : 0,                          // destination
	    a : 0,                          // source a
	    b : 0,                          // sourc b
	    dispField : '0',                // displacement field
	    disp : 0,                       // displacement value
	    dat : 0,                        // data value
	    codeSize : 0,                   // number of words generated
	    codeWord1 : -1,                  // first word of object
	    codeWord2 : -1,                  // second word of object
	    errors : []                     // array of lines of error messages
	   }
}

const missing = 0;  // indicates that a component of a statement is missing

// Print the object representing a source line; for testing

function printAsmStmt (x) {
    console.log('Statement ' + x.lineNumber + ' = /' + x.srcLine + '/');
    console.log('  label field /' + x.fieldLabel + '/');
    console.log('  spaces after label /' + x.fieldSpacesAfterLabel + '/');
    console.log('  operation field /' + x.fieldOperation + '/');
    console.log('  spaces after operation /' +
		x.fieldSpacesAfterOperation + '/');
    console.log('  operand field /' + x.fieldOperands + '/');
    console.log('  comment /' + x.fieldComments + '/');
    console.log (x.hasLabel ? ('  label = ' + x.fieldLabel) : '  no label');
    console.log (x.operation ?
		 '  operation requires ' + showFormat(x.operation.format)
		 : '  no operation');
    console.log ('  operand RRR=' + x.operandRRR
		 + ' RR=' + x.operandRR
		 + ' RX=' + x.operandRX
		 + ' JX=' + x.operandJX
		 + ' DATA=' + x.operandDATA);
    console.log ('  d=' + x.d + ' a=' + x.a + ' b=' + x.b
		 + ' disp=' + x.disp + ' dat=' + x.dat);
    console.log ('  address = ' + x.address
		 + ' codesize=' + x.codeSize
		 + ' codeWord1=' + x.codeWord1 + ' codeWord2=' + x.codeWord2);
    if (x.errors.length > 0) {
	console.log ('error messages:\n' + x.errors.join('\n'));
    } else {
	console.log ('no errors detected');
    }
}

//----------------------------------------------------------------------
//  Assembler
//----------------------------------------------------------------------

// var asmStmt = [];
// var symbols = [];
// var locationCounter = 0;
// var asmListing = [];

var m; // set to current module, used to access key asm variables m.asmStmt etc

function mkErrMsg (s,err) {
    //    let errline = '<span class="ERR">' + err + '</span>';
    let errline = err;
//    console.log('mkErrMsg ' + errline);
    s.errors.push(errline);
}

function assembler () {
    console.log('assembler');
    m = s16modules[currentModNum];
    m.asmStmt = [];
    m.symbols = [];
    m.asmListing = [];
    m.objectCode = [];
    m.locationCounter = 0;
    clearObjectCode ();
    document.getElementById('AsmTextHtml').innerHTML = "";
    symbolTable.clear();
    m.asmListing.push("<pre>");
    m.asmListing.push(
	"<span class='ListingHeader'>Line Addr Code Code Source</span>");

    asmPass1 ();
    asmPass2 ();
    //    printAsmStmts();
    showSymbolTable();
    m.asmListing.push("</pre>");
    setAsmListing ();
}

//----------------------------------------------------------------------
//  Assembler Pass 1
//----------------------------------------------------------------------

// Syntax of assembly language

const splitParser =
    /(^[^\s;]*)((?:\s+)?)((?:[^\s;]+)?)((?:\s+)?)((?:[^\s;]+)?)(.*$)/;
const nameParser = /^[a-zA-Z][a-zA-Z0-9]*$/;
const rrrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;
const rrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;
const rxParser =
    /^R([0-9a-f]|(?:1[0-5])),([a-zA-Z0-9\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const jxParser =
    /^([a-zA-Z0-9\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const datParser =
    /^(((?:[a-zA-Z][a-zA-Z0-9]*)|(?:\$[0-9a-f]{4})|(?:-?[0-9]+)))$/;
const intParser = /^-?[0-9]+$/;
const hexParser = /^\$([0-9a-f]{4})$/;

function asmPass1 () {
    let asmSrcLines = document.getElementById('EditorTextArea').value.split('\n');
    console.log('assembler pass 1: ' + asmSrcLines.length + ' source lines');
    for (let i = 0; i < asmSrcLines.length; i++) {
	m.asmStmt[i] = mkAsmStmt (i+1, m.locationCounter, asmSrcLines[i]);
	parseAsmLine (i);
	m.locationCounter += m.asmStmt[i].codeSize;
//	printAsmStmt(m.asmStmt[i]);
    }
}

function printAsmStmts () {
//    console.log('printAsmStmts');
    for (let i = 0; i < m.asmStmt.length; i++) {
	printAsmStmt(m.asmStmt[i]);
    }
}


// Parse the source for line i and update the object with the results

function parseAsmLine (i) {
    let s = m.asmStmt[i];
//    console.log('parseAsmLine, m.asmStmt = ');
//    printAsmStmt(s);
    let p = splitParser.exec(s.srcLine);
    s.fieldLabel = p[1];
    s.fieldSpacesAfterLabel = p[2];
    s.fieldOperation = p[3];
    s.fieldSpacesAfterOperation = p[4];
    s.fieldOperands = p[5];
    s.fieldComments = p[6];
    parseLabel (s);
    parseOperation (s);
    parseOperand (s);
    if (s.hasLabel) {
	if (symbolTable.has(s.fieldLabel)) {
	    s.errors.push(s.fieldLabel + ' has already been defined');
	} else {
	    symbolTable.set (s.fieldLabel,
	     {symbol : s.fieldLabel, val : m.locationCounter,
	      defLine : s.lineNumber});
	}
    }
}

// Set hasLabel to true iff there is a syntactically valid label.  If
// the label field isn't blank but is not syntactically valid
// (i.e. doesn't match the regular expression for names), then
// generate an error message.


function parseLabel (s) {
    if (s.fieldLabel == '') {
	s.hasLabel = false;
    } else if (s.fieldLabel.search(nameParser) == 0) {
	s.hasLabel = true;
    } else {
	s.hasLabel = false;
	mkErrMsg(s, s.fieldLabel + ' is not a valid label');
//	s.errors.push(s.fieldLabel + ' is not a valid label');
    }
}

// Set operation to the instruction set object describing the
// operation, if the operation field is defined in the map of
// operations.  Otherwise leave operation=null.  Thus s.operation can
// be used as a Boolean to determine whether the operation exists, as
// well as the specification of the operation if it exists.

function parseOperation (s) {
    x = statementSpec.get(s.fieldOperation);
    if (x) {
	s.operation = x;
	s.codeSize = formatSize(x.format);
    }
}


// a constant value is
// a label is   (?:[a-zA-Z][a-zA-Z0-9]*)
// $ followed by 4 hex digits      (?:\$[0-9a-f]{4})
//  a decimal number with optional sign   (?:-?[0-9]+)

// parse displacement separately, for RX just use ([a-zA-Z0-9\$]+)
// ((?:[a-zA-Z][a-zA-Z0-9]*)|(?:\$[0-9a-f]{4})|(?:-?[0-9]+))

// const constParser = /^(dec number) | (hex const) $/;

// parse a source operand src and return an object describing the
// result of parsing the src text as an operand, which must be either
// rrr, rx, or data

// s is an AsmStmt object; parse the operand and update s
function parseOperand (s) {
    let rrr = rrrParser.exec(s.fieldOperands);
    let rr = rrParser.exec(s.fieldOperands);
    let rx = rxParser.exec(s.fieldOperands);
    let jx = jxParser.exec(s.fieldOperands);
    let dat = datParser.exec(s.fieldOperands);
    if (rrr) {
	s.hasOperand = true;
	s.operandRRR = true;
	s.d = rrr[1];
	s.a = rrr[2];
	s.b = rrr[3];
    } else if (rr) {
	s.hasOperand = true;
	s.operandRR = true;
	s.d = rr[1];
	s.a = rr[2];
    } else if (rx) {
	s.hasOperand = true;
	s.operandRX = true;
	s.d = rx[1];
	s.dispField = rx[2];
	s.a = rx[3];
    } else if (jx) {
	s.hasOperand = true;
	s.operandJX = true;
	s.dispField = jx[1];
	s.a = jx[2];
    } else if (dat) {
	s.hasOperand = true;
	s.operandDATA = true;
	s.dat = dat[1];
//	console.log('data 0' + dat[0]);
//	console.log('data 1' + dat[1]);
//	console.log('data 2' + dat[2]);
//	console.log('data 3' + dat[3]);
//	result.val = dat[1];
    } else {
	s.hasOperand = false;
    }
    return;
}


function showOperand (x) {
    if (x.hasOperand) {
	if (x.operandRRR) {
	    console.log('Operand ---' + x.src +
			'---  RRR d=' + x.d + ' a=' + x.a + ' b=' + x.b);
	} else if (x.operandRX) {
	    console.log('Operand ---' + x.src +
			'---  RX d=' + x.d + ' disp=' + x.disp + ' idx=' + x.idx);
	} else if (x.operandDATA) {
	    console.log('Operand ---' + x.src +	'---  Data dat=' + x.dat);
	} else {
	    console.log('Operand ---' + x.src +	'---  error: unrecognized format');
	}
    } else {
	console.log('Operand ---' + x.src + '---  Operand not found');
    }
}

//----------------------------------------------------------------------
//  Pass 2
//----------------------------------------------------------------------

function mkWord (op,d,a,b) {
    let clear = 0x000f;
    return ((op&clear)<<12) | ((d&clear)<<8) | ((a&clear)<<4) | (b&clear);
}

function testWd(op,d,a,b) {
    console.log(intToHex4(mkWord(op,d,a,b)));
}

function asmPass2 () {
    console.log('assembler pass 2');
    let s, fmt,op,x;
    initializeObjectLineBuffer ();
    for (let i = 0; i < m.asmStmt.length; i++) {
	s = m.asmStmt[i];
	fmt = s.operation.format;
//	console.log('pass2 i=' + i + ' fmt=' + showFormat(fmt));
	if (fmt==RRR) {
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],s.d,s.a,s.b);
	    pushObjectWord (s.codeWord1);
//	    console.log('pass2 RRR ' + intToHex4(s.codeWord1));
	} else if (fmt==RR) { // like RRR but with b=0
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],s.d,s.a,0);
	    pushObjectWord (s.codeWord1);
//	    console.log('pass2 RR ' + intToHex4(s.codeWord1));
	} else if (fmt==RX) {
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],s.d,s.a,op[1]);
	    let aaa = s.dispField;
	    let bbb = evaluate(s,s.dispField);
	    s.codeWord2 = evaluate(s,s.dispField);
	    pushObjectWord (s.codeWord1);
	    pushObjectWord (s.codeWord2);
//	    console.log('pas2 rx aaa=' + aaa + ' bbb=' + bbb);
//	    console.log('pass2 RX ' + intToHex4(s.codeWord1)
//			+ intToHex4(s.codeWord2));
	} else if (fmt==JX) { // like RX but with d=0
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],op[2],s.a,op[1]);
	    s.codeWord2 = evaluate(s,s.dispField);
	    pushObjectWord (s.codeWord1);
	    pushObjectWord (s.codeWord2);
//	    console.log('pass2 JX ' + intToHex4(s.codeWord1)
//			+ intToHex4(s.codeWord2));
	} else if (fmt==DATA && s.operandDATA) {
//	    console.log('fmt is DATA and operandDATA=' + s.dat);
	    s.codeWord1 = evaluate(s,s.dat);
	    pushObjectWord (s.codeWord1);
//	    console.log('pass2 DATA ' + intToHex4(s.codeWord1));
	} else {
//	    console.log('pass2 other, noOperation');
	}
	s.listingLine = s.lineNumber.toString().padStart(4,' ')
	    + ' ' + intToHex4(s.address)
	    + ' ' + (s.codeSize>0 ? intToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize>1 ? intToHex4(s.codeWord2) : '    ')
	    + ' ' + s.srcLine;
	m.asmListing.push(s.listingLine);
	for (let i = 0; i < s.errors.length; i++) {
	    m.asmListing.push(highlightText('Error: ' + s.errors[i],'ERR'));
	}
    }
    flushObjectLine ();
}

// Object lines

// The code generator outputs lines of object code containing up to
// nobjWordsOnLine words.  Each word is added to a buffer by calling
// pushObjectWord, and when the buffer reaches the limit (or when all
// object code has been pushed) the buffer is flushed.

var objectLineBuffer = "";
var objBufferSize = 0;
var objBufferLimit = 5;

function initializeObjectLineBuffer () {
    objectLineBuffer = "  hexdata  ";
    objBufferSize = 0;
}

// Generate object code line for module m and clear the buffer for next code line
function flushObjectLine() {
    console.log('flushObjectLine ' + objectLineBuffer);
    if (objBufferSize > 0) { m.objectCode.push (objectLineBuffer) };
    initializeObjectLineBuffer ();
}

// Add object word x to buffer for next object code line in module m
function pushObjectWord (x) {
    if (objBufferSize > 0) { objectLineBuffer += ',' };
    objectLineBuffer += intToHex4(x);
    objBufferSize++;
    if (objBufferSize >= objBufferLimit) { flushObjectLine() };
}


// Evaluate a displacement or data value.  This may be a decimal
// constant, a hex constant, or a label.
function evaluate (s,x) {
    console.log('evaluate ' + x);
    if (x.search(nameParser) == 0) {
	r = symbolTable.get(x);
	if (r) {
	    console.log('evaluate returning ' + r.val);
	    return r.val;
	} else {
	    s.errors.push('symbol ' + x + ' is not defined');
	    console.log('evaluate returning ' + 0);
	    return 0;
	}
    } else if (x.search(intParser) == 0) {
	console.log('evaluate returning ' + parseInt(x,10));
	return tcAdjust(parseInt(x,10));
    } else if (x.search(hexParser) == 0) {
	return hex4ToInt(x.slice(1));
	console.log('evaluate returning ' + 0);
	return 0;
    } else {
	s.errors.push('expression ' + x + ' has invalid syntax');
//	console.log('evaluate returning ' + 0);
	return 0;
    }
}

function setAsmListing () {
    console.log('setAsmListing');
//    let listing = [];
//    for (let i = 0; i < asmSrcLines.length; i++) {
//	listing[i] = asmStmet[i].listingLine;
//    }
    document.getElementById('AsmTextHtml').innerHTML =
	m.asmListing.join('\n');

}

//----------------------------------------------------------------------
//  Boot
//----------------------------------------------------------------------

var bootCurrentLocation = 0;  // global, used by booter

function boot() {
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
function linkerBootLine (i,x) {
    let y = parseAsmLine (i,x);
//    printAsmLine (y);
    let w = y.fieldOperands;
    let n =  hex4ToInt(w);
//    console.log('linkerBootLine ' + w + ' = ' + n);
    console.log('linkerBootLine ' + i + ' ---' + x + '--- = ' + n);
    updateMem2(bootCurrentLocation,n);
    bootCurrentLocation++;
}

//----------------------------------------------------------------------
//  Testing
//----------------------------------------------------------------------

// Test the parser on sample assembly source data

function testParse () {
    testAsmLine ("");
    testAsmLine ("; full line comment");
    testAsmLine ("   ; spaces and full line comment");
    testAsmLine ("  loop1  cmplt  R6,R2,R2  ; spaces before label");
    testAsmLine ("loop1");
    testAsmLine ("loop1  ");
    testAsmLine ("loop1;  ");
    testAsmLine ("loop1  ;  ");
    testAsmLine ("loo;p1  ;  ");
    testAsmLine ("loop1  cmplt  ; start of loop");
    testAsmLine ("loop1  add  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2  ; start of loop");
    testAsmLine ("lo;op1  cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1;  cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1 ; cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cm;plt  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt;  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt ;  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt  R6;,R2,R2   start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2;   start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2 ;  start of loop");
    testAsmLine ("loop1  cmplt  R6,R2,R2   start of loop");
}

function testAsmLine (x) {
    console.log('*** testAsmLine ---' + x + '---');
    let y = parseAsmLine(0,x);
    printAsmLine(y);
}

function testOperands () {
    console.log('testOperands');
    testOperand('R2,R4,R5');       // RRR
    testOperand('R2,R4,R15');      // RRR
    testOperand('R5,arr[R4]');     // RX
    testOperand('123');            // decimal number
    testOperand('-1');             // signed decimal number
    testOperand('$ab3e');          // hex number
    testOperand('xyz');            // label
    testOperand('');               // empty
    testOperand('R2,R4,R45');      // invalid RRR
    testOperand('R2,R4,R16');      // invalid RRR
    testOperand('R16,arr[R4]');    // invalid RX
    testOperand('12a3');           // invalid decimal number
    testOperand('$abgh');          // invalid hex number
}

// Try to parse string x as an operand and print the result
function testOperand (x) {
    showOperand(parseOperand(x));
}


/* from Architecture.hs



----------------------------------------------------------------------
-- Instruction formats
----------------------------------------------------------------------

-- There are two kinds of format: the machine instruction formats, and
-- the assembly language instruction statement formats.  There are
-- three machine instruction formats: RRR, RX, EXP.  However, there is
-- a larger set of assembly language statement formats, because there
-- are special syntaxes for some instructions, and there are assembler
-- directives that aren't instructions at all.

-- Opcodes:    op, xop (if op=e), b (if op=f)
-- Registers:  d, a, b, p, q
-- Constants   disp (16 bits), k (8 bits), kp (4 bits), kq (4 bits)

-- The first word of every instruction is loaded into ir, and it
-- always has these fields; each is 4 bits

--------------------------------------------------------------------
--            Fields              Format         Pseudo format
--------------------------------------------------------------------

--    ---------------------
--    | op |  d |  a |  b |    RRR, if op<e      RRR, RR
--    ---------------------

-- If the instruction has a second word, it is loaded into adr, and it
-- has several formats, depending on op
 
--    ---------------------
--    |   xop   |    k    |    EXP, if op=e      RRRK
--    ---------------------
--    |   xop   | kp | kq |    EXP, if op=e      RRRKK
--    ---------------------
--    |   xop   |  p | kq |    EXP, if op=e      RRRRK
--    ---------------------
--    |   xop   |  p |  q |    EXP, if op=e      RRRRR
--    ---------------------
--    |       disp        |    RX,  if op=f      RX, JX
--    ---------------------

-- These are the assembly language statement formats; some
-- of them are pseudo formats that map into one of the three
-- architecture formats (RR, EXP, RX).

data InstrFormat
  = Empty
  | AsmDir  -- assembler directive
  | Con     -- constant, for data statement        k16
  | RRR     -- machine instruction format    op    d,a,b
  | RR      -- pseudo RRR omit d             op    a,b
  | EXP     -- EXP expands RRR               op,x  d,a,b
  | RRRX    -- machine instruction format    op,x  d,a,b
  | RRRK    -- machine instruction format    op,x  d,a,b,k8
  | RRRRK   -- pseudo EXP                    op,x  d,a,b,p,k4
  | RRRRR   -- pseudo EXP                    op,x  d,a,b,p,q
  | RX      -- machine instruction format    op,b  d,k16[a]
  | JX      -- pseudo RX cond jump (d)       op,b  k16[a]
  deriving Show

type InstrSize = Int

-- Return the number of words required to represent an instruction of
-- a given format

fmtSize :: InstrFormat -> InstrSize
fmtSize Empty   = 0
fmtSize AsmDir  = 0
fmtSize Con     = 1
fmtSize RRR     = 1
fmtSize RR      = 1
fmtSize RRRX    = 2
fmtSize RRRK    = 2
fmtSize RRRRK   = 2
fmtSize RRRRR   = 2
fmtSize RX      = 2
fmtSize JX      = 2


*/
