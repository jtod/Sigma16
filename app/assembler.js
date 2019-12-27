// Sigma16: assembler.js

// The assembler translates assembly language to machine language

// var symbolTable = new Map();;   ??? use m.symbolTable instead of global

// Object lines
var objectLineBuffer = "";
var objBufferSize = 0;
var objBufferLimit = 5;

// assembler
// var asmStmt = [];
// var symbols = [];
// var locationCounter = 0;
// var asmListingPlain = [];


// Notes...
// ??? inconsistent args parseAsmLine

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

function showSymbolTable (m) {
    m.asmListingPlain.push('');
    m.asmListingDec.push('');
    m.asmListingPlain.push("<span class='ListingHeader'>Symbol table</span>");
    m.asmListingDec.push("<span class='ListingHeader'>Symbol table</span>");
    m.asmListingPlain.push("<span class='ListingHeader'>Name      Val   Def   Used</span>");
    m.asmListingDec.push("<span class='ListingHeader'>Name      Val   Def   Used</span>");
    m.symbols =[ ...m.symbolTable.keys() ].sort();
    console.log('symbols = ' + m.symbols);
    for (let i = 0; i < m.symbols.length; i++) {
	let x = m.symbolTable.get(m.symbols[i]);
	m.asmListingPlain.push(m.symbols[i].padEnd(10)
			+ wordToHex4(x.val)
			+ x.defLine.toString().padStart(5));
	m.asmListingDec.push(m.symbols[i].padEnd(10)
			+ wordToHex4(x.val)
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
// Assembly language statement
//----------------------------------------------------------------------

// Representation of assembly language statement

// Each statement has a listing line which contains the line number,
// object code, and source code.  There are two versions of this:
// listingLinePlain just contains the text of the listing line, while
// listingLineHighlightedFields contains <span> elements to enable the
// various fields to be highlighted with colors.

function mkAsmStmt (lineNumber, address, srcLine) {
    return {lineNumber,                     // array index of statement
	    address,                        // address where code goes
	    srcLine,                        // source line
	    listingLinePlain: "",           // object and source text
	    listingLineHighlightedFields : "", // listing with src field spans
	    fieldLabel : '',                // label
	    fieldSpacesAfterLabel : '',     // white space
	    fieldOperation : '',            // operation mnemonic
	    fieldSpacesAfterOperation : '', // white space
	    fieldOperands : '',             // operands
	    fieldComment : '',             // comments are after operand or ;
	    hasLabel : false,               // statement has a valid label
	    operation : noOperation,        // spec of the operation if exists
	    format : NOOPERATION,
	    hasOperand : false,             // statement contains an operand
	    operandType : NOOPERAND,        // type of operand actually present
	    operandRRR : false,             // statement contains RRR operand
	    operandRR : false,              // statement contains RR operand
	    operandRX : false,              // statement contains RX operand
	    operandJX : false,              // statement contains short form RX
	    operandDATA : false,            // statement contains data
	    op : 0,                         // operation
	    d : 0,                          // destination
	    a : 0,                          // source a
	    b : 0,                          // source b
	    rr1 : 0,                        // first reg in rr asm format
	    rr2 : 0,                        // second reg in rr asm format
	    dispField : '0',                // displacement field
	    disp : 0,                       // displacement value
	    k : 0,                          // constant in RRKX format
	    dat : 0,                        // data value
	    codeSize : 0,                   // number of words generated
	    codeWord1 : -1,                  // first word of object
	    codeWord2 : -1,                  // second word of object
	    errors : []                     // array of lines of error messages
	   }
}

const missing = 0;  // indicates that a component of a statement is missing

// Print the object representing a source line; for testing

function printAsmStmt (m,x) {
    console.log('Statement ' + x.lineNumber + ' = /' + x.srcLine + '/');
    console.log('  label field /' + x.fieldLabel + '/');
    console.log('  spaces after label /' + x.fieldSpacesAfterLabel + '/');
    console.log('  operation field /' + x.fieldOperation + '/');
    console.log('  spaces after operation /' +
		x.fieldSpacesAfterOperation + '/');
    console.log('  operand field /' + x.fieldOperands + '/');
    console.log('  comment /' + x.fieldComment + '/');
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

// Report an assembly error: s is an assembly source line, err is an
// error message

function mkErrMsg (m,s,err) {
    let errline = err;
    s.errors.push(errline);
    m.nAsmErrors++;
}
//    console.log('mkErrMsg ' + errline);
//    let errline = '<span class="ERR">' + err + '</span>';

function assembler () {
    console.log('assembler');
    m = s16modules[currentModNum];  // info about assembly will be stored in m
    m.nAsmErrors = 0;
    m.asmStmt = [];
    m.symbols = [];
    m.asmListingPlain = [];
    m.asmListingDec = [];
    m.objectCode = [];
    m.locationCounter = 0;
    m.asmap = [];
    clearObjectCode ();
    document.getElementById('AsmTextHtml').innerHTML = "";
    m.symbolTable.clear();
    m.asmListingPlain.push(
	"<span class='ListingHeader'>Line Addr Code Code Source</span>");
    m.asmListingDec.push(
	"<span class='ListingHeader'>Line Addr Code Code Source</span>");
    asmPass1 (m);
    asmPass2 (m);
    if (m.nAsmErrors > 0) {
	m.asmListingPlain.unshift(highlightText(`\n ${m.nAsmErrors} errors detected\n`,'ERR'));
	m.asmListingDec.unshift(highlightText(`\n ${m.nAsmErrors} errors detected\n`,'ERR'));
    }
    m.asmListingPlain.unshift("<pre class='HighlightedTextAsHtml'>");
    m.asmListingDec.unshift("<pre class='HighlightedTextAsHtml'>");
    showSymbolTable(m);
    m.asmListingPlain.push("</pre>");
    m.asmListingDec.push("</pre>");
    setAsmListing (m);
}

//----------------------------------------------------------------------
//  Parser
//----------------------------------------------------------------------

// Syntax of assembly language

const nameParser = /^[a-zA-Z][a-zA-Z0-9]*$/;
const rrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;
const rxParser =
    /^R([0-9a-f]|(?:1[0-5])),(-?[a-zA-Z0-9\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const jxParser =
    /^([a-zA-Z0-9\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const datParser =
    /^(((?:[a-zA-Z][a-zA-Z0-9]*)|(?:\$[0-9a-f]{4})|(?:-?[0-9]+)))$/;
const intParser = /^-?[0-9]+$/;
const hexParser = /^\$([0-9a-f]{4})$/;

// A register is R followed by register number, which must be either
// a 1 or 2 digit decimal number between 0 and 15, or a hex digit.

const parseReg = /R[0-9a-f]|(?:1[0-5])/;

// An RRR operand consists of three registers, separated by comma
const rrrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;

const rrkParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),([0-9][0-9]?)$/;

// RCX asm format (register, control reg name): getctl R3,mask
const rcxParser =
    /^R([0-9a-f]|(?:1[0-5])),([a-zA-Z][a-zA-Z0-9]*)$/;

// CRX asm format (control reg name, register): putctl mask,R3
// const crxParser =
//    /^([a-zA-Z][a-zA-Z0-9]*),R([0-9a-f]|(?:1[0-5]))$/;

// A string literal consists of arbitrary text enclosed in "...", for
// example "hello".  String literals are most commonly used in data
// statements, but a string literal containing just one character can
// also be used in a lea instruction.  If a string literal contains a
// double quote ", this must be preceded by a backslash to escape it.

// parseString: match " followed by any number of (\" or ^") followed
// by ".  It's important to check for the 2-character sequence \"
// before checking for the single character ^" --- otherwise the \
// will match the ^" so the following " will end the string
// prematurely.

// a quoted character is \x where x is any character; backslash is \\

const parseString = /"((\\")|[^"])*"/;

// testParser (parseString, '"abc123"')
// testParser (parseString, '"ab\\"c123"')

// Split into line fields: non-space characters but space can appear in string
// A field is (non-whitespace | string) *
// String is " (non" | \" )* "

// name: may contain letters, digits, underscore; must begin with letter
// decimal number: optional - followed by digits
// hex number: $ followed by four hex digits (a/A are both ok)
// character: 'x' where x is any character. a quote is '\''. ',' is ok

// field: continguous non-space characters, separated from other
// fields by white space.  The first field must begin in the first
// column, i.e. cannot follow any white space.  A field may contain a
// string literal, which might contain one or more white space
// characters.

// The rrxParser accepts any string that contains the characters that
// are legal for the displacement field, but the field needs to be
// checked for validity (e.g. 23xy is not a valid displacement).

// Test a parser p on a string s and print the fields that are extracted.
// Example: testParser (rxParser, "R7,$2d3[R5]")


// A statement consists of a sequence of fields and whitespace-fields.
// Each is optional.  The structure is:

// label, whitespace, operation, whitespace, operands, whitespace

// optional label field.

// if label: label.  optional space. operation. optional
// space. operands.  if no label.  A field consists of normal
// characters (non space, non ;) or strings

// White space is matched by (\s+)

// experiment.

// const splitParser =
//    /(^[^\s;]*)((?:\s+)?)((?:[^\s;]+)?)((?:\s+)?)((?:[^\s;]+)?)(.*$)/;


// const parseSplitFields =
//     /^ ((?:\w+)?) ((?:\s+)?) ((?:\w+)?) ((?:\s+)?) (.*) $/;

// const regexpSplitFields =
//      '^((?:\\w+)?)((?:\\s+)?)((?:\\w+)?)((?:\\s+)?)((?:\\w+)?)((?:\\s+)?)(.*)$';

     // '((?:\\w+)?)';  field
// const regExpComment = '((?:(?:(;(?:.*))|(?:.*))?)';

// Field is    (  (" (\" | ^")*  ")   |  (^ ";) ) +
// Field is    (  ("((\\")|[^"])*")   |  [^\\s";]) ) +


const regExpString = '"((\")|[^"])*"';

function testParseString () {
    let p = new RegExp(regExpString);
    testParser (p, '"abc"');
    testParser (p, '"abc def"');
    testParser (p, '"abc\\"hithere"');

}

const regExpField = '((?:(?:(?:"(?:(?:\\")|[^"])*")|[^\\s";])+)?)';
//                             " (    \"   | ^")*")


// ........

// A field may contain non-space characters, and may contain a string
// literal, but cannot contain a space or ; (unless those appear in a
// string literal).  The field terminates as soon as a space or ; or
// end of the line is encountered.

// Simplified version of field: don't allow string literals
const regexpFieldNoStringLit =  '((?:[^\\s";]*)?)'

function runTestFieldNoStringLit () {
    let p = new RegExp(regexpFieldSimpleNoStringLit);
    testParser (p, 'abc');
    testParser (p, 'abc,def');
    testParser (p, 'abc def');
    testParser (p, 'abc;def');
    testParser (p, 'abc"def');
}

const regExpWhiteSpace = '((?:\\s+)?)';
const regExpComment = '((?:.*))';

const regexpSplitFields =
      '^'                             // anchor to beginning of line
      + regexpFieldNoStringLit        // label
      + regExpWhiteSpace              // separator
      + regexpFieldNoStringLit        // operation
      + regExpWhiteSpace              // separator
      + regexpFieldNoStringLit        // operands
      + regExpComment                 // comment
      + '$';                          // anchor to end of line

const parseField = new RegExp(regExpField);
const parseSplitFields = new RegExp(regexpSplitFields);

function runTestParser () {
    testParser (parseSplitFields, 'abc');
    testParser (parseSplitFields, 'abc def');
    testParser (parseSplitFields, 'abc def ghi');
    testParser (parseSplitFields, 'abc def ghi jkl');

    testParser (parseSplitFields, '  abc');
    testParser (parseSplitFields, '  abc def');
    testParser (parseSplitFields, '  abc def ghi');
    testParser (parseSplitFields, '  abc def ghi jkl');

    testParser (parseSplitFields, ';  abc');
    testParser (parseSplitFields, '  abc ; def');
    testParser (parseSplitFields, '  abc def ; ghi');
    testParser (parseSplitFields, '  abc def ghi ; jkl');

    testParser (parseSplitFields, 'loop');
    testParser (parseSplitFields, '     load   R1,x[R2]  R1 := x');
    testParser (parseSplitFields, 'lbl  load   R1,x[R2]  R1 := x');
    testParser (parseSplitFields, 'lbl  load   R1,x[R2]  ; R1 := x');

    testParser (parseSplitFields, 'arr  data  3,9,-12,$03bf,42  initial data');
    testParser (parseSplitFields, '  data  "hello world"');
    testParser (parseSplitFields, 'lbl  lea  R3,","[R0]   R3 := comma ');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0]   R3 := space ');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0]');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo ","');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo ",');
    testParser (parseSplitFields, 'lbl lea  R3," "[R0] foo " "');
    testParser (parseSplitFields, 'lbl  lea  R3," "[R0]   ; R3 := " " ');
    testParser (parseSplitFields, 'lbl  lea  R3," "[R0]   R3 := " " ');

    testParser (parseField, 'abc');
    testParser (parseField, 'abc def');
    testParser (parseField, 'abc" "def');
    testParser (parseSplitFields, 'abc" "def"  "ghi  123 456');
    testParser (parseSplitFields, 'abc" "def"  "ghi  123 456  R1 := " " ha');
    testParser (parseSplitFields, 'abc" "def"  "ghi  123 456  R1 := "," ha');
}


// const parseSplitFields =
//       /^((?:[^;\s]|(?:"(?:(?:\\")|[^"])*"))*)((?:\s+)?)(.*)$/;

// testParser (parseSplitFields, 'loop_23')
// testParser (parseSplitFields, 'loop_23 add')
// testParser (parseSplitFields, '*,3x)2r')
// testParser (parseSplitFields, 'label load R1,x[R0] hello')
// testParser (parseSplitFields, '12"abc123"xy')
// testParser (parseSplitFields, 'ab;comment')
// testParser (parseSplitFields, 'ab"cd;ef"gh')


// testParser (parseSplitFields, '   load')


//    / (^[^\s;]*) ((?:\s+)?) ((?:[^\s;]+)?) ((?:\s+)?) ((?:[^\s;]+)?) (.*$)/;

function testParser (p,s) {
    console.log (`testParser ${p} on ${s}`);
    let result = p.exec(s);
    console.log (`testParser result[0] = /${result[0]}/`);
    console.log (`testParser result[1] = /${result[1]}/`);
    console.log (`testParser result[2] = /${result[2]}/`);
    console.log (`testParser result[3] = /${result[3]}/`);
    console.log (`testParser result[4] = /${result[4]}/`);
    console.log (`testParser result[5] = /${result[5]}/`);
    console.log (`testParser result[6] = /${result[6]}/`);
    console.log (`testParser result[7] = /${result[7]}/`);
    console.log (`testParser result[8] = /${result[8]}/`);
    console.log (`testParser result[9] = /${result[9]}/`);
    console.log (`testParser result[10] = /${result[10]}/`);
    console.log (`testParser result[11] = /${result[11]}/`);
    console.log (`testParser result[12] = /${result[12]}/`);
}


// similar to highlightListingLine in emulator

function highlightField (xs,highlight) {
    return "<span class='" + highlight + "'>" + xs + "</span>";
}

//----------------------------------------------------------------------
//  Assembler Pass 1
//----------------------------------------------------------------------

function asmPass1 (m) {
    let asmSrcLines = document.getElementById('EditorTextArea').value.split('\n');
    console.log('assembler pass 1: ' + asmSrcLines.length + ' source lines');
    for (let i = 0; i < asmSrcLines.length; i++) {
	m.asmStmt[i] = mkAsmStmt (i, m.locationCounter, asmSrcLines[i]);
	let s = m.asmStmt[i];
	console.log(`pass1 i=  ${i} src= + ${s.srcLine}`);

	parseAsmLine (m,i);
	m.locationCounter += m.asmStmt[i].codeSize;
//	printAsmStmt(m.asmStmt[i]);
    }
}

function printAsmStmts (m) {
//    console.log('printAsmStmts');
    for (let i = 0; i < m.asmStmt.length; i++) {
	printAsmStmt(m.asmStmt[i]);
    }
}

// Parse the source for line i and update the object with the results

function parseAsmLine (m,i) {
    let s = m.asmStmt[i];
//    console.log('parseAsmLine, m.asmStmt = ');
//    printAsmStmt(m,s);
    let p = parseSplitFields.exec(s.srcLine);
    s.fieldLabel = p[1];
    s.fieldSpacesAfterLabel = p[2];
    s.fieldOperation = p[3];
    s.fieldSpacesAfterOperation = p[4];
    s.fieldOperands = p[5];
    s.fieldComment = p[6];
    console.log(`label = /${s.fieldLabel}/`);
    console.log(`operation = /${s.fieldOperation}/`);
    console.log(`operands = /${s.fieldOperands}/`);
    console.log(`comments = /${s.fieldComment}/`);
    parseLabel (m,s);
    parseOperation (m,s);
    parseOperand (m,s);
    if (s.hasLabel) {
	if (m.symbolTable.has(s.fieldLabel)) {
	    //	    s.errors.push(s.fieldLabel + ' has already been defined');
	    mkErrMsg (m, s, s.fieldLabel + ' has already been defined');
	} else {
	    m.symbolTable.set (s.fieldLabel,
	     {symbol : s.fieldLabel, val : m.locationCounter,
	      defLine : s.lineNumber});
	}
    }
}

// If operand isn't correct type for the operation, give an error
// message.  The operations that need to have the operand checked jave
// code <= DATA.  The higher operations either don't need an operand
// (COMMENT) or need to be checked individually (DIRECTIVE).

function checkOpOp (m,s) {
    console.log(`checkOpOp line ${s.lineNumber}`);
    let format = s.format;
    let operandType = s.operandType;
    console.log (`checkOpOp operation=${s.operation} format=${format} operandType=${operandType}`);
    if (format <= DATA && format != operandType) {
	let msg = `${s.fieldOperation} is ${showFormat(format)} format, but operand type is ${showFormat(operandType)}`;
	mkErrMsg (m,s,msg);
    }
}

// Set hasLabel to true iff there is a syntactically valid label.  If
// the label field isn't blank but is not syntactically valid
// (i.e. doesn't match the regular expression for names), then
// generate an error message.

function parseLabel (m,s) {
    if (s.fieldLabel == '') {
	s.hasLabel = false;
    } else if (s.fieldLabel.search(nameParser) == 0) {
	s.hasLabel = true;
    } else {
	s.hasLabel = false;
	mkErrMsg(m, s, s.fieldLabel + ' is not a valid label');
    }
}

// Set operation to the instruction set object describing the
// operation, if the operation field is defined in the map of
// operations.  Otherwise leave operation=null.  Thus s.operation can
// be used as a Boolean to determine whether the operation exists, as
// well as the specification of the operation if it exists.

function parseOperation (m,s) {
    let op = s.fieldOperation;
    if (op !== '') {
	let x = statementSpec.get(op);
	if (x) {
	    s.operation = x;
	    s.format = s.operation.format;
	    console.log(`parse operation ${s.srcLine} fmt=${s.format}`);
	    s.codeSize = formatSize(x.format);
	} else {
	    if (op.search(nameParser) == 0) {
		mkErrMsg (m, s, `${op} is not a valid operation`);
	    } else {
		mkErrMsg (m, s, `syntax error: operation ${op} must be a name`);
	    }
	}
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
function parseOperand (m,s) {
    let rrr = rrrParser.exec (s.fieldOperands);
    let rrk = rrkParser.exec (s.fieldOperands);
    let rr  = rrParser.exec  (s.fieldOperands);
    let rx  = rxParser.exec  (s.fieldOperands);
    let jx  = jxParser.exec  (s.fieldOperands);
    let rcx = rcxParser.exec (s.fieldOperands);
    let dat = datParser.exec (s.fieldOperands);
    if (rrr) {
	s.hasOperand = true;
	s.operandType = RRR;
	s.operandRRR = true;
	s.d = rrr[1];
	s.a = rrr[2];
	s.b = rrr[3];
    } else if (rr) {   // can omit d for cmp, omit b for inv
	s.hasOperand = true;
	s.operandType = RR;
	s.operandRR = true;
	s.rr1 = rr[1];
	s.rr2 = rr[2];
//	s.d = rr[1];
//	s.a = rr[2];
    } else if (rx) {
	s.hasOperand = true;
	s.operandType = RX;
	s.operandRX = true;
	s.d = rx[1];
	s.dispField = rx[2];
	s.a = rx[3];
    } else if (jx) {
	s.hasOperand = true;
	s.operandType = JX;
	s.operandJX = true;
	s.dispField = jx[1];
	s.a = jx[2];
    } else if (rrk) {
	console.log ('rrk');
	s.hasOperand = true;
	s.operandType = RRKX;
	s.operandRRKX = true;
	s.d = rrk[1];
	s.a = rrk[2];
	s.k = rrk[3];
    } else if (rcx) {
	console.log ('rcx');
	s.d = rcx[1];
	let ctlRegName = rcx[2]
	let ctlRegIdx = findCtlIdx (m,s,ctlRegName);
	s.ctlReg = ctlRegIdx;
	console.log (`rcx d=${s.d} ctlreg=${ctlRegName} ctlRegIdx=${ctlRegIdx}`);
    } else if (dat) {
	s.hasOperand = true;
	s.operandType = DATA;
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

// Given a string xs, either return the control register index for it
// if xs is indeed a valid control register name; otherwise generate
// an error message.

function findCtlIdx (m,s,xs) {
    let c = ctlReg.get(xs);
    let i = 0;
    if (c) {
	i = c.ctlRegIndex;
    } else {
	mkErrMsg (m,s,`${xs} is not a valid control register`);
    }
    console.log (`findCtlIdx ${xs} => ${i}`);
    return i;
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

// Make a code word from four 4-bit fields
function mkWord (op,d,a,b) {
    let clear = 0x000f;
    return ((op&clear)<<12) | ((d&clear)<<8) | ((a&clear)<<4) | (b&clear);
}

// Make a code word from two 4-bit fields and an 8 bit field (EXP format)
function mkWord448 (op,d,k) {
    let clear4 = 0x000f;
    let clear8 = 0x00ff;
    return ((op&clear4)<<12) | ((d&clear4)<<8) | (k&clear8);
}

function testWd(op,d,a,b) {
    console.log(wordToHex4(mkWord(op,d,a,b)));
}

function asmPass2 (m) {
    console.log('assembler pass 2');
    let s, fmt,op,x;
    initializeObjectLineBuffer ();
    for (let i = 0; i < m.asmStmt.length; i++) {
	s = m.asmStmt[i];
	fmt = s.format;
	console.log(`pass2 line=${s.lineNumber} s=/${s.srcLine}/`);
	console.log(`pass2 line=${s.lineNumber} fmt=${fmt} opcode=${s.operation.opcode}`);
	console.log(`pass2 line=${s.lineNumber} operation=${s.operation} operandType=${s.operandType}`);
	checkOpOp (m,s);
	if (fmt==RRR) {
	    console.log (`pass2 RRR`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],s.d,s.a,s.b);
	    generateObjectWord (m, s, s.address, s.codeWord1);
//	    console.log('pass2 RRR ' + wordToHex4(s.codeWord1));
	} else if (fmt==RR) { // like RRR but with b=0
	    console.log (`pass2 RR`);
	    op = s.operation.opcode;
	    if (op[0]==4) { // cmp, where d is omitted
		s.codeWord1 = mkWord(op[0],0,s.rr1,s.rr2);
	    } else { // inc, where b is omitted
		s.codeWord1 = mkWord(op[0],s.rr1,s.rr2,0);
	    }
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    console.log (`pass2 op=${op} ${wordToHex4(s.codeWord1)}`);
	    //	    console.log('pass2 RR ' + wordToHex4(s.codeWord1));
	} else if (fmt==RRKX) {
	    console.log (`pass2 RRKX`);
	    console.log (`d=${s.d} a=${s.a} k=${s.k}`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
	    s.codeWord2 = mkWord(s.a,s.k,0,0);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==RX) {
	    console.log (`pass2 RX`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],s.d,s.a,op[1]);
	    let aaa = s.dispField;
	    let bbb = evaluate(m,s,s.dispField);
	    s.codeWord2 = evaluate(m,s,s.dispField);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
//	    console.log('pas2 rx aaa=' + aaa + ' bbb=' + bbb);
//	    console.log('pass2 RX ' + wordToHex4(s.codeWord1)
//			+ wordToHex4(s.codeWord2));
	} else if (fmt==JX) { // like RX but with d=0
	    console.log (`pass2 JX`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],op[2],s.a,op[1]);
	    s.codeWord2 = evaluate(m,s,s.dispField);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
//	    console.log('pass2 JX ' + wordToHex4(s.codeWord1)
	    //			+ wordToHex4(s.codeWord2));
	} else if (fmt==RCX) {
	    console.log ("pass2 RCX");
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord448(14,s.d,op[1]);
	    s.codeWord2 = s.ctlReg;
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==NOOPERAND) {
	    console.log ('pass2 NOOPERAND');
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord448(op[0],0,op[1]);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	} else if (fmt==DATA && s.operandDATA) {
	    console.log('fmt is DATA and operandDATA=' + s.dat);
	    s.codeWord1 = evaluate(m,s,s.dat);
	    generateObjectWord (m, s, s.address, s.codeWord1);
//	    console.log('pass2 DATA ' + wordToHex4(s.codeWord1));
	} else {
	    console.log('pass2 other, noOperation');
	}
	s.listingLinePlain =  (s.lineNumber+1).toString().padStart(4,' ')
	    + ' ' + wordToHex4(s.address)
	    + ' ' + (s.codeSize>0 ? wordToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize>1 ? wordToHex4(s.codeWord2) : '    ')
//	    + ' ' + s.srcLine
	    + s.fieldLabel
	    + s.fieldSpacesAfterLabel
	    + s.fieldOperation
	    + s.fieldSpacesAfterOperation
	    + s.fieldOperands
	    + s.fieldComment;
	s.listingLineHighlightedFields = (s.lineNumber+1).toString().padStart(4,' ')
	    + ' ' + wordToHex4(s.address)
	    + ' ' + (s.codeSize>0 ? wordToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize>1 ? wordToHex4(s.codeWord2) : '    ')
//	    + ' ' + s.srcLine
	    + highlightField (s.fieldLabel, "FIELDLABEL")
	    + s.fieldSpacesAfterLabel
	    + highlightField (s.fieldOperation, "FIELDOPERATION")
	    + s.fieldSpacesAfterOperation
	    + highlightField (s.fieldOperands, "FIELDOPERAND")
	    + highlightField (s.fieldComment, "FIELDCOMMENT") ;

	m.asmListingPlain.push(s.listingLinePlain);
	m.asmListingDec.push(s.listingLineHighlightedFields);
	for (let i = 0; i < s.errors.length; i++) {
	    m.asmListingPlain.push(highlightText('Error: ' + s.errors[i],'ERR'));
	    m.asmListingDec.push(highlightText('Error: ' + s.errors[i],'ERR'));
	}
    }
    flushObjectLine (m);
}

// linenumber indexed from 0 but add1 for asm listing header and add1
// for html <span>
// function tempshow(m,a) {
//     console.log( '  ' + a + '  ' +  (m.asmap[a] + 2) );
//     }
    
// Object lines

// The code generator outputs lines of object code containing up to
// nobjWordsOnLine words.  Each word is added to a buffer by calling
// generateObjectWord, and when the buffer reaches the limit (or when all
// object code has been pushed) the buffer is flushed.


function initializeObjectLineBuffer () {
    objectLineBuffer = "  hexdata  ";
    objBufferSize = 0;
}

// Generate object code line for module m and clear the buffer for next code line
function flushObjectLine(m) {
    console.log('flushObjectLine ' + objectLineBuffer);
    if (objBufferSize > 0) { m.objectCode.push (objectLineBuffer) };
    initializeObjectLineBuffer ();
}

// Add object word x to buffer for next object code line in module m.
// The asm statement is s, and the object word will be loaded at
// address a.  The line number entered into asmap is ln = s.lineNumber
// + 1 to account for the header line that is inserted before the
// statements in the listing.

function generateObjectWord (m, s, a, x) {
    let ln = s.lineNumber + 2;  // adjust for <span> line and header line
    m.asmap[a] = ln;
    console.log ('generateObjectWord entered asmap[' + a + '] = ' + ln);
    if (objBufferSize > 0) { objectLineBuffer += ',' };
    objectLineBuffer += wordToHex4(x);
    objBufferSize++;
    if (objBufferSize >= objBufferLimit) { flushObjectLine(m) };
}

function showAsmap (m) {
    console.log ('showAsmap');
    for (var i in m) {
	console.log ('[' + i + '] = ' + m[i]);
    }
}

// Evaluate a displacement or data value.  This may be a decimal
// constant, a hex constant, or a label.
function evaluate (m,s,x) {
    console.log('evaluate ' + x);
    if (x.search(nameParser) == 0) {
	r = m.symbolTable.get(x);
	if (r) {
	    console.log('evaluate returning ' + r.val);
	    return r.val;
	} else {
	    //	    s.errors.push('symbol ' + x + ' is not defined');
	    mkErrMsg (m, s, 'symbol ' + x + ' is not defined');
	    console.log('evaluate returning ' + 0);
	    return 0;
	}
    } else if (x.search(intParser) == 0) {
	console.log('evaluate returning ' + parseInt(x,10));
	return intToWord(parseInt(x,10));
    } else if (x.search(hexParser) == 0) {
	return hex4ToWord(x.slice(1));
	console.log('evaluate returning ' + 0);
	return 0;
    } else {
	//	s.errors.push('expression ' + x + ' has invalid syntax');
	mkErrMsg (m, s, 'expression ' + x + ' has invalid syntax');
//	console.log('evaluate returning ' + 0);
	return 0;
    }
}

function setAsmListing (m) {
//    console.log('setAsmListing');
//    let listing = [];
//    for (let i = 0; i < asmSrcLines.length; i++) {
//	listing[i] = asmStmet[i].listingLineHighlightedFields;
    //    }
    let listing = m.asmListingDec.join('\n');
//    let listing = m.asmListingPlain.join('\n');
    console.log ('setAsmListing ' + listing);
    document.getElementById('AsmTextHtml').innerHTML = listing;
//	m.asmListingPlain.join('\n');
//    console.log (' Set asm listing m.asmListingPlain = ' + m.asmListingPlain);
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
function testOperand (m,x) {
    showOperand(parseOperand(m,x));
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
