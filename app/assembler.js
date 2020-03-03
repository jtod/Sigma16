// Sigma16: assembler.js
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
// assembler.js translates assembly language to machine language
//-------------------------------------------------------------------------------

// CharSet is a string containing all the characters that may appear
// in a valid Sigma16 assembly language source program.  It's always
// best to edit source programs using a text editor, not a word
// processor.  Word processors are likely to make character
// substitutions, for example en-dash for minus, typeset quote marks
// for typewriter quote marks, and so on.

const CharSet =
      "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" // letters
      + "0123456789"             // digits
      + " ,;"                     // separators
      + '"'                      // quotes
      + "'"                      // quotes
      + ".$[]()+-*"                // punctuation
      + "?¬£`<=>!%^&{}#~@:|/\\";   // other

function validateChars (xs) {
    console.log (`validateChars`);
    let i, c;
    let badlocs = [];
    for (i = 0; i < xs.length; i++) {
        c = xs.charAt(i);
        if (!CharSet.includes(c)) {
            console.log (`validateChars: bad char at ${i} in ${xs}`);
            badlocs.push(i);
        }
    }
    return badlocs
}


let opcode_cmp = 4; // for pass2/RR, may want to refactor this

// Buffers to hold generated object code
let objBufferLimit = 16;             // how many code items to allow per line
let objectWordBuffer = [];          // list of object code words
let relocationAddressBuffer = [];   // list of relocation addresses

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

function displaySymbolTableHtml (m) {
    m.asmListingPlain.push('');
    m.asmListingDec.push('');
    m.asmListingPlain.push("<span class='ListingHeader'>Symbol table</span>");
    m.asmListingDec.push("<span class='ListingHeader'>Symbol table</span>");
    m.asmListingPlain.push("<span class='ListingHeader'>Name      Val      Def   Used</span>");
    m.asmListingDec.push("<span class='ListingHeader'>Name      Val      Def   Used</span>");
    m.symbols =[ ...m.symbolTable.keys() ].sort();
    console.log('symbols = ' + m.symbols);
    for (let i = 0; i < m.symbols.length; i++) {
	let x = m.symbolTable.get(m.symbols[i]);
	m.asmListingPlain.push(m.symbols[i].padEnd(10)
			       + wordToHex4(x.val)
                               + (x.relocatable ? ' R' : ' C')
			       + x.defLine.toString().padStart(5)
                               + '  '
                               + x.symUsageLines.join(',')
                               + '  '
                               + x.symUsageAddrs.map((w)=>wordToHex4(w)).join(',')
                              );
	m.asmListingDec.push(m.symbols[i].padEnd(10)
			     + wordToHex4(x.val)
                             + (x.relocatable ? ' R' : ' C')
			     + x.defLine.toString().padStart(5)
                             + '  '
                             + x.symUsageLines.join(',')
                             + '  '
                             + x.symUsageAddrs.map((w)=>wordToHex4(w)).join(',')
                            );
    }
}

// For testing, not used normally
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
	    operation : NOOPERATION,        // spec of the operation if exists
	    format : UNKNOWN,
	    hasOperand : false,             // statement contains an operand
	    operandType : NOOPERAND,        // type of operand actually present
	    operandRRR : false,             // statement contains RRR operand
	    operandRR : false,              // statement contains RR operand
	    operandRX : false,              // statement contains RX operand
	    operandJX : false,              // statement contains short form RX
	    operandDATA : false,            // statement contains data
            operandX : false,               // expression operand for directive
            operandIDENT : false,           // identifier directive
            expSrc : null,                  // expression operand if any
            expValue : null,                // value of expression operand if any
	    op : 0,                         // operation
            operand_str1 : '',
            operand_str2 : '',
            operand_str3 : '',
            operand_str4 : '',
            operand_str5 : '',
            field_d : 0,
            field_a : 0,
            field_b : 0,
            field_disp : 0,
            field_e : 0,                     // EXP: word 2, first 4 bits
            field_f : 0,                     // EXP: word 2, second 4 bits
            field_g : 0,                     // EXP: word 2, third 4 bits
            field_h : 0,                     // EXP: word 2, fourth 4 bits
	    field_gh : 0,                    // EXP: word 2, last 8 bits
	    dat : 0,                        // data value
            identarg : '',                     // identifier operand
	    codeSize : 0,                   // number of words generated
	    codeWord1 : -1,                  // first word of object
	    codeWord2 : -1,                  // second word of object
	    errors : []                     // array of lines of error messages
	   }
}

/* deprecated
	    disp : 0,                       // displacement value
	    d : 0,                          // destination
	    a : 0,                          // source a
	    b : 0,                          // source b
	    rr1 : 0,                        // first reg in rr asm format
	    rr2 : 0,                        // second reg in rr asm format
	    k : 0,                          // constant in RRKEXP format
*/

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
		 + ' disp=' + x.field_disp + ' dat=' + x.dat);
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

function assembler () {
    let m = s16modules[selectedModule];
    console.log(`Assembling module ${selectedModule}`);
    m.modName = null;  // remove name from earlier assembly, if any
    m.nAsmErrors = 0;
    m.asmStmt = [];
    m.symbols = [];
    m.asmListingPlain = [];
    m.asmListingDec = [];
    m.objectCode = [];
    m.exports = [];
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
        m.isExecutable = false;
        document.getElementById('ProcAsmListing').innerHTML = "";
	m.asmListingPlain.unshift(highlightText(`\n ${m.nAsmErrors} errors detected\n`,'ERR'));
	m.asmListingDec.unshift(highlightText(`\n ${m.nAsmErrors} errors detected\n`,'ERR'));
    } else {
        m.isExecutable = true;
    }
    m.asmListingPlain.unshift("<pre class='HighlightedTextAsHtml'>");
    m.asmListingDec.unshift("<pre class='HighlightedTextAsHtml'>");
    displaySymbolTableHtml(m);
    m.asmListingPlain.push("</pre>");
    m.asmListingDec.push("</pre>");
    setAsmListing (m);
}

//----------------------------------------------------------------------
//  Regular expressions for the parser
//----------------------------------------------------------------------

// Syntax of assembly language

// a constant value is
// a label is   (?:[a-zA-Z][a-zA-Z0-9]*)
// $ followed by 4 hex digits      (?:\$[0-9a-f]{4})
//  a decimal number with optional sign   (?:-?[0-9]+)

// parse displacement separately, for RX just use ([a-zA-Z0-9\$]+)
// ((?:[a-zA-Z][a-zA-Z0-9]*)|(?:\$[0-9a-f]{4})|(?:-?[0-9]+))

// const constParser = /^(dec number) | (hex const) $/;

// attempt at rxParser allowing underscore, but this is the wrong approach
// /^R([0-9a-f]|(?:1[0-5])),(-?[a-zA-Z][a-zA-Z0-9_]\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
// const nameParser = /^[a-zA-Z][a-zA-Z0-9]*$/;

const nameParser = /^[a-zA-Z][a-zA-Z0-9_]*$/;
const rrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;
const rxParser =
    /^R([0-9a-f]|(?:1[0-5])),(-?[a-zA-Z0-9_\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const kxParser =
    /^([0-9a-f]|(?:1[0-5])),(-?[a-zA-Z0-9_\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const rrxParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),(-?[a-zA-Z0-9_\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const jxParser =
    /^([a-zA-Z0-9_\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;
const datParser =
    /^(((?:[a-zA-Z][a-zA-Z0-9_]*)|(?:\$[0-9a-f]{4})|(?:-?[0-9]+)))$/;
const intParser = /^-?[0-9]+$/;
const hexParser = /^\$([0-9a-f]{4})$/;

const identParser = /^[a-zA-Z][a-zA-Z0-9_]*$/; // temp; just allow one name ?????

// Use data parser instead...
// const expParser = /^\$([0-9a-f]{4})$/;  // temp: just allow hex const ?????

// A register is R followed by register number, which must be either
// a 1 or 2 digit decimal number between 0 and 15, or a hex digit.

const parseReg = /R[0-9a-f]|(?:1[0-5])/;

// An RRR operand consists of three registers, separated by comma
const rrrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;

// An RRRR operand consists of four registers, separated by comma
const rrrrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;

const rrkParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),([0-9][0-9]?)$/;

const rrrkParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),([0-9][0-9]?)$/;

const rrrkkParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),([0-9][0-9]?),([0-9][0-9]?)$/;

const rkkParser =
    /^R([0-9a-f]|(?:1[0-5])),([0-9][0-9]?),([0-9][0-9]?)$/;

const rrkkParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),([0-9][0-9]?),([0-9][0-9]?)$/;

// RCEXP asm format (register, control reg name): getctl R3,mask
const rcParser =
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


//----------------------------------------------------------------------
//  Testing the regular expressions
//----------------------------------------------------------------------

// Test a parser p on a string s and print the fields that are extracted.
// Example: testParser (rxParser, "R7,$2d3[R5]")


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
//  Parser
//----------------------------------------------------------------------

// Parse the source for line i and update the object with the results.
// Each source line is a statement; a statement consists of a sequence
// of fields and whitespace-fields.  Each is optional.  The structure
// is:

// label, whitespace, operation, whitespace, operands, whitespace

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
    let isImport = s.fieldOperation=="import";
    if (s.hasLabel) {
	if (m.symbolTable.has(s.fieldLabel)) {
	    mkErrMsg (m, s, s.fieldLabel + ' has already been defined');
	} else {
	    m.symbolTable.set (
                s.fieldLabel,
	        { symbol : s.fieldLabel,
                  symIsImport : isImport,
                  symImportedFrom : s.identarg,
                  relocatable : !isImport,
                  val : isImport ? 0 : m.locationCounter,
                  symUsageAddrs : [],
                  symUsageLines : [],
	          defLine : s.lineNumber+1});
	}
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


// parseOperand (m,s): m is the module being assembled, and s is the
// current statement, where s.srcOperands has already been set to the
// operand field of the statement. Parse s.srcOperands, set string
// variables to the various operand fields, and set flags indicating
// what type of operand field has been found.  The results are set in
// object variables belonging to s.

// An operand field contains a pattern that determines the type of operand (e.g. rrk or jx etc) and the actual text of each operand.  The function operates by checking the regular expression for every operand type against the operand field text.  Most of these are disjoint but there are some exceptions; for example a valid operand for an import statement could also be interpreted as an operand for a data statement.

function parseOperand (m,s) {
    let rr   = rrParser.exec  (s.fieldOperands);
    let rc   = rcParser.exec (s.fieldOperands);
    let rrr  = rrrParser.exec (s.fieldOperands);
    let rrk  = rrkParser.exec (s.fieldOperands);
    let rkk  = rkkParser.exec (s.fieldOperands);  // need this parser
    let rrkk = rrkkParser.exec (s.fieldOperands);  // need this parser
    let rrrk = rrrkParser.exec (s.fieldOperands);
    let rrrkk = rrrkkParser.exec (s.fieldOperands);
    let jx   = jxParser.exec  (s.fieldOperands);
    let rx   = rxParser.exec  (s.fieldOperands);
    let kx   = kxParser.exec  (s.fieldOperands);
    let rrx  = rrxParser.exec  (s.fieldOperands);
    let dat  = datParser.exec (s.fieldOperands);
    let ident = identParser.exec (s.fieldOperands);  // identifier (directive)
//  let exp = expParser.exec (s.fieldOperands);       // expression (directive)
// Check non-exclusive operand formats: ident
    if (ident) {
        s.hasOperand = true;
        s.operandIDENT = true;
        s.identarg = ident[0];
    }
// Check the mutually exclusive operand formats    
    if (rr) { // Rp,Rq
	s.hasOperand = true;
	s.operandType = RR;
        console.log (`Found ${s.operandType} operand`);
	s.operandRR = true;
	s.operand_str1 = rr[1]; // Rp
	s.operand_str2 = rr[2]; // Rq
    } else if (rc) { // Re,Cf
	console.log ('rc');
	s.hasOperand = true;
	s.operandType = RCEXP;
        console.log (`Found ${s.operandType} operand`);
	s.operandRCEXP = true;
	s.operand_str1 = rc[1]; // Re
	s.operand_str2 = rc[2]; // Cf
    } else if (rrr) { // Rp,Rq,Rr
	s.hasOperand = true;
	s.operandType = RRR;
        console.log (`Found ${s.operandType} operand`);
	s.operandRRR = true;
	s.operand_str1 = rrr[1]; // Rp
	s.operand_str2 = rrr[2]; // Rq
	s.operand_str3 = rrr[3]; // Rr
    } else if (rrk) { // Rd,Re,g
	console.log ('rrk');
	s.hasOperand = true;
	s.operandType = RRKEXP;
        console.log (`Found ${s.operandType} operand`);
	s.operandRRKEXP = true;
	s.operand_str1 = rrk[1]; // Re
	s.operand_str2 = rrk[2]; // Rf
	s.operand_str3 = rrk[3]; // k
//	s.field_ed = rrk[1];
//	s.a = rrk[2];
//	s.k = rrk[3];
    } else if (rkk) { // R2,R3,5,3
	console.log ('rkk');
	s.hasOperand = true;
	s.operandType = RKKEXP;
        console.log (`Found ${s.operandType} operand`);
	s.operandRKKEXP = true;
	s.operand_str1 = rkk[1];
	s.operand_str2 = rkk[2];
	s.operand_str3 = rkk[3];
    } else if (rrkk) { // R2,R3,5,3
	console.log ('rrkk');
	s.hasOperand = true;
	s.operandType = RRKKEXP;
        console.log (`Found ${s.operandType} operand`);
	s.operandRRKEXP = true;
	s.operand_str1 = rrkk[1];
	s.operand_str2 = rrkk[2];
	s.operand_str3 = rrkk[3];
	s.operand_str4 = rrkk[4];
        console.log (`pass1 rrkk 1=${s.operand_str1} 2=${s.operand_str2} 3=${s.operand_str3} 4=${s.operand_str4}`);
    } else if (rrrk) { // R2,R3,5,3
	console.log ('rrrk');
	s.hasOperand = true;
	s.operandType = RRRKEXP;
        console.log (`Found ${s.operandType} operand`);
	s.operand_str1 = rrrk[1];
	s.operand_str2 = rrrk[2];
	s.operand_str3 = rrrk[3];
	s.operand_str4 = rrrk[4];
    } else if (rrrkk) { // R1,R2,R3,5,3
	console.log ('rrrkk');
	s.hasOperand = true;
	s.operandType = RRRKKEXP;
        console.log (`Found ${s.operandType} operand`);
	s.operand_str1 = rrrkk[1];
	s.operand_str2 = rrrkk[2];
	s.operand_str3 = rrrkk[3];
	s.operand_str4 = rrrkk[4];
	s.operand_str5 = rrrkk[5];
    } else if (jx) { // disp[Ra]
	s.hasOperand = true;
	s.operandType = JX;
        console.log (`Found ${s.operandType} operand`);
	s.operandJX = true;
	s.operand_str1 = jx[1]; // disp
	s.operand_str2 = jx[2]; // Ra
    } else if (rx) { // Rd,disp[Ra]
	s.hasOperand = true;
	s.operandType = RX;
        console.log (`Found ${s.operandType} operand`);
	s.operandRX = true;
        s.operand_str1 = rx[1]; // Rd
        s.operand_str2 = rx[2]; // disp
        s.operand_str3 = rx[3]; // Ra
//	s.field_d = s.operand_str1;
//	s.field_disp = s.operand_str2;
//	s.field_a = s.operand_str3;
    } else if (kx) { // jumpc1 Rd,disp[Ra]
	s.hasOperand = true;
	s.operandType = KX;
        console.log (`Found ${s.operandType} operand`);
	s.operandKX = true;
        s.operand_str1 = kx[1]; // Rd
        s.operand_str2 = kx[2]; // disp
        s.operand_str3 = kx[3]; // Ra
	s.field_d = s.operand_str1;
	s.field_disp = s.operand_str2;
	s.field_a = s.operand_str3;
    } else if (rrx) { // Re,Rf,gh[Ra]
        console.log ('RRX');
	s.hasOperand = true;
	s.operandType = RRXEXP;
        console.log (`Found ${s.operandType} operand`);
	s.operandRX = true;
        console.log (`rrx 1=${rrx[1]} 2=${rrx[2]} 3=${rrx[3]} 4=${rrx[4]} `)
        s.operand_str1 = rrx[1]; // Rd
        s.operand_str2 = rrx[2]; // Re
        s.operand_str3 = rrx[3]; // gh
        s.operand_str4 = rrx[4]; // Rf
        console.log (`rrx e=${s.field_e} f=${s.field_f} disp=${s.field_disp} x=${s.d}`)
    } else if (dat) { // 34
	s.hasOperand = true;
	s.operandType = DATA;
        console.log (`Found ${s.operandType} operand`);
	s.operandDATA = true;
	s.dat = dat[1];
        console.log (`\n\nDAT`);
	console.log('data 0' + dat[0]);
	console.log('data 1' + dat[1]);
	console.log('data 2' + dat[2]);
	console.log('data 3' + dat[3]);
    } else {
	s.hasOperand = false;
        console.log (`Found no operand`);
    }
    return;
}


// If the operand doesn't match the instruction format, it's possible
// that some of the operand fields will be left undefined by the
// regular expression match.  For example, if the destination is
// omitted on an RX instruction, and the operand looks like JX, then
// the s.field_disp would be left undefined.  To prevent an error when
// this is passed to evaluate, it is protected by applying ensure to
// it.

function ensure (operand_field) {
    return operand_field ? operand_field : 0
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
        let badCharLocs = validateChars (asmSrcLines[i]);
        if (badCharLocs.length > 0) {
            mkErrMsg (m,s,`Invalid character at position ${badCharLocs}`);
            mkErrMsg (m,s, "See User Guide for list of valid characters");
            mkErrMsg (m,s, "(Word processors often insert invalid characters)");
        }
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

// If operand isn't correct type for the operation, give an error
// message.  The operations that need to have the operand checked jave
// code <= DATA.  The higher operations either don't need an operand
// (COMMENT) or need to be checked individually (DIRECTIVE).

function checkOpOp (m,s) {
    console.log(`checkOpOp line=${s.lineNumber} <${s.srcLine}>`);
    let format = s.format; // format required by the operation
    let operandType = s.operandType; // type of operand
    console.log (`checkOpOp format=${format} operandType=${operandType}`);
    if (format==operandType
        || (format==RRREXP && operandType==RRR)
        || (format==RREXP && operandType==RR)
        || (format==EXP0)
        || (format==DirModule)
        || (format==DirImport && operandType==DATA)
        || (format==DirExport && operandType==DATA)
        || (format==DirOrg && operandType==DATA)
        || (format==DirEqu && operandType==DATA)
        || (format==EMPTY)
        || (format==UNKNOWN)
        || (format==COMMENT)) {
        return true;
    } else {
	let msg = `${s.fieldOperation} is ${showFormat(format)} format, but operand type is ${showFormat(operandType)}`;
	mkErrMsg (m,s,msg);
        return false;
    }
}
//    if (format <= DATA && format != operandType) {
//    if (format > 11) {return true}


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
			'---  RX d=' + x.d + ' disp=' + x.field_disp + ' idx=' + x.idx);
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
    objectWordBuffer = [];
    relocationAddressBuffer = [];
    for (let i = 0; i < m.asmStmt.length; i++) {
	s = m.asmStmt[i];
	fmt = s.format;
	console.log(`pass2 line=${s.lineNumber} s=/${s.srcLine}/`);
	console.log(`pass2 line=${s.lineNumber} fmt=${fmt}`
                    + `opcode=${s.operation.opcode}`);
	console.log(`pass2 line=${s.lineNumber} operation=${s.operation}`
                    + `operandType=${s.operandType}`);
	checkOpOp (m,s);
	if (fmt==RR) { // Rp,Rq
            // cmp has d=0, a=p, b=q
            // inv has d=p, a=q, b=0
	    console.log (`pass2 RR`);
	    op = s.operation.opcode;
            s.field_d = op==opcode_cmp ? 0 : s.operand_str1;
            s.field_a = op==opcode_cmp ? s.operand_str1 : s.operand_str2;
            s.field_b = op==opcode_cmp ? s.operand_str2 : 0;
	    s.codeWord1 = mkWord(op[0],s.field_d,s.field_a,s.field_b);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    console.log (`pass2 op=${op} ${wordToHex4(s.codeWord1)}`);
	} else if (fmt==RREXP) {
	    console.log (`pass2 RREXP`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // Rp
            s.field_e = s.operand_str2; // Rq
            s.field_f = 0;
            //            s.field_g = s.fieldOperation=="invnew" ? 12 : 0;
            s.field_g = logicFunction(s.fieldOperation);
            s.field_h = 0;
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==RCEXP) {
	    console.log ("pass2 RCEXP");
	    op = s.operation.opcode;
            s.field_e = s.operand_str1;
	    let ctlRegName = s.operand_str2;
	    let ctlRegIdx = findCtlIdx (m,s,s.operand_str2);
            s.field_f = ctlRegIdx;
	    s.codeWord1 = mkWord448(14,0,op[1]);
	    s.codeWord2 = mkWord(s.field_e,s.field_f,0,0);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==RRR) { // Rp,Rq,Rr
	    console.log (`pass2 RRR`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // Rp
            s.a = s.operand_str2; // Rq
            s.b = s.operand_str3; // Rr
	    s.codeWord1 = mkWord(op[0],s.d,s.a,s.b);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	} else if (fmt==RRKEXP) {
	    console.log (`pass2 RRKEXP`);
	    console.log (`d=${s.d} a=${s.a} k=${s.k}`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // dest
            if (s.operation.pseudo) { // pseudo instruction: invb
                s.field_e = s.operand_str2; // reg operand
                s.field_f = 0;
                s.field_g = op[2]; // logic function (should be 12 for inv)
                s.field_h = s.operand_str3; // bit index
            } else { // not pseudo
                s.field_e = s.operand_str2; // reg operand
                s.field_f = 0;
                s.field_g = s.operand_str3; // const
                s.field_h = 0;
            }
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
	    s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==RRKKEXP) {
	    console.log (`pass2 RRKKEXP`);
	    console.log (`d=${s.d} a=${s.a} k=${s.k}`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // Rp
            s.field_e = s.operand_str2; // reg operand
            s.field_f = 0;
            s.field_g = s.operand_str3; // start bit index
            s.field_h = s.operand_str4; // end bit index
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
	    s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
        } else if (fmt==RRRKEXP) {
            console.log (`pass2 RRRKEXP`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // dest reg
            if (s.operation.pseudo) { // pseudo, andb/orb/xorb
                s.field_e = s.operand_str2; // reg operand
                s.field_f = s.operand_str3;
                s.field_g = op[2]; // logic function
                s.field_h = s.operand_str4; // bit index
            } else { // not pseudo
                s.field_e = s.operand_str2; // first operand reg
                s.field_f = s.operand_str3; // second operand reg
                s.field_g = s.operand_str4; // function
                s.field_h = 0; // unused
            }
            s.codeWord1 = mkWord448(op[0],s.d,op[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
        } else if (fmt==RRRKKEXP) {
            console.log (`pass2 RRRKKEXP`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // dest reg
            s.field_e = s.operand_str2; // first operand reg
            s.field_f = s.operand_str3; // second operand reg
            s.field_g = s.operand_str4; // function
            s.field_h = s.operand_str5; // unused
            console.log(`RRRKK op=${op[0]} opx=${op[1]} d=${s.d}`);
            console.log(`RRRKK e=${s.field_e}  f=${s.field_f}  g=${s.field_g}  h=${s.field_h} `);
            s.codeWord1 = mkWord448(op[0],s.d,op[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==JX) {
	    console.log (`pass2 JX`);
	    op = s.operation.opcode;
	    s.field_disp = s.operand_str1;
	    s.field_a = s.operand_str2;
	    s.codeWord1 = mkWord(op[0],op[2],s.field_a,op[1]);
            let v = evaluate(m,s,s.address+1,s.field_disp);
	    s.codeWord2 = v.evalVal;
            if (v.evalRel) {
                console.log (`relocatable displacement`);
                generateRelocation (m, s, s.address+1);
            }
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==RX) {
	    console.log (`pass2 RX`);
	    op = s.operation.opcode;
            s.field_d = s.operand_str1;
	    s.field_disp = s.operand_str2;
	    s.field_a = s.operand_str3;
            s.codeWord1 = mkWord(op[0],s.field_d,s.field_a,op[1]);
            let v = evaluate(m,s,s.address+1,s.field_disp);
	    s.codeWord2 = v.evalVal;
            if (v.evalRel) {
                console.log (`relocatable displacement`);
                generateRelocation (m, s, s.address+1);
            }
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==KX) {
	    console.log (`pass2 KX`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],s.d,s.a,op[1]);
            let v = evaluate(m,s,s.address+1,s.field_disp);
	    s.codeWord2 = v.evalVal;
            if (v.evalRel) {
                console.log (`relocatable displacement`);
                generateRelocation (m, s, s.address+1);
            }
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==EXP0) {
	    console.log (`pass2 EXP0`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord448(op[0],0,op[1]);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	} else if (fmt==RRREXP) {
	    console.log (`pass2 RRREXP`);
	    op = s.operation.opcode;
            s.d  = s.operand_str1;   // first register
	    s.field_e  = s.operand_str2;   // second register
	    s.field_f  = s.operand_str3;   // second register
	    s.field_g  = logicFunction(s.fieldOperation);
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,0);
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
	} else if (fmt==RRXEXP) {
	    console.log (`pass2 RRXEXP`);
	    op = s.operation.opcode;
            s.field_e  = s.operand_str1;      // start register
	    s.field_f  = s.operand_str2;      // start register
	    s.field_gh = s.operand_str3;      // offset (8 bits)
	    s.field_a  = s.operand_str4;      // index register
            let v = evaluate(m,s,s.address+1,s.field_gh); // value of offset
	    s.codeWord1 = mkWord448(op[0],s.field_a,op[1]);
            s.codeWord2 = mkWord(0,s.field_e,s.field_f,s.field_gh);
            s.codeWord2 = mkWord448(s.field_e,s.field_f,v.evalVal);
            if (v.evalRel) {
                mkErrMsg (m,s, `This instruction requires constant displacement\n`
                          + `       but ${s.field_disp} is relocatable`);
            }
	    generateObjectWord (m, s, s.address, s.codeWord1);
	    generateObjectWord (m, s, s.address+1, s.codeWord2);
        } else if (fmt==DATA) {
            console.log (`pass2 data ${s.dat}`);
            let v = evaluate(m,s,s.address,s.dat);
            console.log (v);
            s.codeWord1 = v.evalVal;
	    generateObjectWord (m, s, s.address, s.codeWord1);
            if (v.evalRel) {
                console.log (`relocatable data`);
                generateRelocation (m, s, s.address);
            }
        } else if (fmt==DirModule) {
            console.log ('pass2 module statement')
            // require that no code has yet been generated ???
            let modname = s.fieldLabel;
            m.objectCode.push (`module   ${modname}`)
        } else if (fmt==DirImport) {
            console.log ('pass2 import statement, handled in pass 1')
        } else if (fmt==DirExport) {
            console.log ('pass2 export statement')
            console.log (`export identarg=${s.identarg}`);
            m.exports.push(s.identarg);
        } else if (fmt==DirOrg) {
            console.log ('pass2 org statement, not yet implemented')
        } else if (fmt==DirEqu) {
            console.log ('pass2 equ statement, not yet implemented')
	} else {
	    console.log('pass2 other, noOperation');
	}
	s.listingLinePlain =  (s.lineNumber+1).toString().padStart(4,' ')
	    + ' ' + wordToHex4(s.address)
	    + ' ' + (s.codeSize>0 ? wordToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize>1 ? wordToHex4(s.codeWord2) : '    ')
//	    + ' ' + s.srcLine
            + ' '
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
            + ' '
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
    emitObjectWords (m);
    emitRelocations (m);
    emitImports (m);
    emitExports (m);
}

// Add object word x to buffer for next object code line in module m.
// The asm statement is s, and the object word will be loaded at
// address a.  The line number entered into asmap is ln = s.lineNumber
// + 1 to account for the header line that is inserted before the
// statements in the listing.

function generateObjectWord (m, s, a, x) {
    let ln = s.lineNumber + 2;  // adjust for <span> line and header line
    m.asmap[a] = ln;
    console.log (`generateObjectWord (${x}) asmap[${a}]=${ln}`);
    objectWordBuffer.push (x);
}

function emitObjectWords (m) {
    let xs, ys, zs;
    while (objectWordBuffer.length > 0) {
        xs = objectWordBuffer.splice(0,objBufferLimit);
        ys = xs.map( (w) => wordToHex4(w));
        zs = 'data     ' + ys.join(',');
        m.objectCode.push (zs);
    }
}

// Record an address that needs to be relocated
function generateRelocation (m, s, a) {
    console.log (`generateRelocation ${a}`);
    relocationAddressBuffer.push (a);
}

// Generate the relocation statements in object code
function emitRelocations (m) {
    let xs, ys, zs;
    while (relocationAddressBuffer.length > 0) {
        let xs = relocationAddressBuffer.splice(0,objBufferLimit);
        let ys = xs.map((w) => wordToHex4(w));
        let zs = 'relocate ' + ys.join(',')
        m.objectCode.push (zs);
    }
}

// ???? improve iteration over symbol table, this is awkward.  also
// see displaySymbolTableHtml
function emitImports (m) {
    console.log ('emitImports');
    m.symbols =[ ...m.symbolTable.keys() ].sort();
    console.log (`emitImports m.symbols=${m.symbols}`);
    for (i in m.symbols) {
        let x = m.symbolTable.get(m.symbols[i]);
        console.log (`emitImports i=${i} symname=${x.symbol}`)
        if (x.symIsImport) {
            emitImportAddresses (m,x)
        }
    }
}

function emitImportAddresses (m,x) {
    console.log (`emitImportAddresses ${x.symbol}`);
    let xs, ys, zs;
    while (x.symUsageAddrs.length > 0) {
        let xs = x.symUsageAddrs.splice(0,objBufferLimit);
        let ys = xs.map((w) => wordToHex4(w));
        let zs = 'import   '
            + x.symImportedFrom
            + ',' + x.symbol
            + ',' + ys.join(',');
        m.objectCode.push (zs);
    }
}

function emitExports (m) {
    let x, sym, v, r;
    console.log ('emitExports' + m.exports);
    while (m.exports.length > 0) {
        x = m.exports.splice(0,1);
        y = x[0];
        console.log (`emit exports looking up  x=<${x}> y=${y}`);
        console.log (m.symbolTable);
        sym = m.symbolTable.get(y);
        if (sym) {
            v = wordToHex4(sym.val);
            m.objectCode.push (`export   ${y},${v}`);
        } else {
            console.log (`\n\n\n ERROR export error ${x}\n\n\n`);
        }
    }
}

function showAsmap (m) {
    console.log ('showAsmap');
    for (var i in m) {
	console.log ('[' + i + '] = ' + m[i]);
    }
}

// Evaluate an expression, which may be used as a displacement, data
// value, or equ value.  The expression may be a decimal constant, a
// hex constant, or a label.  Return a tuple containing the value
// (evalVal) and a boolean (evalRel) indicating whether the value is
// relocatable.

// Arguments: m is module, s is statement, a is address where the
// value will be placed (the address a is passed in because the word
// being evaluated could appear in the second word of an instruction
// (for RX etc), or any word (in the case of a data statement)).

// Evaluate returns a word which will be inserted into the object code
// during pass 2.  This could be the actual final value (if it's a
// relocatable label) or a placeholder value of 0 (if it's an import).
// Evaluate also records additional information about any symbols that
// appear in the expression: the definition line (used for printing
// the symbol table in the assembly listing) and the (relocatable)
// address where the symbol appears (to enable the linker to insert
// the values of imports).  If an imported name appears in an
// expression, the expression must consist entirely of that name: for
// example, x+1 is legal if x is a local name but not if x is an
// import.

function evaluate (m,s,a,x) {
    console.log('evaluate ' + x);
    let result = 0;
    if (!x.search) {
        mkErrMsg (m, s, `Cannot evaluate expression (search failed), using 0`);
        return 0;y
    }
    if (x.search(nameParser) == 0) { // expression is a name
	r = m.symbolTable.get(x);
	if (r) {
	    console.log('evaluate returning ' + r.val);
            r.symUsageAddrs.push(a);
            r.symUsageLines.push(s.lineNumber+1);
            //	    return { evalVal : r.val, evalRel : r.relocatable };
            result = { evalVal : r.val, evalRel : r.relocatable };
	} else {
	    mkErrMsg (m, s, 'symbol ' + x + ' is not defined');
	    console.log('evaluate returning ' + 0);
            //	    return {evalVal : 0, evalRel : false};
            result = {evalVal : 0, evalRel : false};
	}
    } else if (x.search(intParser) == 0) { // expression is an int literal
	console.log('evaluate returning ' + parseInt(x,10));
        //	return {evalVal : intToWord(parseInt(x,10)), evalRel : false};
        result = {evalVal : intToWord(parseInt(x,10)), evalRel : false};
    } else if (x.search(hexParser) == 0) { // expression is a hex literal
        //	return {evalVal : hex4ToWord(x.slice(1)), evalRel : false};
        result =  {evalVal : hex4ToWord(x.slice(1)), evalRel : false};
    } else { // compound expression (not yet implemented)
	mkErrMsg (m, s, 'expression ' + x + ' has invalid syntax');
        //	return {evalVal : 0, evalRel : false};
        result = {evalVal : 0, evalRel : false};
    }
    if (result) {
        return result
    } else {
        mkErrMsg (m, s, `Cannot evaluate expression, using 0`);
        return 0;
    }
}

function setAsmListing (m) {
    let listing = getCurrentModule().asmListingDec.join('\n');
    document.getElementById('AsmTextHtml').innerHTML = listing;
}

function setObjectListing (m) {
    let listing = "<pre class='HighlightedTextAsHtml'>"
        + getCurrentModule().objectCode.join('<br>')
        + "</pre>";
    document.getElementById('AsmTextHtml').innerHTML = listing;
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

