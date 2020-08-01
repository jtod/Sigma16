// Sigma16: assembler.mjs
// Copyright (C) 2020 John T. O'Donnell
// email: john.t.odonnell9@gmail.com
// License: GNU GPL Version 3 or later. See Sigma16/README.md, LICENSE.txt

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

//------------------------------------------------------------------------------
// assembler.mjs translates assembly language to machine language
//------------------------------------------------------------------------------

import * as com from './common.mjs';
import * as smod from './s16module.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as st from './state.mjs';
import * as ed from './editor.mjs';

// TODO
let opcode_cmp = 4; // refactor, see pass2/fmt==arch.RR

//------------------------------------------------------------------------------
// Global
//------------------------------------------------------------------------------

// Buffers to hold generated object code

let objBufferLimit = 16;             // how many code items to allow per line
let objectWordBuffer = [];          // list of object code words
let relocationAddressBuffer = [];   // list of relocation addresses

//-----------------------------------------------------------------------------
// Assembler state
//-----------------------------------------------------------------------------

// The state is kept in an object created by mkModuleAsm, and stored
// in module m as m.asmInfo

export function mkModuleAsm () {
    com.mode.devlog("mkModuleAsm");
    return {
	modName : "anon",       // name of module specified in module stmt
        text : "",                 // raw source text
        asmSrcLines : [],
	asmStmt : [],               // statements correspond to lines of source
	symbols : [],              // symbols used in the source
	symbolTable : new Map (),  // symbol table
	locationCounter : 0,       // address where next code will be placed
	asmListingPlain : [],      // assembler listing
	asmListingDec : [],        // decorated assembler listing
	objectCode : [],           // string hex representation of object
        objectText : "",           // object code as single string
        metadata : [],             // lines of metadata code
        metadataText : "",         // metadata as single string
        asArrMap : [],             // address-sourceline map
        imports : [],              // list of imported identifiers
        exports : [],              // list of exported identifiers
        modAsmOK : false, // deprecated, use nAsmErrors===0
	nAsmErrors : 0             // number of errors in assembly source code
    }
}

//-----------------------------------------------------------------------------
// Character set
//-----------------------------------------------------------------------------

// CharSet is a string containing all the characters that may appear
// in a valid Sigma16 assembly language source program.  It's always
// best to edit source programs using a text editor, not a word
// processor.  Word processors are likely to make character
// substitutions, for example en-dash for minus, typeset quote marks
// for typewriter quote marks, and so on.  Spaces rather than tabs
// should be used for indentation and layout.

const CharSet =
      "_abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ" // letters
      + "0123456789"              // digits
      + " \t,;\n"                 // separators
      + '"'                       // quotes
      + "'"                       // quotes
      + ".$[]()+-*"               // punctuation
      + "?¬£`<=>!%^&{}#~@:|/\\";  // other

// removeCR copies a string with all \r characters removed.  Given
// string xs, return the string with all CR characters removed.  In
// Unix/Linux a line ends with \n, but in Windows lines end with \r\n
// (CRLF).  The regular expressions used for parsing assume \n as the
// line terminator;

function removeCR (xs) {
    return xs.split("").filter (c => c.charCodeAt(0) != 13).join("");
    }

// Check that the source code contains only valid characters (defined
// to be characters in CharSet).  Do this check after removing \r
// (carriage return) characters.

function validateChars (xs) {
    com.mode.devlog (`validateChars`);
    let i, c;
    let badlocs = [];
    for (i = 0; i < xs.length; i++) {
        c = xs.charAt(i);
        if (!CharSet.includes(c)) {
            com.mode.errlog (`validateChars: bad char at ${i} in ${xs}`);
            badlocs.push(i);
            com.mode.devlog (`i=${i} charcode=${xs.charCodeAt(i)}`);
        }
    }
    return badlocs
}

//-----------------------------------------------------------------------------
// Values
//-----------------------------------------------------------------------------

// A value is a number that can be used in the displacement field of
// an instruction.  An expression is assembly language syntax that
// specifies a value.  The assembler evaluates expressions to
// calculate the corresponding value.

// A value is anonymouse, although it may be bound to a name as an
// Identifier.  The Value itself consists of a number (the value
// field) and a Boolean indicating whether the value is relocatable
// (if this is false, the value is fixed).

export class Value {
    constructor (value, isRelocatable) {
        this.value = value;
        this.isRelocatable = isRelocatable;
    }
    getval () {
        return this.value;
    }
    update (v) {
        this.value = v;
    }
    setrel (r) {
        this.isRelocatable = r;
    }
    show () {
        //        return (`(Value ${this.value} ${this.isRelocatable})`);
        return `(Value ${this.value} ${this.isRelocatable ? "R" : "F"})`;
    }
}

//-----------------------------------------------------------------------------
// Symbol table
//-----------------------------------------------------------------------------

// An identifier is a named Value; the name comes from the label field
// of a statement.  There are three kinds of identifier:

//   - IdeLocation is an ordinary symbol whose value is the location counter
//   - IdeImport has the value is provided by linker, provisionally 0
//   - IdeEqu is the value of an expression

const IdeLocation = Symbol ("IdeLocation"); // location counter
const IdeImport   = Symbol ("IdeImport");   // set by linker
const IdeEqu      = Symbol ("IdeEqu");      // evaluation of operand expression

// Show identifier origin as a string
function showSymOrigin (s) {
    return ( s === IdeLocation ? "Local"
             : s === IdeImport ? "Import"
             : s === IdeEqu    ? "Equ"
             : "Unknown" )
}


// Fields of an identifier:
//   name = identifier text from label field
//   defLine = number of the source line where it's defined
//   origin= IdeLocation, IdeImport, or IdeEqu
//   value = a word which is the value
//   expr = operand text (used only for equ), Nothing otherwise
//   usageLines = list of line numbers where identifier is used

// When created, an identifier must have a name and an origin (because
// it's created When a statement is found that has a label, a symbol
// is created and added to the symbol table.

// name is label field, defline is line number, v is Value
class Identifier {
    constructor (name, defLine, v) {
        this.name = name;
        this.value = v;
        this.origin = IdeLocation; // assumption; override if import or equ
        this.defLine = defLine;
        this.usageLines = [];
    }
    update(v) {
        com.mode.devlog (`Identifier ${this.name} setting value := ${v}`);
        this.value = v;
    }
    show () {
        return ("(Identifier " + this.name + " " + this.value.show() + ")");
    }
}

// dev tool
function displaySymbolTable (ma) {
    console.log ("Symbol table");
    console.log (ma.symbolTable.keys());
    let foox = ma.symbolTable.get("x");
}

function displaySymbolTableHtml (ma) {
    ma.asmListingText.push('');
    ma.asmListingPlain.push('');
    ma.asmListingDec.push('');
    ma.asmListingText.push("Symbol table");
    ma.asmListingPlain.push("<span class='ListingHeader'>Symbol table</span>");
    ma.asmListingDec.push("<span class='ListingHeader'>Symbol table</span>");
    ma.asmListingText.push("Name      Val      Def   Used");
    ma.asmListingPlain.push("<span class='ListingHeader'>Name      Val      Def   Used</span>");
    ma.asmListingDec.push("<span class='ListingHeader'>Name      Val      Def   Used</span>");
    ma.symbols =[ ...ma.symbolTable.keys() ].sort();
    com.mode.devlog('symbols = ' + ma.symbols);
    for (let i = 0; i < ma.symbols.length; i++) {
	let x = ma.symbolTable.get(ma.symbols[i]);
        let xs = ma.symbols[i].padEnd(10)
	    + arith.wordToHex4(x.value.value)
            + (x.value.isRelocatable ? ' R' : ' F')
	    + x.defLine.toString().padStart(5)
            + '  '
            + x.usageLines.join(',');
        ma.asmListingText.push(xs);
        ma.asmListingPlain.push(xs);
        ma.asmListingDec.push(xs);
    }
}
//            + x.symUsageAddrs.map((w)=>arith.wordToHex4(w)).join(',');

//-----------------------------------------------------------------------------
// Expressions
//-----------------------------------------------------------------------------

// Expressions are assembly language syntax to define values that
// appear in the machine language code.  Expression evaluation occurs
// at assembly time.  The object code contains only words, but cannot
// contain expressions.

//   nonnegative decimal integer:   0, 34       fixed
//   negative decimal integer:      -103        fixed
//   identifier:                    xyz, loop   fixed or relocatable
//   Later, will allow limited arithmetic on expressions

// The evaluator takes an expression and environment and returns a
// value, which may be either fixed or relocatable.

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

// s and a are needed to record the usage line of any identifiers that
// occur in x.

function evaluate (ma, s, a, x) {
    com.mode.devlog(`evaluate ${x} ${typeof(x)}`);
    let result;
    if (x.search(nameParser) == 0) { // identifier
	let r = ma.symbolTable.get(x);
	if (r) {
            result = r.value; // identifier already has a value, return it
            r.usageLines.push (s.lineNumber+1);
            com.mode.devlog (`evaluate identifier => ${result.show()}`);
	} else {
            mkErrMsg (ma, s, 'symbol ' + x + ' is not defined');
	    com.mode.devlog('evaluate returning ' + 0);
            result = new Value (0, false);
	}
    } else if (x.search(intParser) == 0) { // integer literal
	com.mode.devlog('evaluate returning ' + parseInt(x,10));
        result = new Value (arith.intToWord(parseInt(x,10)), false);
    } else if (x.search(hexParser) == 0) { // hex literal
        result =  new Value (arith.hex4ToWord(x.slice(1)), false);
    } else { // compound expression (not yet implemented)
        mkErrMsg (ma, s, 'expression ' + x + ' has invalid syntax');
        result = new Value (0, false);
    }
    com.mode.trace = true;
    com.mode.devlog (`evaluate received expression ${x}`)
    com.mode.devlog (`evaluate returning ${result.show()}`)
    com.mode.trace = false;
    return result;
}

//-----------------------------------------------------------------------------
// Assembly language statement
//-----------------------------------------------------------------------------

// Each statement has a listing line which contains the line number,
// object code, and source code.  There are two versions of this:
// listingLinePlain just contains the text of the listing line, while
// listingLineHighlightedFields contains <span> elements to enable the
// various fields to be highlighted with colors.

function showAsmStmt (s) {
    com.mode.devlog (`*** showAsmStmt line=${s.lineNumber}`);
    com.mode.devlog (`*** srcLine=${s.srcLine}`);
    com.mode.devlog (`*** address=${s.address}`);
}

function mkAsmStmt (lineNumber, address, srcLine) {
    return {lineNumber,                    // array index of statement
	    address,                       // address where code goes
	    srcLine,                       // source line
	    listingLinePlain: "",          // object and source text
	    listingLineHighlightedFields : "", // listing with src field spans
	    fieldLabel : '',               // label
	    fieldSpacesAfterLabel : '',    // white space
	    fieldOperation : '',           // operation mnemonic
	    fieldSpacesAfterOperation : '', // white space
	    fieldOperands : '',             // operands
	    fieldComment : '',             // comments are after operand or ;
	    hasLabel : false,              // statement has a valid label
	    operation : arch.NOOPERATION,       // spec of the operation if exists
	    format : arch.UNKNOWN,
	    hasOperand : false,            // statement contains an operand
	    operandType : arch.NOOPERAND,       // type of operand actually present
//	    operandRRR : null,            // statement contains RRR operand
	    operandRR : null,             // statement contains RR operand
	    operandRX : null,             // statement contains RX operand
//	    operandJX : null,             // statement contains short form RX
	    operandDATA : false,           // statement contains data
            operandX : false,              // expression operand for directive
            operandIDENT : false,          // identifier directive
            expSrc : null,            // expression operand if any
            expValue : null,          // value of expression operand if any
	    op : 0,                        // operation
            operand_str1 : '',
            operand_str2 : '',
            operand_str3 : '',
            operand_str4 : '',
            operand_str5 : '',
            field_d : 0,
            field_a : 0,
            field_b : 0,
            field_disp : 0,
            field_e : 0,                   // EXP: word 2, first 4 bits
            field_f : 0,                   // EXP: word 2, second 4 bits
            field_g : 0,                   // EXP: word 2, third 4 bits
            field_h : 0,                   // EXP: word 2, fourth 4 bits
	    field_gh : 0,                  // EXP: word 2, last 8 bits
	    dat : 0,                       // data value
            identarg : '',                 // identifier operand
	    codeSize : 0,                  // number of words generated
	    codeWord1 : -1,                // first word of object
	    codeWord2 : -1,                // second word of object
	    errors : []                    // array of lines of error messages
	   }
}

// Print the object representing a source line; for testing

function printAsmStmt (ma,x) {
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
		 '  operation requires ' + arch.showFormat(x.operation.format)
		 : '  no operation');
//    console.log ('  operand RRR=' + x.operandRRR
//		 + ' RR=' + x.operandRR
//		 + ' RX=' + x.operandRX
//		 + ' JX=' + x.operandJX
//		 + ' DATA=' + x.operandDATA);
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

//-----------------------------------------------------------------------------
//  Error messages
//-----------------------------------------------------------------------------

// Report an assembly error: s is an assembly source line, err is an
// error message

function mkErrMsg (ma,s,err) {
    console.log (err);
    s.errors.push(err);
    ma.nAsmErrors++;
}

//-----------------------------------------------------------------------------
// Interfaces to the assembler
//-----------------------------------------------------------------------------

// When Assembler pane is entered, if an AsmInfo doesn't exist then
// create one and insert it into the current module.  Copy the text of
// the current module into the AsmInfo.

export function enterAssembler () {
    let m = smod.getSelectedModule ();
    if (!m.asmInfo) {
        m.asmInfo = mkModuleAsm ();
    }
    m.asmInfo.asmSrcLines = m.text.split('\n');
}

// Interface to assembler for use in the GUI

export function assemblerGUI () {
    com.mode.devlog ("assemblerGUI starting");
    let m = smod.getSelectedModule ();
    let ma =  mkModuleAsm ();
    m.asmInfo = ma;
    ma.text = m.text;
    document.getElementById('AsmTextHtml').innerHTML = ""; // clear text in asm
    document.getElementById('ProcAsmListing').innerHTML = ""; // clear text in proc
    com.clearObjectCode (); // clear text in linker pane
    assembler (ma);
    setAsmListing (m);
    if (ma.nAsmErrors > 0) {
        document.getElementById('ProcAsmListing').innerHTML = "";
    }
}
//    if (ma.nAsmErrors > 0) {
//    let ma = m.asmInfo;
//    ma.asmSrcLines = document.getElementById('EditorTextArea').value.split('\n');
    
// Interface to assembler for use in the command line interface

export function assemblerCLI (src) {
    com.mode.devlog ("assemblerCLI starting");
    let ma = mkModuleAsm ();
    ma.text = src;
    assembler (ma);
    return ma;
 }

//-----------------------------------------------------------------------------
//  Assembler
//-----------------------------------------------------------------------------

// assembler translates assembly language source code to object code,
// and also produces an assembly listing and metadata.  The source is
// obtained from the ma object, and the results are placed there.
// This is the main translator, used for both gui and cli Input:
// ma.asmSrcLines contains array of lines of source code Result:
// define the fields in ma

export function assembler (ma) {
//    let ma = m.asmInfo;
    let src = ma.text;
    let src2 = removeCR (src);
//    let badlocs = validateChars (src2);
    ma.asmSrcLines = src2.split("\n");
    com.mode.devlog (`assembler nloc=${ma.asmSrcLines.length}`);

    ma.modName = null;
    ma.nAsmErrors = 0;
    ma.asmStmt = [];
    ma.symbols = [];
    ma.asmListingText = [];
    ma.asmListingPlain = [];
    ma.asmListingDec = [];
    ma.objectCode = [];
    ma.exports = [];
    ma.locationCounter = 0;
    ma.asArrMap = [];
    ma.symbolTable.clear();
    ma.asmListingText.push ("Line Addr Code Code Source");
    ma.asmListingPlain.push(
	"<span class='ListingHeader'>Line Addr Code Code Source</span>");
    ma.asmListingDec.push(
	"<span class='ListingHeader'>Line Addr Code Code Source</span>");
    asmPass1 (ma);
    if (com.mode.trace) displaySymbolTable (ma);
    asmPass2 (ma);
    if (ma.nAsmErrors > 0) {
	ma.asmListingText.unshift
          (`\n ${ma.nAsmErrors} errors detected\n`,'ERR');
        	ma.asmListingPlain.unshift (com.highlightField
           (`\n ${ma.nAsmErrors} errors detected\n`,'ERR'));
        	ma.asmListingDec.unshift (com.highlightField
           (`\n ${ma.nAsmErrors} errors detected\n`,'ERR'));
    }
    ma.asmListingPlain.unshift("<pre class='HighlightedTextAsHtml'>");
    ma.asmListingDec.unshift("<pre class='HighlightedTextAsHtml'>");
    displaySymbolTableHtml(ma);
    ma.asmListingPlain.push("</pre>");
    ma.asmListingDec.push("</pre>");
}

//-----------------------------------------------------------------------------
//  Regular expressions for the parser
//-----------------------------------------------------------------------------

// Syntax of assembly language

// a constant value is
// a label is   (?:[a-zA-Z][a-zA-Z0-9]*)
// $ followed by 4 hex digits      (?:\$[0-9a-f]{4})
//  a decimal number with optional sign   (?:-?[0-9]+)

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

// A register is R followed by register number, which must be either
// a 1 or 2 digit decimal number between 0 and 15, or a hex digit.

const parseReg = /R[0-9a-f]|(?:1[0-5])/;

// An aRRR operand consists of three registers, separated by comma
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

// aRCEXP asm format (register, control reg name): getctl R3,mask
const rcParser =
    /^R([0-9a-f]|(?:1[0-5])),([a-zA-Z][a-zA-Z0-9]*)$/;

const nameNameParser = // import statement
      /^([a-zA-Z][a-zA-Z0-9_]*),([a-zA-Z][a-zA-Z0-9_]*)$/;
      
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

const regExpField = '((?:(?:(?:"(?:(?:\\")|[^"])*")|[^\\s";])+)?)';

// A field may contain non-space characters, and may contain a string
// literal, but cannot contain a space or ; (unless those appear in a
// string literal).  The field terminates as soon as a space or ; or
// end of the line is encountered.

// Simplified version of field: don't allow string literals
const regexpFieldNoStringLit =  '((?:[^\\s";]*)?)'

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

//-----------------------------------------------------------------------------
//  Parser
//-----------------------------------------------------------------------------

// Parse the source for line i and update the object with the results.
// Each source line is a statement; a statement consists of a sequence
// of fields and whitespace-fields.  Each is optional.  The structure
// is:

// label, whitespace, operation, whitespace, operands, whitespace

function parseAsmLine (ma,i) {
    let s = ma.asmStmt[i];
    com.mode.devlog (`parseAsmLine i=${i}`);
    showAsmStmt(s);
    com.mode.devlog ("about to parse splitfields");
    let p = parseSplitFields.exec(s.srcLine);
    com.mode.devlog (`p = ${p})`);
    s.fieldLabel = p[1];
    s.fieldSpacesAfterLabel = p[2];
    s.fieldOperation = p[3];
    s.fieldSpacesAfterOperation = p[4];
    s.fieldOperands = p[5];
    s.fieldComment = p[6];
    com.mode.devlog(`label = /${s.fieldLabel}/`);
    com.mode.devlog(`operation = /${s.fieldOperation}/`);
    com.mode.devlog(`operands = /${s.fieldOperands}/`);
    com.mode.devlog(`comments = /${s.fieldComment}/`);
    parseLabel (ma,s);
    parseOperation (ma,s);
    parseOperand (ma,s);
    if (s.hasLabel) {
	if (ma.symbolTable.has(s.fieldLabel)) {
            mkErrMsg (ma, s, s.fieldLabel + ' has already been defined');
	} else {
            let isImport = s.fieldOperation=="import";
            let isEqu = s.fieldOperation=="equ";
            let v = isImport ? new Value (s.fieldOperands, false) // val=mod
                : isEqu ? evaluate (ma, s, ma.locationCounter, s.fieldOperands)
                : new Value (ma.locationCounter, true);
            ma.symbolTable.set (s.fieldLabel,
                                new Identifier (s.fieldLabel, s.lineNumber+1, v));
            if (isImport) {
                ma.imports.push(s.fieldLabel);
                // The label is requried in an import, operand field is
                // modname,nameInThatModule
                // examples
                //     foo  import Mod3,bar    it's called foo here
                //      baz  import Mod4,baz    it's called baz here and in Mod4
            }
	}
    }
}

// Set hasLabel to true iff there is a syntactically valid label.  If
// the label field isn't blank but is not syntactically valid
// (i.e. doesn't match the regular expression for names), then
// generate an error message.

function parseLabel (ma,s) {
    if (s.fieldLabel == '') {
	s.hasLabel = false;
    } else if (s.fieldLabel.search(nameParser) == 0) {
	s.hasLabel = true;
    } else {
	s.hasLabel = false;
        mkErrMsg(ma, s, s.fieldLabel + ' is not a valid label');
    }
}

// Set operation to the instruction set object describing the
// operation, if the operation field is defined in the map of
// operations.  Otherwise leave operation=null.  Thus s.operation can
// be used as a Boolean to determine whether the operation exists, as
// well as the specification of the operation if it exists.

function parseOperation (ma,s) {
    let op = s.fieldOperation;
    if (op !== '') {
	let x = arch.statementSpec.get(op);
	if (x) {
	    s.operation = x;
	    s.format = s.operation.format;
	    com.mode.devlog(`parse operation ${s.srcLine} fmt=${s.format}`);
	    s.codeSize = arch.formatSize(x.format);
	} else {
	    if (op.search(nameParser) == 0) {
                mkErrMsg (ma, s, `${op} is not a valid operation`);
	    } else {
                mkErrMsg (ma, s, `syntax error: operation ${op} must be a name`);
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

// An operand field contains a pattern that determines the type of
// operand (e.g. rrk or jx etc) and the actual text of each operand.
// The function operates by checking the regular expression for every
// operand type against the operand field text.  Most of these are
// disjoint but there are some exceptions; for example a valid operand
// for an import statement could also be interpreted as an operand for
// a data statement.

function parseOperand (ma,s) {
    let rc   = rcParser.exec (s.fieldOperands);
    let rr   = rrParser.exec  (s.fieldOperands);
//    let rrr  = rrrParser.exec (s.fieldOperands);
//    let rx   = rxParser.exec  (s.fieldOperands);
//    let jx   = jxParser.exec  (s.fieldOperands);
    let rrk  = rrkParser.exec (s.fieldOperands);
    let rkk  = rkkParser.exec (s.fieldOperands);  // need this parser
    let rrkk = rrkkParser.exec (s.fieldOperands);  // need this parser
    let rrrk = rrrkParser.exec (s.fieldOperands);
    let rrrkk = rrrkkParser.exec (s.fieldOperands);
    let kx   = kxParser.exec  (s.fieldOperands);
    let rrx  = rrxParser.exec  (s.fieldOperands);
    let dat  = datParser.exec (s.fieldOperands);
    let ident = identParser.exec (s.fieldOperands);  // identifier (directive)
    let nameName = nameNameParser.exec (s.fieldOperands);
//    console.log (`Testing parse_rrr`);
//    console.log (`calling parse_rrr => ${parse_rrr(s.fieldOperands)}`);
//        console.log (`calling parse_rrr => ${parse_rrr(s)}`);
// Check non-exclusive operand formats: ident
    if (ident) {
        s.hasOperand = true;
        s.operandIDENT = true;
        s.identarg = ident[0];
    }
// Check the mutually exclusive operand formats    
    if (rr) { // Rp,Rq
	s.hasOperand = true;
	s.operandType = arch.aRR;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operandRR = true;
	s.operand_str1 = rr[1]; // Rp
	s.operand_str2 = rr[2]; // Rq
    } else if (rc) { // Re,Cf
	com.mode.devlog ('rc');
	s.hasOperand = true;
	s.operandType = arch.aRCEXP;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operandaRCEXP = true;
	s.operand_str1 = rc[1]; // Re
	s.operand_str2 = rc[2]; // Cf
        /*
    } else if (rrr) { // Rp,Rq,Rr
	s.operandRRR = true;
	s.operand_str1 = rrr[1]; // Rp
	s.operand_str2 = rrr[2]; // Rq
	s.operand_str3 = rrr[3]; // Rr
*/
    } else if (rrk) { // Rd,Re,g
	com.mode.devlog ('rrk');
	s.hasOperand = true;
	s.operandType = arch.aRRKEXP;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operandaRRKEXP = true;
	s.operand_str1 = rrk[1]; // Re
	s.operand_str2 = rrk[2]; // Rf
	s.operand_str3 = rrk[3]; // k
//	s.field_ed = rrk[1];
//	s.a = rrk[2];
//	s.k = rrk[3];
    } else if (rkk) { // R2,R3,5,3
	com.mode.devlog ('rkk');
	s.hasOperand = true;
	s.operandType = arch.aRKKEXP;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operandRKKEXP = true;
	s.operand_str1 = rkk[1];
	s.operand_str2 = rkk[2];
	s.operand_str3 = rkk[3];
    } else if (rrkk) { // R2,R3,5,3
	com.mode.devlog ('rrkk');
	s.hasOperand = true;
	s.operandType = arch.aRRKKEXP;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operandaRRKEXP = true;
	s.operand_str1 = rrkk[1];
	s.operand_str2 = rrkk[2];
	s.operand_str3 = rrkk[3];
	s.operand_str4 = rrkk[4];
        com.mode.devlog (`pass1 rrkk 1=${s.operand_str1} 2=${s.operand_str2} 3=${s.operand_str3} 4=${s.operand_str4}`);
    } else if (rrrk) { // R2,R3,5,3
	com.mode.devlog ('rrrk');
	s.hasOperand = true;
	s.operandType = arch.aRRRKEXP;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operand_str1 = rrrk[1];
	s.operand_str2 = rrrk[2];
	s.operand_str3 = rrrk[3];
	s.operand_str4 = rrrk[4];
    } else if (rrrkk) { // R1,R2,R3,5,3
	com.mode.devlog ('rrrkk');
	s.hasOperand = true;
	s.operandType = arch.aRRRKKEXP;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operand_str1 = rrrkk[1];
	s.operand_str2 = rrrkk[2];
	s.operand_str3 = rrrkk[3];
	s.operand_str4 = rrrkk[4];
	s.operand_str5 = rrrkk[5];
    } else if (rrx) { // Re,Rf,gh[Ra]
        com.mode.devlog ('RRX');
	s.hasOperand = true;
	s.operandType = arch.aRRXEXP;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operandRX = true;
        com.mode.devlog (`rrx 1=${rrx[1]} 2=${rrx[2]} 3=${rrx[3]} 4=${rrx[4]} `)
        s.operand_str1 = rrx[1]; // Rd
        s.operand_str2 = rrx[2]; // Re
        s.operand_str3 = rrx[3]; // gh
        s.operand_str4 = rrx[4]; // Rf
        com.mode.devlog (`rrx e=${s.field_e} f=${s.field_f} disp=${s.field_disp} x=${s.d}`)
    } else if (nameName) { // modname,locname
        console.log (`found Name,Name operand`);
        s.operand_str1 = nameName[1]
        console.log (nameName[0]); // full operand
        console.log (nameName[1]); // modname
        console.log (nameName[2]); // locname
        console.log (nameName[3]);  // undefined
        console.log (nameName[4]);
        let nameName2 = nameNameParser.exec (s.fieldOperands);
        let {1:modname, 2:locname} = nameName2;
        console.log (`nameName, modname=${modname} locname=${locname}`);
    } else if (dat) { // 34
	s.hasOperand = true;
	s.operandType = arch.DATA;
        com.mode.devlog (`Found ${s.operandType} operand`);
	s.operandDATA = true;
	s.dat = dat[1];
        com.mode.devlog (`\n\nDAT`);
	com.mode.devlog('data 0' + dat[0]);
	com.mode.devlog('data 1' + dat[1]);
	com.mode.devlog('data 2' + dat[2]);
	com.mode.devlog('data 3' + dat[3]);
    } else {
        // other operand format may be multiple data values, equ
        // expression, import/export; these cases are handled in the
        // corresponding operation cases in pass2.
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

//-----------------------------------------------------------------------------
//  Assembler Pass 1
//-----------------------------------------------------------------------------

// Requires that asmSrcLines has been set to the source code, split
// into an array of lines of text

function asmPass1 (ma) {
    com.mode.devlog('assembler pass 1: ' + ma.asmSrcLines.length + ' source lines');
    for (let i = 0; i < ma.asmSrcLines.length; i++) {
        com.mode.devlog (`pass1 i=${i} line=<${ma.asmSrcLines[i]}>`);
	ma.asmStmt[i] = mkAsmStmt (i, ma.locationCounter, ma.asmSrcLines[i]);
	let s = ma.asmStmt[i];
        com.mode.devlog (`$$$ pass 1 i=${i}... stmt =...`);
        showAsmStmt(s);
        let badCharLocs = validateChars (ma.asmSrcLines[i]);
        com.mode.devlog (`validate chars: badCharLocs=${badCharLocs}`);

        if (badCharLocs.length > 0) {
            mkErrMsg (ma,s,`Invalid character at position ${badCharLocs}`);
            mkErrMsg (ma,s, "See User Guide for list of valid characters");
            mkErrMsg (ma,s, "(Word processors often insert invalid characters)");
        }
	com.mode.devlog(`pass1 i=  ${i} src= + ${s.srcLine}`);
        parseAsmLine (ma,i);
	ma.locationCounter += ma.asmStmt[i].codeSize;
    }
}

function printAsmStmts (ma) {
    for (let i = 0; i < ma.asmStmt.length; i++) {
	printAsmStmt(ma.asmStmt[i]);
    }
}

// If operand isn't correct type for the operation, give an error
// message.  The operations that need to have the operand checked jave
// code <= DATA.  The higher operations either don't need an operand
// (COMMENT) or need to be checked individually (DIRECTIVE).

function checkOpOp (ma,s) {
    com.mode.devlog(`checkOpOp line=${s.lineNumber} <${s.srcLine}>`);
    let format = s.format; // format required by the operation
    let operandType = s.operandType; // type of operand
    com.mode.devlog (`checkOpOp format=${format} operandType=${operandType}`);
    if (format==operandType
        || (format==arch.aRRREXP && operandType==arch.aRRR)
        || (format==arch.aRREXP && operandType==arch.aRR)
        || (format==arch.EXP0)
        || (format==arch.DirModule)
        || (format==arch.DirImport && operandType==arch.DATA)
        || (format==arch.DirExport && operandType==arch.DATA)
        || (format==arch.DirOrg && operandType==arch.DATA)
        || (format==arch.DirEqu && operandType==arch.DATA)
        || (format==arch.EMPTY)
        || (format==arch.UNKNOWN)
        || (format==arch.COMMENT)) {
        return true;
    } else {
	let msg = `${s.fieldOperation} is ${arch.showFormat(format)} format,`
        + ` but operand type is ${arch.showFormat(operandType)}`;
        mkErrMsg (ma,s,msg);
        return false;
    }
}

// Given a string xs, either return the control register index for it
// if xs is indeed a valid control register name; otherwise generate
// an error message.

function findCtlIdx (ma,s,xs) {
    let c = arch.ctlReg.get(xs);
    let i = 0;
    if (c) {
	i = c.ctlRegIndex;
    } else {
        mkErrMsg (ma,s,`${xs} is not a valid control register`);
    }
    com.mode.devlog (`findCtlIdx ${xs} => ${i}`);
    return i;
}

/*
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
*/

//-----------------------------------------------------------------------------
//  Pass 2
//-----------------------------------------------------------------------------

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
    console.log(arith.wordToHex4(mkWord(op,d,a,b)));
}

function asmPass2 (ma) {
    com.mode.devlog('assembler pass 2');
    let s, fmt,op,x;
    objectWordBuffer = [];
    relocationAddressBuffer = [];
    for (let i = 0; i < ma.asmStmt.length; i++) {
	s = ma.asmStmt[i];
        console.log (`pass 2 line ${i} <${s.srcLine}>`);
        s.codeWord1 = null;
        s.codeWord2 = null;
	fmt = s.format;
	com.mode.devlog(`pass2 line=${s.lineNumber} s=/${s.srcLine}/`);
	com.mode.devlog(`pass2 line=${s.lineNumber} fmt=${fmt}`
                    + `opcode=${s.operation.opcode}`);
	com.mode.devlog(`pass2 line=${s.lineNumber} operation=${s.operation}`);
//                    + `operandType=${s.operandType}`);
        //	checkOpOp (m,s);
//        checkOpOp (ma,s); ?????? Need to fix up after refactoring operand parse
	if (fmt==arch.aRR) { // Rp,Rq   ?????????????????????
            // cmp has d=0, a=p, b=q
            // inv has d=p, a=q, b=0
	    com.mode.devlog (`pass2 aRR`);
	    op = s.operation.opcode;
            s.field_d = op==opcode_cmp ? 0 : s.operand_str1;
            s.field_a = op==opcode_cmp ? s.operand_str1 : s.operand_str2;
            s.field_b = op==opcode_cmp ? s.operand_str2 : 0;
	    s.codeWord1 = mkWord(op[0],s.field_d,s.field_a,s.field_b);
            //	    generateObjectWord (m, s, s.address, s.codeWord1);
            generateObjectWord (ma, s, s.address, s.codeWord1);
	    com.mode.devlog (`pass2 op=${op} ${arith.wordToHex4(s.codeWord1)}`);
	} else if (fmt==arch.aRREXP) { // ??????????????????????????
	    com.mode.devlog (`pass2 aRREXP`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // Rp
            s.field_e = s.operand_str2; // Rq
            s.field_f = 0;
            s.field_g = logicFunction(s.fieldOperation);
            s.field_h = 0;
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
            generateObjectWord (ma, s, s.address, s.codeWord1);
            generateObjectWord (ma, s, s.address+1, s.codeWord2);
	} else if (fmt==arch.aRCEXP) { // ????????????????????????????
	    com.mode.devlog ("pass2 aRCEXP");
	    op = s.operation.opcode;
            s.field_e = s.operand_str1;
	    let ctlRegName = s.operand_str2;
            let ctlRegIdx = findCtlIdx (ma,s,s.operand_str2);
            s.field_f = ctlRegIdx;
	    s.codeWord1 = mkWord448(14,0,op[1]);
	    s.codeWord2 = mkWord(s.field_e,s.field_f,0,0);
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);
	} else if (fmt==arch.aRRR) { // Rd,Ra,Rb
	    com.mode.devlog (`pass2 aRRR`);
	    op = s.operation.opcode;
            const x =  rrrParser.exec (s.fieldOperands);
            if (x) {
                const {1:d, 2:a, 3:b} = x;
                s.codeWord1 = mkWord (op[0], d, a, b);
                generateObjectWord (ma, s, s.address, s.codeWord1);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RRR operands`);
            }
	} else if (fmt==arch.aRRKEXP) { // ?????????????????????
	    com.mode.devlog (`pass2 aRRKEXP`);
	    com.mode.devlog (`d=${s.d} a=${s.a} k=${s.k}`);
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
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);
	} else if (fmt==arch.aRRKKEXP) { // ??????????????????????????
	    com.mode.devlog (`pass2 aRRKKEXP`);
	    com.mode.devlog (`d=${s.d} a=${s.a} k=${s.k}`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // Rp
            s.field_e = s.operand_str2; // reg operand
            s.field_f = 0;
            s.field_g = s.operand_str3; // start bit index
            s.field_h = s.operand_str4; // end bit index
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
	    s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);
        } else if (fmt==arch.aRRRKEXP) { // ???????????????????????
            com.mode.devlog (`pass2 aRRRKEXP`);
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
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);
        } else if (fmt==arch.aRRRKKEXP) { // ??????????????????????????
            com.mode.devlog (`pass2 aRRRKKEXP`);
	    op = s.operation.opcode;
            s.d = s.operand_str1; // dest reg
            s.field_e = s.operand_str2; // first operand reg
            s.field_f = s.operand_str3; // second operand reg
            s.field_g = s.operand_str4; // function
            s.field_h = s.operand_str5; // unused
            com.mode.devlog(`RRRKK op=${op[0]} opx=${op[1]} d=${s.d}`);
            com.mode.devlog(`RRRKK e=${s.field_e} f=${s.field_f}`
                            + ` g=${s.field_g}  h=${s.field_h} `);
            s.codeWord1 = mkWord448(op[0],s.d,op[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,s.field_h);
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);

	} else if (fmt==arch.aJX) {
	    com.mode.devlog (`pass2 aJX`);
	    op = s.operation.opcode;
            s.operandType = arch.aJX; // ???? is this needed
            const x = jxParser.exec  (s.fieldOperands);
            if (x) {
                const {1:disp, 2:a} = x;

	        s.codeWord1 = mkWord (op[0], op[2], a, op[1]);
                let v = evaluate (ma, s, s.address+1, disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    com.mode.devlog (`relocatable displacement`);
                    generateRelocation (ma, s, s.address+1);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires JX operands`);
            }
	} else if (fmt==arch.aRX) {
	    com.mode.devlog (`pass2 aRX`);
	    op = s.operation.opcode;
            let x = rxParser.exec  (s.fieldOperands);
            if (x) {
                const {1:d, 2:disp, 3:a} = x;
                s.codeWord1 = mkWord (op[0], d, a, op[1]);

                let v = evaluate (ma, s, s.address+1, disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    generateRelocation (ma, s, s.address+1);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
                console.log (`pass2 RX codeword1 ${arith.wordToHex4(s.codeWord1)}`);
                console.log (`pass2 RX codeword2 ${arith.wordToHex4(s.codeWord2)}`);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RX operands`);
            }

	} else if (fmt==arch.aKX) { // ???????????????????????????
	    com.mode.devlog (`pass2 aKX`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord(op[0],s.d,s.a,op[1]);
            let v = evaluate(ma,s,s.address+1,s.field_disp);
            s.codeWord2 = v.value;
            if (v.evalRel) {
                com.mode.devlog (`relocatable displacement`);
                generateRelocation (ma, s, s.address+1);
            }
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);
	} else if (fmt==arch.EXP0) { // ????????????????????????
	    com.mode.devlog (`pass2 EXP0`);
	    op = s.operation.opcode;
	    s.codeWord1 = mkWord448(op[0],0,op[1]);
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	} else if (fmt==arch.aRRREXP) { // ????????????????????????????
	    com.mode.devlog (`pass2 aRRREXP`);
	    op = s.operation.opcode;
            s.d  = s.operand_str1;   // first register
	    s.field_e  = s.operand_str2;   // second register
	    s.field_f  = s.operand_str3;   // second register
	    s.field_g  = logicFunction(s.fieldOperation);
	    s.codeWord1 = mkWord448(op[0],s.d,op[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,0);
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);
	} else if (fmt==arch.aRRXEXP) { // ??????????????????????????
	    com.mode.devlog (`pass2 aRRXEXP`);
	    op = s.operation.opcode;
            s.field_e  = s.operand_str1;      // start register
	    s.field_f  = s.operand_str2;      // start register
	    s.field_gh = s.operand_str3;      // offset (8 bits)
	    s.field_a  = s.operand_str4;      // index register
            let v = evaluate(ma,s,s.address+1,s.field_gh); // value of offset
	    s.codeWord1 = mkWord448(op[0],s.field_a,op[1]);
            s.codeWord2 = mkWord(0,s.field_e,s.field_f,s.field_gh);
            s.codeWord2 = mkWord448(s.field_e,s.field_f,v.value);
            if (v.evalRel) {
                mkErrMsg (ma,s,`This instruction requires fixed displacement\n`
                          + `       but ${s.field_disp} is relocatable`);
            }
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);
        } else if (fmt==arch.DATA) { // ???????????????????????
            com.mode.devlog (`pass2 data ${s.dat}`);
            let v = evaluate(ma,s,s.address,s.dat);
            com.mode.devlog (v);
            s.codeWord1 = v.value;
	    generateObjectWord (ma, s, s.address, s.codeWord1);
            if (v.evalRel) {
                com.mode.devlog (`relocatable data`);
                //                generateRelocation (m, s, s.address);
                generateRelocation (ma, s, s.address);
            }
        } else if (fmt==arch.DirModule) { // ??????????????????????
            com.mode.devlog ('pass2 module statement')
            // require that no code has yet been generated ???
            let modname = s.fieldLabel;
            ma.objectCode.push (`module   ${modname}`)
        } else if (fmt==arch.DirImport) { // ?????????????????????
            com.mode.devlog ('pass2 import statement, handled in pass 1')
        } else if (fmt==arch.DirExport) { // ????????????????????
            com.mode.devlog ('pass2 export statement')
            com.mode.devlog (`export identarg=${s.identarg}`);
            ma.exports.push(s.identarg);
        } else if (fmt==arch.DirOrg) { // ???????????????????
            com.mode.devlog ('pass2 org statement, not yet implemented')
        } else if (fmt==arch.DirEqu) { // handled in pass 1 ?????????????
            com.mode.devlog ('pass2 equ statement') 
	} else {
	    com.mode.devlog('pass2 other, noOperation');
	}
	s.listingLinePlain =  (s.lineNumber+1).toString().padStart(4,' ')
	    + ' ' + arith.wordToHex4(s.address)
	    + ' ' + (s.codeSize>0 ? arith.wordToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize>1 ? arith.wordToHex4(s.codeWord2) : '    ')
            + ' '
	    + s.fieldLabel
	    + s.fieldSpacesAfterLabel
	    + s.fieldOperation
	    + s.fieldSpacesAfterOperation
	    + s.fieldOperands
	    + fixHtmlSymbols (s.fieldComment);
	s.listingLineHighlightedFields = (s.lineNumber+1).toString().padStart(4,' ')
	    + ' ' + arith.wordToHex4(s.address)
	    + ' ' + (s.codeSize>0 ? arith.wordToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize>1 ? arith.wordToHex4(s.codeWord2) : '    ')
            + ' '
	    + com.highlightField (s.fieldLabel, "FIELDLABEL")
	    + s.fieldSpacesAfterLabel
	    + com.highlightField (s.fieldOperation, "FIELDOPERATION")
	    + s.fieldSpacesAfterOperation
	    + com.highlightField (s.fieldOperands, "FIELDOPERAND")
	    + com.highlightField (fixHtmlSymbols(s.fieldComment), "FIELDCOMMENT") ;

	ma.asmListingText.push(s.listingLinePlain);
	ma.asmListingPlain.push(s.listingLinePlain);
	ma.asmListingDec.push(s.listingLineHighlightedFields);
	for (let i = 0; i < s.errors.length; i++) {
	    ma.asmListingText.push('Error: ' + s.errors[i],'ERR');
            	    ma.asmListingPlain.push (com.highlightField
                                     ('Error: ' + s.errors[i],'ERR'));
            ma.asmListingDec.push(com.highlightField
                                  ('Error: ' + s.errors[i],'ERR'));
	}
    }
    emitObjectWords (ma);
    emitRelocations (ma);
    emitImports (ma);
    emitExports (ma);
    ma.objectText = ma.objectCode.join("\n");
    emitMetadata (ma);
    ma.metadataText = ma.metadata.join("\n");
}

function fixHtmlSymbols (str) {
    let x;
    x = str.replace(/</g, "&lt;")
    return x;
}

// Add object word x to buffer for next object code line in module m.
// The asm statement is s, and the object word will be loaded at
// address a.  The line number entered into asmap is ln = s.lineNumber
// + 1 to account for the header line that is inserted before the
// statements in the listing.

function generateObjectWord (ma, s, a, x) {
    objectWordBuffer.push (x);
    ma.asArrMap[a] = s.lineNumber;
}

function emitObjectWords (ma) {
    let xs, ys, zs;
    while (objectWordBuffer.length > 0) {
        xs = objectWordBuffer.splice(0,objBufferLimit);
        ys = xs.map( (w) => arith.wordToHex4(w));
        zs = 'data     ' + ys.join(',');
        ma.objectCode.push (zs);
    }
}

// Record an address that needs to be relocated

function generateRelocation (ma, s, a) {
    com.mode.devlog (`generateRelocation ${a}`);
    relocationAddressBuffer.push (a);
}

// Generate the relocation statements in object code

function emitRelocations (ma) {
    let xs, ys, zs;
    while (relocationAddressBuffer.length > 0) {
        let xs = relocationAddressBuffer.splice(0,objBufferLimit);
        let ys = xs.map((w) => arith.wordToHex4(w));
        let zs = 'relocate ' + ys.join(',')
        ma.objectCode.push (zs);
    }
}

// Generate import statements in object code

function emitImports (ma) {
    com.mode.trace = true;
    for (let x of ma.imports) {
        let sym = ma.symbolTable.get(x);
        let v = sym.value.value;
        let z = `import   ${sym.name},${v}`;
        ma.objectCode.push(z);
    }
    com.mode.trace = false;
}

function emitExports (ma) {
    let x, y, sym, v, r;
    com.mode.devlog ('emitExports' + ma.exports);
    while (ma.exports.length > 0) {
        x = ma.exports.splice(0,1);
        y = x[0];
        com.mode.devlog (`emit exports looking up  x=<${x}> y=${y}`);
        com.mode.devlog (ma.symbolTable);
        sym = ma.symbolTable.get(y);
        if (sym) {
            r = sym.value.isRelocatable ? "relocatable" : "fixed";
            v = arith.wordToHex4(sym.value.value);
            ma.objectCode.push (`export   ${y},${v},${r}`);
        } else {
            com.mode.devlog (`\n\n\n ERROR export error ${x}\n\n\n`);
        }
    }
}

// The size of text lines in object code and metadata is limited to
// NitemsPerLine values.  This has two purposes: it makes the object
// code and metadata more readable, and it helps avoid buffer overrun
// while reading in the data in C programs.

const NitemsPerLine = 10;

function emitMetadata (ma) {
    com.mode.devlog ("emitMetadata");
    let xs, ys;
    xs = [...ma.asArrMap];
    ma.metadata.push (`asmap ${xs.length}`);
    while (xs.length > 0) {
        let ys = xs.slice (0, NitemsPerLine);
        xs.splice (0, NitemsPerLine);
        ma.metadata.push (ys);
    }
    xs = [...ma.asmListingPlain];
    ys = [...ma.asmListingDec];
    ma.metadata.push (`source ${xs.length}`);
    for (let i = 0; i < xs.length; i++) {
        ma.metadata.push(xs[i]);
        ma.metadata.push(ys[i]);
    }
}

// Convert  the address-source map x to a string

export function showAsMap (x) {
    console.log ('Address~Source map');
    for (let i = 0; i < x.length; i++) {
        console.log (`address ${wordToHex4(i)} => ${x[i]}`);
    }
}

export function setAsmListing () {
    com.mode.devlog ("setAsmListing");
    let listingDec = smod.getSelectedModule().asmInfo.asmListingDec;
    let listing = listingDec ? listingDec.join('\n') : 'no listing';
    document.getElementById('AsmTextHtml').innerHTML = listing;
}

export function setObjectListing () {
    let code = smod.getSelectedModule().asmInfo.objectCode;
    let codeText = code ? code.join('<br>') : 'no code';
    let listing = "<pre class='HighlightedTextAsHtml'>" + codeText + "</pre>";
    document.getElementById('AsmTextHtml').innerHTML = listing;
}

export function setMetadata () {
    let code = smod.getSelectedModule().asmInfo.metadata;
    let codeText = code ? code.join('<br>') : 'no code';
    let listing = "<pre class='HighlightedTextAsHtml'>" + codeText + "</pre>";
    document.getElementById('AsmTextHtml').innerHTML = listing;
}

/*
function emitImportAddresses (ma,x) {
    com.mode.devlog (`emitImportAddresses ${x.symbol}`);
    let xs, ys, zs;
    while (x.symUsageAddrs.length > 0) {
        let xs = x.symUsageAddrs.splice(0,objBufferLimit);
        let ys = xs.map((w) => arith.wordToHex4(w));
        let zs = 'import   '
            + x.symImportedFrom
            + ',' + x.symbol
            + ',' + ys.join(',');
        ma.objectCode.push (zs);
    }
}
2020-07-29 1514 lines
*/
