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
// import * as ed from './editor.mjs';

// TODO
let opcode_cmp = 4; // refactor, see pass2/fmt==arch.RR

//------------------------------------------------------------------------------
// Global
//------------------------------------------------------------------------------

// Buffers to hold generated object code

let objBufferLimit = 8;             // how many code items to allow per line
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
    getIsRelocatable () {
        return this.isRelocatable;
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
    com.mode.devlog (`evaluate received expression ${x}`)
    com.mode.devlog (`evaluate returning ${result.show()}`)
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
    return {lineNumber,                        // array index of statement
	    address,                           // address where code goes
	    srcLine,                           // source line
	    listingLinePlain: "",              // object and source text
	    listingLineHighlightedFields : "", // listing with src field spans
	    fieldLabel : '',                   // label
	    fieldSpacesAfterLabel : '',        // white space
	    fieldOperation : '',               // operation mnemonic
	    fieldSpacesAfterOperation : '',    // white space
	    fieldOperands : '',                // operands
	    fieldComment : '',                 // comments are after operand or ;
	    hasLabel : false,                  // statement has a valid label
            operation : null,                  // spec of the operation if exists
	    codeSize : 0,                      // number of words generated
	    codeWord1 : null,                  // first word of object
	    codeWord2 : null,                  // second word of object
	    errors : []                        // array of lines of error messages
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
const xParser =
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

// aRC asm format (register, control reg name): getctl R3,mask
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
    com.mode.devlog (`parseAsmLine i=${i}`);
    let s = ma.asmStmt[i];
    showAsmStmt(s);
    let p = parseSplitFields.exec(s.srcLine);
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
    com.mode.devlog (`Pass1 ${s.lineNumber} ${s.fieldOperation} ${showOperation(s.operation)}`);

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
    com.mode.devlog (`parseOperation line ${s.lineNumber} op=${op}`);
    if (op !== '') {
	let x = arch.statementSpec.get(op);
	if (x) {
            com.mode.devlog (`parseOperation: found statementSpec ${x}`);
	    s.operation = x;
            if (s.operation.ifmt==arch.iData && s.operation.afmt==arch.aData) {
                s.codeSize = 1; // should be number of data words in operand
            } else {
	        s.codeSize = arch.formatSize(x.ifmt);
            }
	} else {
            s.operation = arch.emptyOperation;
            s.codeSize = 0;
            // GIVE ERROR MESSAGE ?????
	}
    } else {
        s.operation = arch.emptyOperation;
    }
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
    com.mode.devlog('Assembler Pass 2');
    objectWordBuffer = [];
    relocationAddressBuffer = [];
    for (let i = 0; i < ma.asmStmt.length; i++) {
	let s = ma.asmStmt[i];
	com.mode.devlog(`Pass2 line ${s.lineNumber} = /${s.srcLine}/`);
        let op = s.operation;
        com.mode.devlog (`Pass2 operation ${s.fieldOperation} ${showOperation(op)}`);
	if (op.ifmt==arch.iRRR && op.afmt==arch.aRRR) {
            com.mode.devlog (`pass2 iRRR/aRRR`);
            const x =  rrrParser.exec (s.fieldOperands);
            if (x) {
                const {1:d, 2:a, 3:b} = x;
                s.codeWord1 = mkWord (op.opcode[0], d, a, b);
                generateObjectWord (ma, s, s.address, s.codeWord1);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RRR operands`);
            }
	} else if (op.ifmt==arch.iRRR && op.afmt==arch.aRR) {
	    com.mode.devlog (`Pass2 iRRR/aRR`);
            const x = rrParser.exec (s.fieldOperands);
            if (x) {
                const {1:a, 2:b} = x;
	        s.codeWord1 = mkWord (op.opcode[0], 0, a, b);
                generateObjectWord (ma, s, s.address, s.codeWord1);
	        com.mode.devlog (`Pass2 op=${op} ${arith.wordToHex4(s.codeWord1)}`);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RR operands`);
            }
	} else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRR) {
	    com.mode.devlog (`pass2 iEXP2/aRR`);
            const x = rrParser.exec (s.fieldOperands);
            if (x) {
                const {1:d, 2:e} = x;
                let f = 0;
                let g = arith.logicFunction(s.fieldOperation);
                let h = 0;
	        s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
                s.codeWord2 = mkWord (e, f, g, h);
                generateObjectWord (ma, s, s.address, s.codeWord1);
                generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RR operands`);
            }
	} else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRC) {
	    com.mode.devlog ("pass2 aRC");
            let x = rcParser.exec  (s.fieldOperands);
            if (x) {
                const {1:e, 2:ctlRegName} = x;
                let ctlRegIdx = findCtlIdx (ma,s,ctlRegName);
	        s.codeWord1 = mkWord448 (14,0,op.opcode[1]);
	        s.codeWord2 = mkWord (e, ctlRegIdx, 0, 0);
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RC operands`);
            }
	} else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRRk) {
	    com.mode.devlog (`pass2 aRRk`);
            const x = rrkParser.exec (s.fieldOperands);
            if (x) {
/* Look at this, especially pseudo                
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
*/
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RRk operands`);
            }
	} else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRRkk) {
	    com.mode.devlog (`pass2 iEXP2/aRRkk`);
            const x = rrkkParser.exec (s.fieldOperands);
            if (x) {
                const {1:d, 2:e, 3:g, 4:h} = x;
                let f = 0;
	        s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
	        s.codeWord2 = mkWord (e, f, g, h);
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RRkk operands`);
            }
        } else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRRRk) {
            com.mode.devlog (`pass2 aRRRk`);
            const x = rrrkParser.exec (s.fieldOperands);
            if (x) {
    /* PSEUDO check
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
*/
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RRRkk operands`);
            }
        } else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRRRkk) {
            com.mode.devlog (`pass2 aRRRkk`);
            const x = rrrkkParser.exec (s.fieldOperands);
            if (x) {
                const {1:d, 2:e, 3:f, 4:g, 5:h} = x;
                s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
                s.codeWord2 = mkWord (e, f, g, h);
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RRRkk operands`);
            }
	} else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRRR && s.pseudo) {
            com.mode.devlog ('Pass2 EXP2/RRR pseudo (logic)');
            
	} else if (op.ifmt==arch.iRX && op.afmt==arch.aX && !s.pseudo) {
            com.mode.devlog (`Pass2 RX/X)`);
            const x = xParser.exec  (s.fieldOperands);
            if (x) {
                const {1:disp, 2:a} = x;
	        s.codeWord1 = mkWord (op.opcode[0], 0, a, op.opcode[1]);
                let v = evaluate (ma, s, s.address+1, disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    com.mode.devlog (`relocatable displacement`);
                    generateRelocation (ma, s, s.address+1);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires X operand`);
            }
	} else if (op.ifmt==arch.iRX && op.afmt==arch.aX && s.pseudo) {
	    com.mode.devlog (`Pass2 RX/X pseudo (conditional jumps)`);
            const x = xParser.exec  (s.fieldOperands);
            if (x) {
                const {1:disp, 2:a} = x;
	        s.codeWord1 = mkWord (op.opcode[0], op.opcode[2], a, op.opcode[1]);
                let v = evaluate (ma, s, s.address+1, disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    com.mode.devlog (`relocatable displacement`);
                    generateRelocation (ma, s, s.address+1);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires X operand`);
            }
	} else if (op.ifmt==arch.iRX && op.afmt==arch.aRX) {
	    com.mode.devlog (`Pass2 RX/RX`);
            let x = rxParser.exec  (s.fieldOperands);
            if (x) {
                const {1:d, 2:disp, 3:a} = x;
                s.codeWord1 = mkWord (op.opcode[0], d, a, op.opcode[1]);
                let v = evaluate (ma, s, s.address+1, disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    generateRelocation (ma, s, s.address+1);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
                com.mode.devlog (`pass2 RX codeword1 ${arith.wordToHex4(s.codeWord1)}`);
                com.mode.devlog (`pass2 RX codeword2 ${arith.wordToHex4(s.codeWord2)}`);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RX operands`);
            }
        } else if (op.ifmt==arch.iRX && op.afmt==arch.akX) {
	    com.mode.devlog (`pass2 RX/kX`);
            let x = kxParser.exec (s.fieldOperands);
            if (x) {
                const {1:d, 2:disp, 3:a} = x;
	        s.codeWord1 = mkWord (op.opcode[0], d, a, op.opcode[1]);
                let v = evaluate (ma, s, s.address+1, disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    com.mode.devlog (`relocatable displacement`);
                    generateRelocation (ma, s, s.address+1);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires kX operands`);
            }
	} else if (op.ifmt==arch.iEXP2 && op.afmt==arch.akX) {
	    com.mode.devlog (`pass2 EXP2/kX`);
            let x = kxParser.exec (s.fieldOperands);
            if (x) {
	        s.codeWord1 = mkWord(op.opcode[0],s.d,s.a,op.opcode[1]);
                let v = evaluate(ma,s,s.address+1,s.field_disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    com.mode.devlog (`relocatable displacement`);
                    generateRelocation (ma, s, s.address+1);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RX operands`);
            }
	} else if (op.ifmt==arch.iEXP1 && op.afmt==arch.a0) {
	    com.mode.devlog (`Pass2 iEXP1/no-operand`);
            if (s.fieldOperands=="") {
	        s.codeWord1 = mkWord448(op.opcode[0],0,op.opcode[1]);
	        generateObjectWord (ma, s, s.address, s.codeWord1);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires no operands`);
            }
	} else if (op.ifmt==arch.aRRREXP) { // ????????????????????????????
	    com.mode.devlog (`pass2 aRRREXP`);
            s.d  = s.operand_str1;   // first register
	    s.field_e  = s.operand_str2;   // second register
	    s.field_f  = s.operand_str3;   // second register
	    s.field_g  = logicFunction(s.fieldOperation);
	    s.codeWord1 = mkWord448(op.opcode[0],s.d,op.opcode[1]);
            s.codeWord2 = mkWord(s.field_e,s.field_f,s.field_g,0);
	    generateObjectWord (ma, s, s.address, s.codeWord1);
	    generateObjectWord (ma, s, s.address+1, s.codeWord2);

	} else if (op.ifmt==arch.iEXP2 && op.afmt==arch.aRRX) {
	    com.mode.devlog (`pass2 EXP2/RRX`);
            const x = rrxParser.exec (s.fieldOperands);
            if (x) {
                const {1:e, 2:f, 3:gh, 4:a} = x;
                let v = evaluate (ma, s, s.address+1, gh); // value of offset
	        s.codeWord1 = mkWord448 (op.opcode[0], a, op.opcode[1]);
                s.codeWord2 = mkWord448 (e, f, v.value);
                if (v.evalRel) {
                    mkErrMsg (ma,s,`This instruction requires fixed displacement\n`
                              + `       but ${s.field_disp} is relocatable`);
                }
	        generateObjectWord (ma, s, s.address, s.codeWord1);
	        generateObjectWord (ma, s, s.address+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RR operands`);
            }
        } else if (op.ifmt==arch.iData && op.afmt==arch.aData) {
            com.mode.devlog (`Pass2 data ${s.dat}`);
            let v = evaluate(ma,s,s.address,s.fieldOperands);
            com.mode.devlog (v);
            s.codeWord1 = v.getval();
	    generateObjectWord (ma, s, s.address, s.codeWord1);
            if (v.getIsRelocatable()) {
                com.mode.devlog (`relocatable data`);
                generateRelocation (ma, s, s.address);
            }
        } else if (op.afmt==arch.aModule) { // ??????????????????????
            com.mode.devlog ('pass2 module statement')
            // require that no code has yet been generated ???
            let modname = s.fieldLabel;
            ma.objectCode.push (`module   ${modname}`)
        } else if (op.afmt==arch.aImport) { // ?????????????????????
            com.mode.devlog ('pass2 import statement, handled in pass 1')
        } else if (op.afmt==arch.aExport) { // ????????????????????
            com.mode.devlog ('pass2 export statement')
            com.mode.devlog (`export identarg=${s.identarg}`);
            ma.exports.push(s.identarg);
        } else if (op.afmt==arch.aOrg) { // ???????????????????
            com.mode.devlog ('pass2 org statement, not yet implemented')
        } else if (op.afmt==arch.aEqu) { // handled in pass 1 ?????????????
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
    ma.objectCode.push ("");
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
        zs = 'data       ' + ys.join(',');
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
        let zs = 'relocate   ' + ys.join(',')
        ma.objectCode.push (zs);
    }
}

// Generate import statements in object code

function emitImports (ma) {
    for (let x of ma.imports) {
        let sym = ma.symbolTable.get(x);
        let v = sym.value.value;
        let z = `import     ${sym.name},${v}`;
        ma.objectCode.push(z);
    }
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

function showOperation (op) {
    return `ifmt=${op.ifmt.description} afmt=${op.afmt.description}`
    + `opcode=${op.opcode} pseudo=${op.pseudo}`;
}
