// Sigma16: assembler.mjs
// Copyright (C) 2023 John T. O'Donnell.  License: GNU GPL Version 3
// See Sigma16/README, LICENSE, and https://github.com/jtod/Sigma16

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

//----------------------------------------------------------------------
// assembler.mjs translates assembly language to machine language
//----------------------------------------------------------------------

import * as com from './common.mjs';
import * as arch from './architecture.mjs';
import * as arith from './arithmetic.mjs';
import * as st from './state.mjs';

//----------------------------------------------------------------------
// Global
//----------------------------------------------------------------------

// Buffers to hold generated object code

let objBufferLimit = 8;             // how many code items to allow per line
let objectWordBuffer = [];          // list of object code words
let relocationAddressBuffer = [];   // list of relocation addresses

//----------------------------------------------------------------------
// GUI interface to the assembler
//----------------------------------------------------------------------

// The assembler finds the currently selected module m, and then picks
// up the source text from m.currentSrc.  It leaves its results in
// m.asmInfo.  If it finds the name of the module being assembled from
// a module statement, it updates the module's record in
// st.env.moduleSet.

// Main functions in the assembler API:
// enterAssemblerPage ()   -- when the user goes to the Assembler page
// assemblerGUI ()         -- when "Assemble" button is clicked
// setSource ()            -- when "Show Source" button is clicked
// displayObjectListing () -- when "Show Object" button is clicked
// displayAsmListing ()    -- when "Show Listing" button is clicked
// displayMetadata ()      -- when "Show Metadata" button is clicked

// Called when user goes to the Assembler page

export function enterAssemblerPage () {
    const m = st.env.moduleSet.getSelectedModule ()
    console.log ("enterAssemblerPage")
    console.log (m.currentSrc)
    const ai = m.asmInfo
    ai.asmSrcText = m.currentSrc
    ai.asmSrcLines = ai.asmSrcText.split("\n")
    mDisplayAsmSource (m)
    ai.metadata.listingDec = []
}

const notYet = "Listing will be available after assembly"

// Called when user clicks "Assemble" on assembler page

export function assemblerGUI () {
    com.mode.devlog ("assemblerGUI starting");
    const m = st.env.moduleSet.getSelectedModule ();
    console.log (`assemblerGUI m=${m.modKey}`)
    document.getElementById('AsmTextHtml').innerHTML = "";
    document.getElementById('ProcAsmListing').innerHTML = "";
    com.clearObjectCode (); // clear text in linker pane
    assembler (m)
    displayAsmListing ();
    console.log ("++++++++++++++++++++ asm done")
    console.log (m.asmInfo.objectText)
}

// Called when user clicks "Show Source" on the assembler page

export function displayAsmSource () {
    const m = st.env.moduleSet.getSelectedModule ();
    mDisplayAsmSource (m)
}

// Worker function for displayAsmSource, given module m

export function mDisplayAsmSource (m) {
    let xs = m.asmInfo.asmSrcLines.slice ()
    xs.unshift("<pre class='HighlightedTextAsHtml'>");
    xs.push("</pre>");
    document.getElementById('AsmTextHtml').innerHTML = xs.join("\n");
}

// Called when user clicks "Show Object" on the assembler page

export function displayObjectListing () {
    const m = st.env.moduleSet.getSelectedModule ();
    //    const codeText = m.asmInfo.objMd.objText;
    const codeText = m.objText
    let listing = "<pre class='VerbatimText'>" + codeText + "</pre>";
    document.getElementById('AsmTextHtml').innerHTML = listing;
}

// Called when user clicks "Show Listing" on the assembler page

export function displayAsmListing () {
    com.mode.devlog ("displayAsmListing");
    const m = st.env.moduleSet.getSelectedModule ();
    const ai =  m.asmInfo;
    let lst = ai.metadata.listingDec; // array of listing lines
    lst.unshift("<pre class='HighlightedTextAsHtml'>")
    lst.push("</pre>")
    document.getElementById('AsmTextHtml').innerHTML = lst.join("\n")
}

// Called when user clicks "Show Metadata" on the assembler page

export function displayMetadata () {
    const m = st.env.moduleSet.getSelectedModule ();
    const src = m.getAsmText ();
    //    const mdText = m.asmInfo.objMd.mdText;
    const mdText = m.mdText
    let listing = "<pre class='HighlightedTextAsHtml'>" + mdText + "</pre>";
    document.getElementById('AsmTextHtml').innerHTML = listing;
}

//----------------------------------------------------------------------
// Assembler information record
//----------------------------------------------------------------------

// A ...Text fields is a single string that can be used for display,
// and a ...Lines field is a list of individual lines corresponding to
// the text string.

export class AsmInfo {
    constructor (m) {
        this.asmSrcText = "; default asm src"; // raw source text
        this.asmSrcLines = [];               // list of lines of source text
        this.objectText = "";       // object code as single string
	this.objectCode = [];                // string hex representation
        this.mdText = "";
        this.metadata = new Metadata ();  // address-source map
        this.asmListingText = "";

	this.asmModName = "anonymous";     // may be defined by module stmt
	this.asmStmt = [];                 // corresponds to source lines
	this.symbols = [];                 // symbols used in the source
	this.symbolTable = new Map ();     // symbol table
	this.locationCounter =             //  next code address
            new Value (0, Local, Relocatable);
        this.imports = [];                 // imported module/identifier
        this.exports = [];                 // exported identifiers
	this.nAsmErrors = 0;               // errors in assembly source code
    }
}

//----------------------------------------------------------------------
// Character set
//----------------------------------------------------------------------

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
      + " \t,;\r\n"               // separators
      + '"'                       // quotes
      + "'"                       // quotes
      + ".$[]()+-*"               // punctuation
      + "?��`<=>!%^&{}#~@:|/\\";  // other

//----------------------------------------------------------------------
// Symbol table
//----------------------------------------------------------------------

// The symbol table is a map from strings to Identifiers, where the
// string is the text of the identifier name, and the Identifier
// object is a record containing all the required information about
// the identifier.

// An Identifier is a symbol table entry: i.e. a record giving all the
// information about an identifier.  It is stored in the symbol table
// keyed by the identifier string.  The name is the identifier string,
// taken from the label field; v is its value, and defline is line
// number where the identifier was defined.

class Identifier {
    constructor (name, mod, extname, v, defLine) {
        this.name = name;
        this.mod = mod;
        this.extname = extname;
        this.value = v;
        this.defLine = defLine;
        this.usageLines = [];
    }
}

function displaySymbolTableHtml (ma) {
    ma.metadata.pushSrc ("", "", "");
    ma.metadata.pushSrc ("Symbol table",
                         "<span class='ListingHeader'>Symbol table</span>",
                         "<span class='ListingHeader'>Symbol table</span>");
    let symtabHeader = "Name        Val Org Mov  Def Used";
    ma.metadata.pushSrc
      (symtabHeader,
       `<span class='ListingHeader'>${symtabHeader}</span>`,
       `<span class='ListingHeader'>${symtabHeader}</span>`);
    let syms  =[ ...ma.symbolTable.keys() ].sort();
    com.mode.devlog (`Symbol table keys = ${syms}`);
    for (let symkey of syms) {
        let x = ma.symbolTable.get (symkey);
        let fullname = x.mod ? `${x.mod}.${x.name}`
            : `${x.name}`;
        let xs = fullname.padEnd(11)
            + x.value.toString()
    	    + x.defLine.toString().padStart(5)
            + '  '
            + x.usageLines.join(',');

        ma.metadata.pushSrc (xs, xs, xs);
    }
}

//----------------------------------------------------------------------
// Instruction fields
//----------------------------------------------------------------------

export const Field_op = Symbol ("op");
export const Field_d = Symbol ("d");
export const Field_a = Symbol ("a");
export const Field_b = Symbol ("b");
export const Field_disp = Symbol ("disp");
export const Field_e = Symbol ("e");
export const Field_f = Symbol ("f");
export const Field_g = Symbol ("g");
export const Field_h = Symbol ("h");

//----------------------------------------------------------------------
// Values
//----------------------------------------------------------------------

// A value is a 16-bit word represented as a natural number; it also
// has attributes (origin and movability) that affect its usage.
// Values are produced by evaluating an expression.  Values may be
// used to define instruction fields, and they may also be used as
// arguments to assembler directives.

// Origin attribute
export const Local = Symbol ("Loc");         // defined in this module
export const External = Symbol ("Ext");      // defined in another module

// Movability attribute
export const Fixed = Symbol ("Fix");         // constant
export const Relocatable = Symbol ("Rel");   // changes during relocation

// Add x+y and return the result.  Need ma and s for generating error
// messages.

export function addVal (ma,s,x,y) {
    let result = Zero.copy();
    if (x.origin==External || y.origin==External) {
        mkErrMsg (ma, s, `Cannot perform arithmetic on external value`)
    } else  if (x.movability==Relocatable && y.movability==Relocatable) {
        mkErrMsg (ma, s, `Cannot add two relocatable values`);
    } else {
        let m = x.movability==Relocatable || y.movability==Relocatable
                  ? Relocatable : Fixed;
        result = new Value (wrapWord (x.word + y.word), Local, m);
    }
    com.mode.devlog (`addVal ${x.word} + ${y.word} = ${result.word}`);
    com.mode.devlog (`addVal ${x.toString()} +  ${y.toString()}`
                 + ` = ${result.toString()}`);
    return result;
}

// Calculate offset for pc-relative addressing
function findOffset (here, there) {
    const k = there.movability === Relocatable
          ? Math.abs (there.word - (here.word + 2))
          : there.word
    console.log (`findOffset here=${here} there=${there} k=${k}`)
    return k
}

// Word addition doesn't overflow, but wraps around.  Any negative
// would be an internal error, and is set to 0 with message.

function wrapWord (x) {
    if (x<0) {
        com.mode.devlog (`Internal error: wrapWord ${x}`);
        x = 0;
    }
    return x; // check for neg, and mod
}

export class Value {
    constructor (v, o, m) {
        this.word = v;
        this.origin = o;
        this.movability = m;
    }
    copy () {
        return new Value (this.word, this.origin, this.movability);
    }
    add (k) {
        this.word = this.word + k.word;
        this.movability =
            k.movability==Fixed ? this.movability
            : this.movability==Fixed ? k.movability
            : Fixed;
    }
    toString () {
        let xs =  `${arith.wordToHex4(this.word)}`
            + ` ${this.origin.description}`
            + ` ${this.movability.description}`
        return xs;
    }
}

const ExtVal = new Value (0, External, Fixed);

function mkConstVal (k) { return new Value (k, Local, Fixed); }
const Zero = mkConstVal (0);
const One  = mkConstVal (1);
const Two  = mkConstVal (2);

//----------------------------------------------------------------------
// Evaluation of expressions
//----------------------------------------------------------------------

// An expression is assembly language syntax that specifies a value.
// The assembler evaluates expressions to calculate the corresponding
// value.

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
    com.mode.devlog(`Enter evaluate ${typeof(x)} <${x}>`);
    let result;
    if (x.search(nameParser) == 0) { // identifier
	let r = ma.symbolTable.get(x);
	if (r) {
            result = r.value.copy(); // identifier already has a value
            r.usageLines.push (s.lineNumber+1);
	} else {
            mkErrMsg (ma, s, 'symbol ' + x + ' is not defined');
            result = mkConstVal(0); // new Value (0, Local, Fixed);
	}
    } else if (x.search(intParser) == 0) { // integer literal
        result = mkConstVal(arith.intToWord(parseInt(x,10)));
    } else if (x.search(hexParser) == 0) { // hex literal
//      result =  new Value (arith.hex4ToWord(x.slice(1)), Local, Fixed);
        result =  mkConstVal (arith.hex4ToWord(x.slice(1)));
    } else { // compound expression (not yet implemented)
        mkErrMsg (ma, s, 'expression ' + x + ' has invalid syntax');
        result = Zero.copy(); // new Value (0, Local, Fixed);
    }
//    com.mode.devlog (`evaluate received expression ${x}`)
    com.mode.devlog (`evaluate ${x} returning (${result.toString()})`)
    return result;
}

//----------------------------------------------------------------------
// Assembly language statement
//----------------------------------------------------------------------

// Each statement has a listing line which contains the line number,
// object code, and source code.  There are two versions of this:
// listingLinePlain just contains the text of the listing line, while
// listingLineHighlightedFields contains <span> elements to enable the
// various fields to be highlighted with colors.

function showAsmStmt (s) {
    com.mode.devlog (`*** showAsmStmt line=${s.lineNumber}`);
    com.mode.devlog (`*** srcLine=${s.srcLine}`);
    com.mode.devlog (`*** address=${s.address}`);

    console.log (`*** AsmStmt line = ${s.lineNumber}`);
    console.log (`*** AsmStmt line length = ${s.srcLine.length}`);
    console.log (`*** AsmStmt srcLine = <${s.srcLine}<`);
    console.log (`*** AsmStmt address = ${s.address}`);
}

function mkAsmStmt (lineNumber, address, srcLine) {
    com.mode.devlog (`@@@@@@@@ mkAsmStmt ${address.toString()}`);
    return {lineNumber,                        // array index of statement
	    address,                           // address where code goes
	    srcLine,                           // source line
	    listingLinePlain: "",              // object and source text
	    listingLineHighlightedFields : "", // listing with field spans
	    fieldLabel : '',                   // label
	    fieldSpacesAfterLabel : '',        // white space
	    fieldOperation : '',               // operation mnemonic
	    fieldSpacesAfterOperation : '',    // white space
	    fieldOperands : '',                // operands
	    fieldComment : '',                 // comments after operand or ;
	    hasLabel : false,                  // statement has a valid label
            operation : null,                  // operation spec if it exists
            operands : [],                     // individual operands
	    codeSize : Zero,                   // number of words generated
	    orgAddr : -1,                      // addr specified by org/block
            reserveSize : Zero,
            locCounterUpdate : Zero,           // new LC after reserve directive
	    codeWord1 : null,                  // first word of object
	    codeWord2 : null,                  // second word of object
	    errors : []                        // lines of error messages
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
		 + ' codesize=' + x.codeSize.word
		 + ' codeWord1=' + x.codeWord1
                 + ' codeWord2=' + x.codeWord2);
    if (x.errors.length > 0) {
	console.log ('error messages:\n' + x.errors.join('\n'));
    } else {
	console.log ('no errors detected');
    }
}

//----------------------------------------------------------------------
//  Error messages
//----------------------------------------------------------------------

// Report an assembly error: s is an assembly source line, err is an
// error message

function mkErrMsg (ma,s,err) {
    com.mode.devlog (err);
    if (!s) {
        s = ma.asmStmt[ma.asmStmt.length-1]
    }
    s.errors.push(err);
    ma.nAsmErrors++;
}

export function splitLines (txt) {
    let lines = txt.split ("\n")
    return lines
}

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

//----------------------------------------------------------------------
//  Assembler
//----------------------------------------------------------------------

// Translate assembly language source code to object code, also
// producing an assembly listing and metadata.  The argument m is an
// s16module which contains the source code in m.currentSrc.  The
// assembler obtains an existing asmInfo from m, names it ai, and
// fills in the fields of ai.  This approach enables bidirectional
// transfer of information about the module between the ModuleSet and
// the assembler.  This function is the main translator, used for both
// gui and cli.

export function assembler (m) {
    const ai = m.asmInfo
    ai.srcText = m.currentSrc
    ai.srcLines = splitLines (ai.srcText)
    console.log (`Entering assembler, ${ai.asmSrcLines.length} source lines`)
//    console.log (ai.srcLines)
    
    ai.nAsmErrors = 0;
    ai.asmStmt = [];
    ai.symbols = [];
    ai.objectCode = [];
    ai.exports = [];
    ai.locationCounter = new Value (0, Local, Relocatable);
    ai.symbolTable.clear();
    ai.metadata.pushSrc
      ("Line Addr Code Code Source",
       "<span class='ListingHeader'>Line Addr Code Code Source</span>",
       "<span class='ListingHeader'>Line Addr Code Code Source</span>");
    asmPass1 (ai);
    asmPass2 (ai);
    if (ai.nAsmErrors > 0) {
        let x = `\n ${ai.nAsmErrors} errors detected\n`;
        let y = com.highlightField (x, 'ERR');
        ai.metadata.unshiftSrc (x, y, y);
    }
    displaySymbolTableHtml(ai); // add symbol table to listing
    const mdText = ai.metadata.toText ();
    ai.objectText = ai.objectCode.join("\n");
    m.objText = ai.objectText
    m.mdText = mdText
    com.mode.devlog (ai.objectText);
    return
}
//----------------------------------------------------------------------
//  Regular expressions for the parser
//----------------------------------------------------------------------

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

const identParser = /^[a-zA-Z][a-zA-Z0-9_]*$/
const nameParser =  /^[a-zA-Z][a-zA-Z0-9_]*$/
const intParser =   /^-?[0-9]+$/
export const hexParser = /^\$([0-9a-f]{4})$/

const rcParser =
    /^R([0-9a-f]|(?:1[0-5])),([a-zA-Z][a-zA-Z0-9]*)$/;

const rrxParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5])),(-?[a-zA-Z0-9_\$]+)\[R([0-9a-f]|(?:1[0-5]))\]/;

const rrParser =
    /^R([0-9a-f]|(?:1[0-5])),R([0-9a-f]|(?:1[0-5]))$/;

// A register is R followed by register number, which must be either
// a 1 or 2 digit decimal number between 0 and 15, or a hex digit.
// An aRRR operand consists of three registers, separated by comma
// An RRRR operand consists of four registers, separated by comma

      
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

// No need to anchor to end of line; a trailing \r is ok
const regexpSplitFields =
      '^'                             // anchor to beginning of line
      + regexpFieldNoStringLit        // label
      + regExpWhiteSpace              // separator
      + regexpFieldNoStringLit        // operation
      + regExpWhiteSpace              // separator
      + regexpFieldNoStringLit        // operands
      + regExpComment                 // comment


const parseField = new RegExp(regExpField);
const parseSplitFields = new RegExp(regexpSplitFields);

function requireX (ma, s, field) {
    const xrParser = /^([^\[]+)\[(.*)\]$/
    let disp = "0"
    let index = 0
    const xr = xrParser.exec (field)
    if (xr) { // disp[reg]
        disp = xr[1]
        const regsrc = xr[2]
        index = requireReg (ma, s, regsrc)  // disp = field
        console.log (`requireX parse failed`)
    } else { // allow [R0] to be omitted
        disp = field
        index = 0
    }
    const result = {disp, index}
    console.log (`requireX field=${field} disp=<${disp}> index=${index}`)
    return result
}

// requireNoperands checks whether the instruction contains the
// correct number of operands, and if there aren't enough it defiles
// sufficient dummy ones.

function requireNoperands (ma, s, n) {
    let k = s.operands.length
    if (k != n) {
        mkErrMsg (ma, s, `There are ${k} operands but ${n} are required`)
    }
    for (let i = 0; i < n; i++) {
        if (! s.operands[i]) {
            s.operands[i] = "?"
        }
    }
}

// requireK4: obtain a 4-bit word from source.  The context is ma and
// statement s.  xs is a text field from assembly language source; it
// should evaluate to a 4-bit constant and be placed in "field" in the
// object code word at address a.  The value will not be relocated,
// but it could be imported.

// ??? todo  Generate import if necessary
// ??? todo  give message if result>15
// k4 is always fixed, never relocatable

function requireK16 (ma, s, field, xs) {
    com.mode.devlog (`requireK4 <${xs}>`);
    const a = s.address.word;
    const v = evaluate (ma, s, a, xs);
    const result = v.word;
    return result;
}

function requireK4 (ma, s, field, xs) {
    com.mode.devlog (`requireK4 <${xs}>`);
    const a = s.address.word;
    const v = evaluate (ma, s, a, xs);
    const result = v.word;
    return result;
}

// k8 is always fixed, never relocatable, and is always in the gh field

// ma is assembler context, s is statement, a is address where the k8
// field will be inserted, field is the name of the field where it
// will be inserted (currently this will always be gh field), and xs
// is the source string specifying the value.

function requireK8 (ma, s, a, field, xs) {
    com.mode.devlog (`requireK8 ${xs}`);
    const v = evaluate (ma, s, a, xs);
    const result = v.word;
    return result;
}

// field should be a register, e.g. R3 or r12.  Return the number (3,
// 12) but if the syntax is invalid, add an error message to the
// statement s and return 0.

function requireReg (ma, s, field) {
    let result = 0
    if ((field.length === 2 || field.length === 3)
        && (field[0] === "R" || field[0] === "r")) {
        let ntext = field.slice(1)
        let n = parseInt (ntext)
        if (n >= 0 &&  n <= 15) {
            result = n
        } else {
            mkErrMsg (ma, s, `register in ${field} must be between 0 and 15`)
        }
    } else {
        mkErrMsg (ma, s, `${field} must be register, e.g. R4 or r14`)
    }
    com.mode.devlog (`requireReg field=${field} result=${result}`);
    return result;
}

//----------------------------------------------------------------------
//  Parser
//----------------------------------------------------------------------

// Parse the source for line i and update the object with the results.
// Each source line is a statement; a statement consists of a sequence
// of fields and whitespace-fields.  Each is optional.  The structure
// is:

// label, whitespace, operation, whitespace, operands, whitespace

function parseAsmLine (ma,i) {
    com.mode.devlog (`parseAsmLine i=${i}`);
    let s = ma.asmStmt[i];
    console.log (`parseAsmLine i=${i}`);
    showAsmStmt(s);

    let p = parseSplitFields.exec(s.srcLine);
//    console.log (`parseAsmLine i=${i} srcLine=<${s.srcLine}>  p=${p}`)
    s.fieldLabel = p[1];
    s.fieldSpacesAfterLabel = p[2];
    s.fieldOperation = p[3];
//    console.log (`parseAsmLine fieldOperation=${s.fieldOperation}`)
    s.fieldSpacesAfterOperation = p[4];
    s.fieldOperands = p[5];
    s.fieldComment = p[6];
    parseLabel (ma,s);
    parseOperation (ma,s);
    s.operands = s.fieldOperands.split(',');
    com.mode.devlog (`ParseAsmLine ${s.lineNumber}`);
    com.mode.devlog (`  fieldLabel = ${s.hasLabel} /${s.fieldLabel}/`);
    com.mode.devlog (`  fieldOperation = /${s.fieldOperation}/`);
    com.mode.devlog (`  operation = ${showOperation(s.operation)}`);
    com.mode.devlog (`  fieldOperands = /${s.fieldOperands}/`);
    com.mode.devlog (`  operands = ${s.operands}`);
    com.mode.devlog (`  fieldComment = /${s.fieldComment}/`);
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
    com.mode.devlog (`parseOperation line ${s.lineNumber} op=<${op}>`);
    if (op !== '') {
	let x = arch.statementSpec.get(op);
	if (x) {
            com.mode.devlog (`parseOperation: found statementSpec ${x}`);
	    s.operation = x;
            if (s.operation.ifmt==arch.iDir
                && s.operation.afmt==arch.aModule) {
                ma.modName = s.fieldLabel;
                com.mode.devlog (`parseOperation module=${ma.modName}`);
            } else if (s.operation.ifmt==arch.iData
                       && s.operation.afmt==arch.aData) {
                s.codeSize = One.copy();
            } else if (s.operation.ifmt==arch.iDir
                       && s.operation.afmt==arch.aReserve) {
                let y = evaluate (ma, s, ma.locationCounter,
                                  s.fieldOperands);
                //                s.reserveAddr = y.word;
                s.reserveSize = y;
                com.mode.devlog (`parse Operation reserveSize=${s.reserveSize}`);
            } else if (s.operation.ifmt==arch.iDir
                       && s.operation.afmt==arch.aOrg) {
                let y = evaluate (ma, s, ma.locationCounter,
                                  s.fieldOperands);
                //                s.orgAddr = y.word;
                s.orgAddr = y
                com.mode.devlog (`parse Operation orgAddr=${s.orgAddr}`);
            } else {
	        s.codeSize = mkConstVal(arch.formatSize(x.ifmt));
            }
	} else {
            s.operation = arch.emptyOperation;
            s.codeSize = Zero;
            mkErrMsg (ma, s, `${op} is not a valid operation`)
	}
    } else {
        s.operation = arch.emptyOperation;
    }
}

//----------------------------------------------------------------------
//  Assembler Pass 1
//----------------------------------------------------------------------

// Pass 1 parses the source, calculates the code size for each
// statement, defines labels, and maintains the location counter.
// Requires that asmSrcLines has been set to the source code, split
// into an array of lines of text.

function asmPass1 (ma) {
    com.mode.devlog('Assembler Pass 1: ' + ma.asmSrcLines.length
                    + ' source lines');
    for (let i = 0; i < ma.asmSrcLines.length; i++) {
        com.mode.devlog (`Pass 1 i=${i} line=<${ma.asmSrcLines[i]}>`);
        console.log (`Pass 1 i=${i} line=<${ma.asmSrcLines[i]}>`);
	ma.asmStmt[i] = mkAsmStmt (i, ma.locationCounter.copy(),
                                   ma.asmSrcLines[i]);
	let s = ma.asmStmt[i];
        let badCharLocs = validateChars (ma.asmSrcLines[i]);
        //        com.mode.devlog (`validateChars: badCharLocs=${badCharLocs}`);
        console.log (`validateChars: badCharLocs=${badCharLocs}`);

        if (badCharLocs.length > 0) {
            mkErrMsg (ma,s,`Invalid character at position ${badCharLocs}`);
            mkErrMsg (ma,s, "See User Guide for list of valid characters");
            mkErrMsg (ma,s, "(Word processors often insert"
                            + " invalid characters)");
        }
        parseAsmLine (ma,i);
        com.mode.devlog (`Pass 1 ${i} /${s.srcLine}/`
                         + ` address=${s.address} codeSize=${s.codeSize}`);
        handleLabel (ma,s);
        updateLocationCounter (ma,s,i);
    }
}

// Define identifier appearing in label field

function handleLabel (ma,s) {
    if (s.hasLabel) {
        com.mode.devlog (`ParseAsmLine label ${s.lineNumber} /${s.fieldLabel}/`);
	if (ma.symbolTable.has(s.fieldLabel)) {
            mkErrMsg (ma, s, s.fieldLabel + ' has already been defined');
        } else if (s.fieldOperation==="module") {
            com.mode.devlog (`Parse line ${s.lineNumber} label: module`);
        } else if (s.fieldOperation==="equ") {
            let v = evaluate (ma, s, ma.locationCounter, s.fieldOperands);
            let i = new Identifier (s.fieldLabel, null, null, v, s.lineNumber+1);
            ma.symbolTable.set (s.fieldLabel, i);
            com.mode.devlog (`Parse line ${s.lineNumber} set ${i.toString()}`);
        } else if (s.fieldOperation==="import") {
            let mod = s.operands[0];
            let extname = s.operands[1];
            let v = ExtVal.copy();
            let i = new Identifier (s.fieldLabel, mod, extname,
                                    v, s.lineNumber+1);
            ma.symbolTable.set (s.fieldLabel, i);
            com.mode.devlog
              (`Label import ${s.lineNumber} locname=${s.fieldLabel}`
               + ` mod=${mod} extname=${extname}`);
        } else {
            let v = ma.locationCounter.copy();
            com.mode.devlog (`def label lc = ${ma.locationCounter.toString()}`);
            com.mode.devlog (`def label v = ${v.toString()}`);
            let i = new Identifier (s.fieldLabel, null, null, v, s.lineNumber+1);
            com.mode.devlog (`Parse line ${s.lineNumber} label ${s.fieldLabel}`
                             + ` set ${i.toString()}`);
            ma.symbolTable.set (s.fieldLabel, i);
        }
    }
}

function updateLocationCounter (ma,s,i) {
    com.mode.devlog (`Pass 1 ${i} @ was ${ma.locationCounter.toString()}`);
    if (s.operation.ifmt==arch.iDir && s.operation.afmt==arch.aOrg) {
        let v = evaluate (ma, s, ma.locationCounter, s.fieldOperands);
        ma.locationCounter = v.copy();
        console.log (`P1 org @${ma.locationCounter.toString()}`);
        com.mode.devlog (`org ${i} ${ma.locationCounter.toString()}`);
    } else if (s.operation.ifmt==arch.iDir
               && s.operation.afmt==arch.aReserve) {
        let v = evaluate (ma, s, ma.locationCounter, s.fieldOperands);
        console.log (`P1 reserve0 @<${ma.locationCounter.toString()}>`);
        console.log (`P1 reservev v=<${v}>`);
        ma.locationCounter.add (v);
        s.locCounterUpdate = ma.locationCounter.copy()
        console.log (`P1 reserve1 @<${ma.locationCounter.toString()}>`);
        com.mode.devlog (`reserve ${i} ${ma.locationCounter.toString()}`);
    } else {
        com.mode.devlog (`Pass1 code codesize=${s.codeSize.toString()}`);
        ma.locationCounter.add (s.codeSize);
        com.mode.devlog (`code ${i} ${ma.locationCounter.toString()}`);
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

//----------------------------------------------------------------------
//  Pass 2
//----------------------------------------------------------------------

// Make a code word from four 4-bit fields

function mkWord (op,d,a,b) {
    let clear = 0x000f;
    return ((op&clear)<<12) | ((d&clear)<<8) | ((a&clear)<<4) | (b&clear);
}

// Make a code word from two 4-bit fields and an 8 bit field (EXP format)

function mkWord448 (x,y,k8) {
    let clear4 = 0x000f;
    let clear8 = 0x00ff;
    return ((x&clear4)<<12) | ((y&clear4)<<8) | (k8&clear8);
}

// Make a code word from a 4-bit field and a 12 bit field

function mkWord412 (k4,k12) {
    let clear4 = 0x000f;
    let clear8 = 0x00ff;
    let clear12 = 0x0fff;
    return ((k4&clear4)<<12) | (k12&clear12)
}

function testWd(op,d,a,b) {
    console.log(arith.wordToHex4(mkWord(op,d,a,b)));
}

// imports are handled in Pass 1 because they define labels; exports
// are handled in Pass2 because they use labels.

function asmPass2 (ma) {
    com.mode.setTrace()
    com.mode.devlog('Assembler Pass 2');
    objectWordBuffer = [];
    relocationAddressBuffer = [];
    ma.objectCode.push (`module   ${ma.modName}`);
    for (let i = 0; i < ma.asmStmt.length; i++) {
	let s = ma.asmStmt[i];
	com.mode.devlog(`Pass2 line ${s.lineNumber} = /${s.srcLine}/`);
        com.mode.devlog (`>>> pass2 operands = ${s.operands}`);
        let op = s.operation;
        com.mode.devlog (`Pass2 op ${s.fieldOperation} ${showOperation(op)}`)
        com.mode.devlog (`Pass2 op ifmt=${op.ifmt.description}`
                         + ` afmt=${op.afmt.description}`
                         + ` pseudo=${op.pseudo}`);

// Directives        
        if (op.ifmt==arch.iDir && op.afmt == arch.aOrg) {
            emitObjectWords (ma);
            const a = s.orgAddr;
            const ahex = arith.wordToHex4 (a.word);
            let stmt = `org      ${ahex}`
            ma.objectCode.push (stmt);
        } else if (op.ifmt==arch.iDir && op.afmt==arch.aReserve) {
            emitObjectWords (ma);
            const size = s.reserveSize;
            //            const xhex = arith.wordToHex4 (ma.locationCounter.word)
            const xhex = arith.wordToHex4 (s.locCounterUpdate.word)
            let stmt = `org      ${xhex}`
            ma.objectCode.push (stmt);

            
            
// RRR-RRR
        } else if (op.ifmt==arch.iRRR && op.afmt==arch.aRRR) {
            com.mode.devlog (`pass2 iRRR/aRRR`);
            requireNoperands (ma, s, 3)
            const d = requireReg(ma,s,s.operands[0]);
            const a = requireReg(ma,s,s.operands[1]);
            const b = requireReg(ma,s,s.operands[2]);
            com.mode.devlog (`RRR noperands = ${s.operands.length}`)
            s.codeWord1 = mkWord (op.opcode[0], d, a, b);
            generateObjectWord (ma, s, s.address.word, s.codeWord1);

	} else if (op.ifmt==arch.iRRR && op.afmt==arch.aRR) {

// RRR-RR            
	    com.mode.devlog (`Pass2 iRRR/aRR`);
            requireNoperands (ma, s, 2)
            const d = 0;
            const a = requireReg(ma,s,s.operands[0]);
            const b = requireReg(ma,s,s.operands[1]);
	    s.codeWord1 = mkWord (op.opcode[0], d, a, b);
            generateObjectWord (ma, s, s.address.word, s.codeWord1);

	} else if (op.ifmt==arch.iRX && op.afmt==arch.aRX) {

// RX-RX
	    com.mode.devlog (`***** Pass2 RX/RX`);
            requireNoperands (ma, s, 2)
            const d = requireReg(ma,s,s.operands[0]);
            const {disp,index} = requireX(ma,s,s.operands[1]);
            com.mode.devlog (`RX/RX disp = /${disp}/ index=${index}`)
            let a = index;
            let b = op.opcode[1];
            let v = evaluate (ma, s, s.address.word+1, disp);
            console.log (`RX/RX v=${v}`)
            if (v.evalRel) {
                console.log ('RX/RX generating relocation')
                generateRelocation (ma, s, s.address.word+1);
            }
            com.mode.devlog (`pass 2 RX/RX a=${a} b=${b} v=${v.toString()}`);
            s.codeWord1 = mkWord (op.opcode[0], d, a, b);
            s.codeWord2 = v.word;
	    generateObjectWord (ma, s, s.address.word, s.codeWord1);
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2);
            handleVal (ma, s, s.address.word+1, disp, v, Field_disp);

        } else if (op.ifmt==arch.iRX && op.afmt==arch.aX) {
// RX-X
            com.mode.devlog (`***** Pass2 RX/X`);
            requireNoperands (ma, s, 1)
            const d = op.pseudo ? op.opcode[2] : 0
            const {disp,index} = requireX (ma, s, s.operands[0])
            com.mode.devlog (`RX/X disp = /${disp}/ index=${index}`)
            let a = index
            let b = op.opcode[1]
            let v = evaluate (ma, s, s.address.word+1, disp);
            console.log (`RX/X v=${v}`)
            if (v.evalRel) {
                console.log ('RX/X generating relocation')
                generateRelocation (ma, s, s.address.word+1);
            }
            com.mode.devlog (`pass 2 RX/X a=${a} b=${b} v=${v.toString()}`)
            s.codeWord1 = mkWord (op.opcode[0], d, a, b);
            s.codeWord2 = v.word;
	    generateObjectWord (ma, s, s.address.word, s.codeWord1);
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2);
            handleVal (ma, s, s.address.word+1, disp, v, Field_disp);

        } else if (op.ifmt==arch.iRX && op.afmt==arch.akX) {
// RX-kX
	    com.mode.devlog (`pass2 RX/kX`);
            requireNoperands (ma, s, 2)
            const k = evaluate (ma, s, s.address.word, s.operands[0])
            const d = k.word;
            const {disp,index} = requireX(ma,s,s.operands[1]);
            let a = index;
            let b = op.opcode[1];
            let v = evaluate (ma, s, s.address.word+1, disp);
            if (v.evalRel) {
                generateRelocation (ma, s, s.address.word+1);
            }
            s.codeWord1 = mkWord (op.opcode[0], d, a, b);
            s.codeWord2 = v.word;
	    generateObjectWord (ma, s, s.address.word, s.codeWord1);
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2);
            handleVal (ma, s, s.address.word, s.operands[0], k, Field_d);
            handleVal (ma, s, s.address.word+1, disp, v, Field_disp);

        } else if (op.ifmt==arch.iEXP && op.afmt==arch.a0) {
// EXP-null (resume)
	    com.mode.devlog (`Pass2 iEXP1/no-operand`)
//            requireNoperands (ma, s, 0) triggers error
            s.codeWord1 = mkWord448(op.opcode[0],0,op.opcode[1])
            s.codeword2 = 0
	    generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

// EXP-R (timeron)
        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aR) {
            com.mode.devlog (`pass2 EXP/R`);
            requireNoperands (ma, s, 1)
            const d = requireReg(ma,s,s.operands[0]);
            s.codeWord1 = mkWord448(op.opcode[0],d,op.opcode[1])
            s.codeWord2 = 0
	    generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aK) {
// EXP-K (brf, brb)
            console.log (`Pass2 EXP/K`)
            requireNoperands (ma, s, 1)
            const dest = evaluate (ma, s, s.address.word, s.operands[0])
            const offset = findOffset (s.address, dest)
            console.log (`pc relative offset = ${offset}`)
            s.codeWord1 = mkWord448(op.opcode[0],0,op.opcode[1])
            s.codeWord2 = offset
	    generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRR) {
// EXP-RR (invw pseudo)
	    com.mode.devlog (`pass2 EXP-RR pseudo`);
            requireNoperands (ma, s, 2)
            const addr = s.address.word;
            const d = requireReg (ma, s, s.operands[0]);
            const e = requireReg (ma, s, s.operands[1]);
            const f = 0
            const g = 0;
            const h = op.pseudo ? op.opcode[2] : 0
	    s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
            s.codeWord2 = mkWord (e, f, g, h);
            generateObjectWord (ma, s, s.address.word, s.codeWord1);
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2);

        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aRRR) {
// EXP-RRR (push, pop, top; if pseudo then andw, orw, xorw)
            com.mode.devlog ('Pass2 EXP/RRR');
            requireNoperands (ma, s, 3)
            const addr = s.address.word;
            const d = requireReg (ma, s, s.operands[0]);
            const e = requireReg (ma, s, s.operands[1]);
            const f = requireReg (ma, s, s.operands[2]);
            const g = 0;
            const h = op.pseudo ? op.opcode[2] : 0
	    s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
            s.codeWord2 = mkWord (e, f, g, h);
            generateObjectWord (ma, s, s.address.word, s.codeWord1);
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2);
            
        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aRK) {
// EXP-RK (brfz, brbz, brfnz, brbnz)
            console.log (`Pass2 EXP/RK pcr`)
            requireNoperands (ma, s, 2)
            const d = requireReg (ma, s, s.operands[0])
            const dest = evaluate (ma, s, s.address.word, s.operands[1])
            const offset = findOffset (s.address, dest)
            console.log (`pc relative offset = ${offset}`)
            s.codeWord1 = mkWord448(op.opcode[0],d,op.opcode[1])
            s.codeWord2 = offset
	    generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aRkk && op.pseudo) {
            // EXP-Rkk aRkk pseudo: invb Similar to aRkkkk but with
            // operands[3]=0 unspecified and operands[4] = opcode[2]
            console.log (`Pass2 EXP/Rkk pseudo`)
            requireNoperands (ma, s, 3)
            const ab = op.opcode[1]
            const d = requireReg(ma,s,s.operands[0])
            const e = requireK4 (ma, s, Field_e, s.operands[1])
            const f = requireK4 (ma, s, Field_f, s.operands[2])
            const g = 0 // second arg is ignored
            const h = op.opcode[2] // function code
	    s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord (e, f, g, h)
	    generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aRkK && !op.pseudo) {
// EXP-RkK:  brfc0, brbc0, brfc1, brbc1
            console.log (`Pass2 EXP/RkK pcr`)
            requireNoperands (ma, s, 3)
            const d = requireReg (ma, s, s.operands[0])
            const e = requireK4 (ma, s, Field_e, s.operands[1])
            const dest = evaluate (ma, s, s.address.word, s.operands[2])
            const offset = findOffset (s.address, dest)
            console.log (`pc relative offset = ${offset}`)
            s.codeWord1 = mkWord448(op.opcode[0],d,op.opcode[1])
            s.codeWord2 = mkWord412 (e, offset)
	    generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)
            
	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRRk) {
// EXP-RRk (shiftl, shiftr)
            com.mode.devlog ("*********EXP-RRk **********")
	    com.mode.devlog (`pass2 aRRk`)
            requireNoperands (ma, s, 3)
            const ab = op.opcode[1]
            const d = requireReg (ma, s, s.operands[0])
            const e = requireReg (ma, s, s.operands[1])
            const kv = evaluate (ma, s, s.address.word, s.operands[2])
            const f = 0
            const gh = kv.word
            com.mode.devlog (`Pass 2 RRk d=${d} ab=${ab} e=${e} f=${f}`)
            s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord448 (e,f,gh)
            generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aRRRk) {
// EXP-RRRk (logicw)
            com.mode.devlog (`pass2 aRRRk`);
            requireNoperands (ma, s, 4)
            const ab = op.opcode[1]
            const d = requireReg (ma, s, s.operands[0])
            const e = requireReg (ma, s, s.operands[1])
            const f = requireReg (ma, s, s.operands[2])
            const g = 0
            //            const h = s.operands[3]
            const hexp = evaluate (ma, s, s.address.word, s.operands[3])
            const h = hexp.word
            s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord (e,f,g,h)
            generateObjectWord (ma, s, s.address.word, s.codeWord1)
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRkkkk) {
// EXP-Rkkkk (logicr) logicr R3,6,8,2,xor
            com.mode.devlog ('Pass2 EXP/Rkkkk (logicr)')
            requireNoperands (ma, s, 5)
            const ab = op.opcode[1]
            const d = requireReg(ma,s,s.operands[0])
            const e = requireK4 (ma, s, Field_e, s.operands[1])
            const f = requireK4 (ma, s, Field_f, s.operands[2])
            const g = requireK4 (ma, s, Field_g, s.operands[3])
            const h = requireK4 (ma, s, Field_h, s.operands[4])
	    s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord (e, f, g, h)
            generateObjectWord (ma, s, s.address.word, s.codeWord1)
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRkRkk) {
// EXP-RkRkk (logicb) logicb R3,4,R9,8,xor
            com.mode.devlog ('Pass2 EXP/RkRkk')
            requireNoperands (ma, s, 5)
            const ab = op.opcode[1]
            const d = requireReg (ma, s, s.operands[0])          // op1
            const e = requireK4  (ma, s, Field_e, s.operands[1]) // idx1
            const f = requireReg (ma, s, s.operands[2])          // op2
            const g = requireK4  (ma, s, Field_g, s.operands[3]) // idx2
            const h = requireK4  (ma, s, Field_h, s.operands[4]) // fcn
	    s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord (e, f, g, h)
            generateObjectWord (ma, s, s.address.word, s.codeWord1)
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRkRk) {
// EXP-RkRk (moveb) moveb R3,4,R5,13
            com.mode.devlog ('Pass2 EXP/RkRk')
            requireNoperands (ma, s, 4)
            const ab = op.opcode[1]
            const d = requireReg (ma, s, s.operands[0])          // op1
            const e = requireK4  (ma, s, Field_e, s.operands[1]) // idx1
            const f = requireReg (ma, s, s.operands[2])          // op2
            const g = requireK4  (ma, s, Field_g, s.operands[3]) // idx2
            const h = 0
	    s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord (e, f, g, h)
            generateObjectWord (ma, s, s.address.word, s.codeWord1)
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2)

	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRRX) {
// EXP-RRX (save, restor)
            console.log ("Pass 2 EXP/RRX")
            requireNoperands (ma, s, 3)
            const ab = op.opcode[1]
            const d = requireReg (ma, s, s.operands[0])
            const e = requireReg (ma, s, s.operands[1])
            const {disp,index} = requireX (ma, s, s.operands[2])
            const f = index
            const gh = disp
            console.log (`sav/rest gh=${gh} f=${f}`)
	    s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord448 (e, f, gh)
            generateObjectWord (ma, s, s.address.word, s.codeWord1)
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2)
            
	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRkkRk) {
// EXP-RkkRk (extrc, extrci)
	    console.log (`pass2 iEXP/aRkkRk`);
            requireNoperands (ma, s, 5)
            const ab = op.opcode[1]
            const d = requireReg (ma, s, s.operands[0])
            const f = requireK4 (ma, s, Field_f, s.operands[1])
            const g = requireK4 (ma, s, Field_g, s.operands[2])
            const e = requireReg (ma, s, s.operands[3])
            const h = requireK4 (ma, s, Field_h, s.operands[4])
            console.log (`extract d=${d} e=${e} f=${f} g=${g} h=${h}`)
	    s.codeWord1 = mkWord448 (op.opcode[0], d, ab)
            s.codeWord2 = mkWord (e, f, g, h)
            generateObjectWord (ma, s, s.address.word, s.codeWord1)
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2)


	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRkkk & op.pseudo) {
// EXP-Rkkk  (andb pseudo)
            com.mode.devlog ('Pass2 EXP-Rkkk');
            requireNoperands (ma, s, 4)
            const addr = s.address.word;
            const d = requireReg (ma, s, s.operands[0]);
            const e = requireK4 (ma, s, Field_e, s.operands[1]);
            const f = requireK4 (ma, s, Field_f, s.operands[2]);
            const g = requireK4 (ma, s, Field_g, s.operands[3]);
            const h = op.opcode[2];
	    s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
            s.codeWord2 = mkWord (e, f, g, h);
            generateObjectWord (ma, s, s.address.word, s.codeWord1);
            generateObjectWord (ma, s, s.address.word+1, s.codeWord2);

            
	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRC) {
// EXP-RC  (getctl, putctl)
	    com.mode.devlog ("pass2 aRC");
            requireNoperands (ma, s, 2) // ???????????
            let x = rcParser.exec  (s.fieldOperands);
            if (x) {
                const {1:e, 2:ctlRegName} = x;
                let ctlRegIdx = findCtlIdx (ma,s,ctlRegName);
	        s.codeWord1 = mkWord448 (14,0,op.opcode[1]);
	        s.codeWord2 = mkWord (e, ctlRegIdx, 0, 0);
	        generateObjectWord (ma, s, s.address.word, s.codeWord1);
	        generateObjectWord (ma, s, s.address.word+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RC operands`);
            }

        } else if (op.ifmt==arch.iEXP && op.afmt==arch.aRRRk) {
            com.mode.devlog (`pass2 aRRRk`);
            requireNoperands (ma, s, 4)
            const d = requireReg(ma,s,s.operands[0]);
            const e = requireReg(ma,s,s.operands[1]);
            const f = requireReg(ma,s,s.operands[2]);
            const kv = evaluate (ma, s, s.address.word, s.operands[3]);
            const g = 0;
            const h = kv.word;
	    s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
	    s.codeWord2 = mkWord (e, f, g, h);
	    generateObjectWord (ma, s, s.address.word, s.codeWord1);
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2);

// EXP-RRX save, restore
	} else if (op.ifmt==arch.iEXP && op.afmt==arch.aRRX) {
	    com.mode.devlog (`pass2 EXP/RRX`);
	    com.mode.devlog (`pass2 EXP-RRX`);
            requireNoperands (ma, s, 3)
            const x = rrxParser.exec (s.fieldOperands);
            const d = requireReg(ma,s,s.operands[0]);
            const e = requireReg(ma,s,s.operands[1]);
            const {disp,index} = requireX (ma, s, s.operands[2]);
            const f = index;
            const gh = requireK8 (ma, s, s.address.word+1, arch.Field_gh, disp);
	    s.codeWord1 = mkWord448 (op.opcode[0], d, op.opcode[1]);
            s.codeWord2 = mkWord448 (e, f, gh);
	    generateObjectWord (ma, s, s.address.word, s.codeWord1);
	    generateObjectWord (ma, s, s.address.word+1, s.codeWord2);

// EXP-kX            
	} else if (op.ifmt==arch.iEXP && op.afmt==arch.akX) {
	    com.mode.devlog (`pass2 EXP/kX`);
            requireNoperands (ma, s, 2)
            let x = kxParser.exec (s.fieldOperands);
            if (x) {
	        s.codeWord1 = mkWord(op.opcode[0],s.d,s.a,op.opcode[1]);
                let v = evaluate(ma,s,s.address.word+1,s.field_disp);
                s.codeWord2 = v.value;
                if (v.evalRel) {
                    com.mode.devlog (`relocatable displacement`);
                    generateRelocation (ma, s, s.address.word+1);
                }
	        generateObjectWord (ma, s, s.address.word, s.codeWord1);
	        generateObjectWord (ma, s, s.address.word+1, s.codeWord2);
            } else {
                mkErrMsg (ma, s, `ERROR operation requires RX operands`);
            }

// Data-Data            
        } else if (op.ifmt==arch.iData && op.afmt==arch.aData) {
            com.mode.devlog (`Pass2 ${s.lineNumber} data`);
            let v = evaluate(ma,s,s.address,s.fieldOperands);
            com.mode.devlog (`data v = ${v.toString()}`);
//            s.codeWord1 = v.getval();
            s.codeWord1 = v.word;
	    generateObjectWord (ma, s, s.address.word, s.codeWord1);
            //            if (v.getIsRelocatable()) {
            if (v.movability==Relocatable) {
                com.mode.devlog (`relocatable data`);
                generateRelocation (ma, s, s.address.word);
            }

// Dir-Export            
        } else if (op.ifmt==arch.iDir && op.afmt==arch.aExport) {
            com.mode.devlog ('pass2 export statement')
            const x = identParser.exec (s.fieldOperands);
            if (x) {
                com.mode.devlog (`export identarg=${s.identarg}`);
                const {0:ident} = x;
                ma.exports.push(ident);
            } else {
                mkErrMsg (ma, s, `ERROR export requires identifier operand`);
            }

// Unknown format            
	} else {
	    com.mode.devlog('pass2 other, noOperation');
	}
        
	s.listingLinePlain =  (s.lineNumber+1).toString().padStart(4,' ')
	    + ' ' + arith.wordToHex4(s.address.word)
	    + ' ' + (s.codeSize.word>0 ? arith.wordToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize.word>1 ? arith.wordToHex4(s.codeWord2) : '    ')
            + ' '
	    + s.fieldLabel
	    + s.fieldSpacesAfterLabel
	    + s.fieldOperation
	    + s.fieldSpacesAfterOperation
	    + s.fieldOperands
	    + fixHtmlSymbols (s.fieldComment);
	s.listingLineHighlightedFields =
            (s.lineNumber+1).toString().padStart(4,' ')
	    + ' ' + arith.wordToHex4(s.address.word)
	    + ' ' + (s.codeSize.word>0 ? arith.wordToHex4(s.codeWord1) : '    ')
	    + ' ' + (s.codeSize.word>1 ? arith.wordToHex4(s.codeWord2) : '    ')
            + ' '
    	    + com.highlightField (s.fieldLabel, "FIELDLABEL")
	    + s.fieldSpacesAfterLabel
	    + com.highlightField (s.fieldOperation, "FIELDOPERATION")
	    + s.fieldSpacesAfterOperation
	    + com.highlightField (s.fieldOperands, "FIELDOPERAND")
	    + com.highlightField (fixHtmlSymbols(s.fieldComment),
                                  "FIELDCOMMENT") ;
        ma.metadata.pushSrc (s.listingLinePlain,
                             s.listingLinePlain,
                             s.listingLineHighlightedFields);
	for (let i = 0; i < s.errors.length; i++) {
            ma.metadata.pushSrc
              ('Error: ' + s.errors[i],
               com.highlightField ('Error: ' + s.errors[i],'ERR'),
               com.highlightField ('Error: ' + s.errors[i],'ERR'));
	}
    }
    emitObjectWords (ma);
    emitRelocations (ma);
    emitExports (ma);
    emitImports (ma);
    ma.objectCode.push ("");
    ma.objectText = ma.objectCode.join("\n");
    com.mode.clearTrace()
}

// handleVal: Generate relocation or import if necessary

function handleVal (ma, s, a, vsrc, v, field) {
    com.mode.devlog (`handleVal ${a} /${vsrc}/ ${field.description}`);
    if (v.origin==Local && v.movability==Relocatable) {
        generateRelocation (ma, s, a);
    } else if (v.origin==External) {
        let sym = ma.symbolTable.get(vsrc);
        if (sym) {
            let modstr = sym.mod;
            let extname = sym.extname;
            let astr = arith.wordToHex4(a);
            let fstr = field.description
            let x = `import ${modstr},${extname},${astr},${fstr}`;
            ma.imports.push(x);
            com.mode.devlog (`handleVal generate ${x}`);
        } else { // should be impossible: internal error
            mkErrMsg (`external symbol ${vsrc} undefined`);
            com.mode.devlog (`external symbol ${vsrc} undefined - impossible`);
        }
    }
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
    //    ma.asArrMap[a] = s.lineNumber; // ASMAP
    ma.metadata.addMapping (a, s.lineNumber);
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
    com.mode.devlog ("emitImports");
    for (let x of ma.imports) {
        ma.objectCode.push(x);
    }
}

function emitExports (ma) {
    let x, y, sym, w, r;
    com.mode.devlog ('emitExports' + ma.exports);
    while (ma.exports.length > 0) {
        x = ma.exports.splice(0,1);
        y = x[0];
        com.mode.devlog (`emit exports looking up  x=<${x}> y=${y}`);
        com.mode.devlog (ma.symbolTable);
        sym = ma.symbolTable.get(y);
        if (sym) {
            r = sym.value.movability==Relocatable ? "relocatable" : "fixed";
            w = arith.wordToHex4(sym.value.word);
            com.mode.devlog (`emit exports y=${y} r=${r} w=${w}`);
            ma.objectCode.push (`export   ${y},${w},${r}`);
        } else {
//    mkErrMsg (ma, null, `export identifier ${y} is undefined`);
            console.log (`export identifier ${y} is undefined`)  // use mkErrMsg
        }
    }
}

function showOperation (op) {
    if (op) {
    return `ifmt=${op.ifmt.description} afmt=${op.afmt.description}`
            + `opcode=${op.opcode} pseudo=${op.pseudo}`;
    } else {
        return "unknown op"
    }
}

//-------------------------------------------------------------------------
// Metadata
//-------------------------------------------------------------------------

// The emulator tries to display the source code line corresponding to
// the instruction currently executing.  The information it needs to
// do this is called Metadata.  This is optional: the emulator needs
// only the object code to run a machine language program.  If the
// optional Metadata is present, the emulator can also show the source
// code corresponding to the current and next instruction.

const eltsPerLineLimit = 4; // how many numbers in the mapping per line

export class Metadata {
    constructor () {
        this.clear ();
    }
    clear () {
        this.pairs = [];   // list of (a,i) pairs for serializing
        this.mapArr = [];  // mapArr[a] = i
        this.listingText = [];
        this.listingPlain = [];   // source lines
        this.listingDec = []; // src lines decorated with html span elements
        this.mdText = null;
        this.adrOffset = 0;
        this.srcOffset = 0; // convert a->i to a->i+srcLineOffset
    }
    addPairs (ps) {
        for (let p of ps) {
            this.mapArr [p.address] = p.index
            this.pairs.push (p)
        }
    }
    translateMap (adrOffset, srcOffset) {
        this.adrOffset = adrOffset
        this.srcOffset = srcOffset
        let xs = []
        this.mapArr = []
        for (const x of this.pairs) {
            const p = {address: x.address + adrOffset,
                       index: x.index + srcOffset}
            xs.push (p)
            this.mapArr [x.address + adrOffset] = x.index + srcOffset
        }
        this.pairs = xs
    }
    addMappingSrc (a, i, srcText, srcPlain, srcDec) { // add a->i plus src
        const p = {address: a, index: i}
        this.pairs.push(p)
        this.mapArr[a] = i;
        this.listingText[i] = srcText;
        this.listingPlain[i] = srcPlain;
        this.listingDec[i] = srcDec;
    }
    addMapping (a, i) { // add new mapping a->i
        const p = {address: a, index: i}
        this.pairs.push (p)
        this.mapArr[a] = i;
    }
    pushSrc (srcText, srcPlain, srcDec) { // add src line in three forms
        this.listingText.push (srcText);
        this.listingPlain.push (srcPlain);
        this.listingDec.push (srcDec);
    }
    unshiftSrc (srcText, srcPlain, srcDec) { // add src line at start
        this.listingText.unshift (srcText);
        this.listingPlain.unshift (srcPlain);
        this.listingDec.unshift (srcDec);
    }
    addSrc (i, srcText, srcPlain, srcDec) { // add src line in three forms
        this.listingText[i] = srcText;
        this.listingPlain[i] = srcPlain;
        this.listingDec[i] = srcDec;
    }
    getSrcIdx (a) { // find source line index corresponding to address a
        const i = this.mapArr[a];
        return i ? i : 0;
    }
    getSrcText (a) { // return text source line corresponding to address a
        const x = this.listingText[this.getSrcIdx(a)];
        return x ? x : `no text src for ${a}`
    }
    getSrcPlain (a) { // return plain source line corresponding to address a
        const x = this.listingPlain[this.getSrcIdx(a)];
        return x ? x : `no plain src for ${a}`
    }
    getSrcDec (a) { // return decorated src line corresponding to address a
        const x = this.listingDec[this.getSrcIdx(a)];
        return x ? x : `no decorated src for ${a}`
    }
    getMdText () {
        if (!this.mdText) { this.mdText = this.toText () }
        return this.mdText
    }
    addSrcLines (xs) {
        for (let i = 0; i < xs.length; i += 3) {
            this.listingText.push (xs[i]);
            this.listingPlain.push (xs[i+1]);
            this.listingDec.push (xs[i+2]);
        }
    }
    setMdText (xs) {
        this.mdText = xs;
    }
    fromText (x) { // parse text and populate the object
        this.clear ();
        this.mdText = x;
        const xs = x.split ("\n");
        let ns = [];
        let i = 0;
        while (i < xs.length && xs[i].substring(0,6) != "source" ) {
            let ys = xs[i].split(",");
            ns = ns.concat (ys.map ((q) => parseInt(q)));
            i++;
        }
        let j = 0;
        while (j < ns.length) {
            let a = ns[j] ? ns[j] : 0;
            let idx = ns[j+1] ? ns[j+1] : 0;
            this.addMapping (a, idx);
            j += 2;
        }
        i++; // skip "source"
        j = 0;
        while (i < xs.length) {
            if (xs[i] != "") {
                this.listingText[j]  = xs[i] ? xs[i] : "";
                this.listingPlain[j] = xs[i+1] ? xs[i+1] : "";
                this.listingDec[j]   = xs[i+2] ? xs[i+2] : "";
                j += 1;
                i += 3;
            } else {
                i++; //skip empty line
            }
        }
    }
    mapToTexts () {  // convert map to list of lines of text
        let xs = []; // flatten the pairs
        for (const p of this.pairs) {
            xs.push (p.address, p.index);
        }
        let ys = []; // list of length-limited lists
        while (xs.length > 0) {
            ys.push (xs.splice(0,eltsPerLineLimit))
        }
        let zs = [] // list of strings showing the map
        for (const y of ys) {
            zs.push (y.toString())
        }
        return zs
    }
    getSrcLines () {
        let xs = [];
        for (let i = 0; i < this.listingPlain.length; i++) {
            xs.push (this.listingText[i]);
            xs.push (this.listingPlain[i]);
            xs.push (this.listingDec[i]);
        }
        return xs;
    }
    getPlainLines () {
        let xs = [];
        for (let i = 0; i < this.listingPlain.length; i++) {
            xs.push (this.listingPlain[i]);
        }
        return xs;
    }
    toText () { // convert contents of object to text
        let xs = this.mapToTexts ()
        xs.push ("source")
        xs = xs.concat (this.getSrcLines ())
        return xs.join ("\n")
    }
}
