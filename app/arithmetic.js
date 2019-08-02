// Sigma16   Copyright (c) 2019 by John T. O'Donnell

//--------------------------------------------------------------------------
// Words, binary numbers, and two's complement integers

// Sigma16 uses 16-bit words.  A word is represented as a JavaScript
// integer whose value is the binary interpretation of the word.  Thus
// a word w must satisfy 0 <= w < 2^16.

const const8000  = 32768;    // 2^15
const constffff  = 65535     // 2^16 - 1
const const10000 = 65536;    // 2^16

// A JavaScript variable has a large range, but a JavaScript variable
// used to represent a Sigma16 binary word or two's complement integer
// must be an integer (no fractional part like 3.5) and within a
// restricted range.

// Binary number          0 <= x < 2^16          0, 1, ...,          65535
// Two's complemen t  -2^15 <= x < 2^15 - 1    -32768, ...,  0, ..., 32767

const minBin = 0;
const maxBin = 65535;
const minTC  = -32768;
const maxTC  = 32767;

const wordTrue = 1;
const wordFalse = 0;

// Determine whether a JavaScript number is a valid Sigma16 word
// (which is represented using binary).  If not, print an error
// message and treat the number as 0.

function validateWord (x) {
    if (x < minBin || x > maxBin) {
	console.log (`validateWord: ${x} is not a valid word (out of range)`);
	return 0;
    } else {
	return x;
    }
}

// Determine whether a JavaScript number is a valid Sigma16 integer
// (which is represented using two's complement).  If not, print an
// error message and treat the number as 0.

function validateInt (x) {
    if (x < minTC || x > maxTC) {
	console.log (`validateInt: ${x} is not a valid int (out of range)`);
	return 0;
    } else {
	return x;
    }
}

//---------------------------------------------------------------------------
// Converting between binary words and two's complement integers

// Natural numbers are represented in binary.  The binary value of a
// word w is just s (i.e. the value of the JavaScript int).

// Integers are represented in two's complement.  The two's complement
// value of a word w is returned by wordToInt (w)

// Invariants
//   intToWord (wordToInt (x)) = x
//   wordToInt (intToWord (x)) = x

function wordToInt (w) {
    let x = validateWord (w);
    return x < const8000 ? x : x - const10000;
}

// An Int is represented using two's complement.  return [w, ovfl]
// where w is a word representing the two's complement int, and ovfl
// is a boolean.  If ovfl is true then x is not representable in a 16
// bit word

function intToWord (x) {
    let y = validateInt (x);
    return y < 0 ? y + const10000 : y;
}

// ??????????? deprecated

function intToTc (x) {
    if (0 <= x && x < 32768) {
	return x
    } else if (-32768 <= x & x < 0) {
	return wordInvert(x) + 1
    } else { return NAN }
}

//---------------------------------------------------------------------------
//  Operating on fields of a word

// Return the 4-bit fields of a word

function splitWord (x) {
    let y = validateWord (x);
    let s = y & 0x000f;
    y = y >>> 4;
    let r = y & 0x000f;
    y = y >>> 4;
    let q = y & 0x000f;
    y = y >>> 4;
    let p = y & 0x000f;
    return [p,q,r,s];
}

//---------------------------------------------------------------------------
// Operating on individual bits in a word

// Return a word where bit i is 1
function setBit(i) { return 1 << i }

// return bit i in word x, where the rightmost bit has index 0
function extractBit (x, i) {
    return (x >>> i) & 0x00000001;
}

// Check where this is used ??????????????

var intToBit = function (x) {
    if (x < 0 || x > 1)
    {console.log('intToBit invalid int: ' + x);
     return('#');
    }
    return x===0 ? '0' : '1';
}

//----------------------------------------------------------------------
// Hexadecimal notation

// The assembly language allows hex numbers to be written with either
// lower case a-f or upper case A-F.  When it outputs hex numbers, the
// system always uses the lower case form.  There are conversion
// functions between hex strings (which must be four characters
// limited to valid hex digits) and words.

// Invariants.  For valid x and str:
//   hex4ToWord (wordToHex4 (x)) = x       e.g. hex4ToWord (wordToHex4 (52309))
//   wordToHex4 (hex4ToWord (str)) = str   e.g. wordToHex4 (hex4ToWord ("5c7e"))

// Character code constants for useful hex characters
const charCode0 = '0'.charCodeAt(0);
const charCode9 = '9'.charCodeAt(0);
const charCodea = 'a'.charCodeAt(0);
const charCodef = 'f'.charCodeAt(0);
const charCodeA = 'A'.charCodeAt(0);
const charCodeF = 'F'.charCodeAt(0);
const hexDigit =
      ['0','1','2','3','4','5','6','7','8','9','a','b','c','d','e','f'];

// Return a string giving the hex representation of a word

var wordToHex4 = function (x) {
    let [p,q,r,s] = splitWord (x);
    return hexDigit[p] + hexDigit[q] + hexDigit[r] + hexDigit[s];
}

// Convert a string with four hex digits to a word.  If the string
// isn't a valid hex number, thhe result is NaN.  To be valid, the
// string must contain exactly four characters, each a valid hex
// digit.  There is no prefix syntax: 3a8b is ok but not $3a8b or
// 0x3a8b.  The assembly language uses a $ to indicate that a number
// is hex, but the $ is not part of the hex number.

function hex4ToWord (h) {
//    console.log("hex4ToWord " + h.length);
    if (h.length != 4) return NaN;
    return (4096 * hexCharToInt(h[0])
  	    +  256  * hexCharToInt(h[1])
    	    +   16  * hexCharToInt(h[2])
       	    +         hexCharToInt(h[3]));
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

//---------------------------------------------------------------------------
// Bitwise logic on words

// Invert each bit in a word

function wordInvert (x) {
    return x ^ 0x0000ffff
}


//---------------------------------------------------------------------------
// Addition and subtraction

// Add two binary numbers.  If there is a carry out, that is
// discarded: instead of overflow, the result wraps around.  For
// example, binAdd (65535, 7) returns 6.  This is used to calculate
// effective addresses and to increment the pc register.

function binAdd (x, y) {
    let r = validateWord (x) + validateWord (y);
    return r & 0x0000ffff;
}

// Arithmetic for the add instruction: add two integers.  There is no
// carry input bit, so the condition code is not needed as an argument
// (hence this uses rrr2 rather than rrr3.)  Return a primary value
// (the two's complement sum) and a secondary result (the new value of
// the condition code).  The condition code result contains the carry
// out and indications of binary overflow and integer overflow.

function add (x,y) {
    let sum = x + y;
    let sumw = sum & 0x0000ffff;
    let carryOut = extractBit (sum,16);
    let msb = extractBit (sum, 15);
    let binOverflow = carryOut;
    let tcOverflow = carryOut==msb ? 0 : 1;
    let ccNew = (binOverflow == 1 ? ccV : 0)
 	| (tcOverflow == 1 ? ccv : 0)
        | (carryOut == 1 ? ccC : 0);
    let result = [sumw, ccNew];
    console.log('rrr2_add');
    console.log ('  x = ' + wordToHex4(x) + ' ' + x);
    console.log ('  y = ' + wordToHex4(y) + ' ' + y);
    console.log ('  sum = ' + wordToHex4(sum) + ' ' + sum);
    console.log ('  sumw = ' + wordToHex4(sumw) + ' ' + sumw);
    console.log ('  msb = ' + msb);
    console.log ('  carryOut = ' + carryOut);
    console.log ('  binOverflow = ' + binOverflow);
    console.log ('  tcOverflow = ' + tcOverflow);
    console.log ('  ccNew = ' + wordToHex4(ccNew));
    console.log ('  result = ' + result);
    return result;
}

function sub (x,y) {
}

function mul (x,y) {
}

function div (x,y) {
}

function sub (x,y) {
}


//---------------------------------------------------------------------------
// Comparison

function cmp (x,y) {
}

// Arithmetic for the cmplt instruction.  The primary result is a
// boolean represented as a word, which goes into the destination
// register.  The secondary is discarded: this instruction doesn't
// change the condition code.  To do that, use the cmp instruction.

function cmplt (x,y) {
    let xi = wordToInt (x);
    let yi = wordToInt (y);
    let primary = xi < yi ? wordTrue : wordFalse;
    let result = [primary, 0];
    return result;
}

function cmpeq (x,y) {
    let xi = wordToInt (x);
    let yi = wordToInt (y);
    let primary = xi === yi ? wordTrue : wordFalse;
    let result = [primary, 0];
    return result;
}

function cmpgt (x,y) {
    let xi = wordToInt (x);
    let yi = wordToInt (y);
    let primary = xi > yi ? wordTrue : wordFalse;
    let result = [primary, 0];
    return result;
}


function inv (x) {
}

function and (x,y) {
}

function or (x,y) {
}

function xor (x,y) {
}

function addc (x,y,cc) {
}

function trap (x,y,z) {
}


//--------------------------------------------------------------------------
// Condition codes

// Bits are numbered from right to left, starting with 0.  Thus the
// least significant bit has index 0, and the most significant bit has
// index 15.

// Define a word for each condition that is representable in the
// condition code.  The arithmetic operations may or several of these
// together to produce the final condition code.

const ccG = setBit(0);
const ccg = setBit(1);
const ccE = setBit(2);
const ccl = setBit(3);
const ccL = setBit(4);
const ccV = setBit(5);
const ccv = setBit(6);
const ccC = setBit(7);

//------------------------------------------------------------------------------
// Experiments.... deprecated

// experiment, try a function defined in main.  The idea is that a
// function might be defined in main for the electron standalone
// version, but there could be a fall back definition in the browser
// version

// result of clicking Editor: try def in main
//   in browser: tryMainFcn NO
//   in electron standalone app:  

function tryMainFcn () {
    if (typeof testDefInMain === 'function') {
	document.getElementById('EditorTextArea').value =
	    "tryMainFcn: testDefInMain exists " + testDefInMain (41);
    } else {
	document.getElementById('EditorTextArea').value = "tryMainFcn NO";
    }
}
