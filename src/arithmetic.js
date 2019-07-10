// Sigma16   Copyright (c) John T. O'Donnell, 2019

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

// Return a word where bit i is 1
function setBit(i) { return 1 << i }

// return bit i in word x, where the rightmost bit has index 0
function extractBit (x, i) {
    return (x >>> i) & 0x00000001;
}

function wordInvert (x) {
    return x ^ 0x0000ffff
}

function intToTc (x) {
    if (0 <= x && x < 32768) {
	return x
    } else if (-32768 <= x & x < 0) {
	return wordInvert(x) + 1
    } else { return NAN }
}

// Arithmetic for the add instruction
// Returns primary value (sum) and secondary (condition code)
// cc is the condition code, ignored by wordAdd
// x and y should be 16-bi words

function rrr_add (x,y,cc) {
    console.log('rrr_add');
    let sum = x + y;
    let sum16 = sum & 0x0000ffff;
    let carryOut = extractBit (sum,16);
    let msb = extractBit (sum, 15);
    let binOverflow = carryOut;
    let tcOverflow = carryOut==msb ? 0 : 1;
    let ccNew = (binOverflow == 1 ? ccV : 0)
 	| (tcOverflow == 1 ? ccv : 0)
        | (carryOut == 1 ? ccC : 0);
    console.log ('addWords');
    console.log ('  x = ' + intToHex4(x) + ' ' + x);
    console.log ('  y = ' + intToHex4(y) + ' ' + y);
    console.log ('  sum = ' + intToHex4(sum) + ' ' + sum);
    console.log ('  sum16 = ' + intToHex4(sum16) + ' ' + sum16);
    console.log ('  msb = ' + msb);
    console.log ('  carryOut = ' + carryOut);
    console.log ('  binOverflow = ' + binOverflow);
    console.log ('  tcOverflow = ' + tcOverflow);
    console.log ('  cc new = ' + intToHex4(ccNew));
    let result = [ sum16, ccNew ];
    console.log ('  rrr_add result = ' + result);
    return result;
}

// Hexadecimal, registers, memory

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
